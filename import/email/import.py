import imaplib
import datetime
import time
from typing import Dict,List,Union,Optional,Any
import fnmatch
import re
import os
import json
import settings
import traceback
from logger import ImportLogger
from email_utils import add_file_timestamp, parse_message, check_address, imap_date, MailSender, parse_address_list

ACCOUNT = settings.ACCOUNT
OUTPUT_PATH = settings.OUTPUT_PATH
LOG_DB_PATH = settings.LOG_DB_PATH
SOURCES = settings.SOURCES
SENDING_ACCOUNT = settings.SENDING_ACCOUNT
IGNORE_RECIPIENTS = getattr(settings, 'IGNORE_RECIPIENTS', [])
DELAY_WEEKS = 12
if hasattr(settings, 'DELAY_WEEKS'):
    DELAY_WEEKS = settings.DELAY_WEEKS

sending_only_from = None
if hasattr(settings, 'SENDING_ONLY_FROM'):
    sending_only_from = datetime.datetime.fromisoformat(settings.SENDING_ONLY_FROM)

class SourceDef:

    def __init__(self, conf):
        if not isinstance(conf['from'], list):
            raise ValueError("from is not a list")
        v = conf['from']
        v = [x.lower() for x in  v]
        self.senders = conf['from']
        self.dir = conf['dir']
        self.path = OUTPUT_PATH + '/' + self.dir
        self.files = conf['files']
        if not os.path.exists(self.path):
            os.mkdir(self.path)

    def is_from(self, sender):
        """
            Check if sender is on accepted list for this sender
        """
        return check_address(sender, self.senders)
    
    def is_expected_file(self, name):
        for file_def in self.files:
            file, file_type = file_def
            name = name.lower()
            file = file.lower()
            if fnmatch.fnmatch(name, file):
                return (True, file_type)
        return (False, None)
class HandledMessage:
    def __init__(self, message_id):
        self.source: Optional[SourceDef] = None
        self.message_id = message_id
        self.senders = None
        self.message_time = None
        self.msg: Optional[Dict[str, Any]] = None
        self.notify = False
        self.files = []
    
    def set_message(self, msg: dict): 
        self.msg = msg
        msg_time = None
        if 'time' in msg:
            try:
                msg_time = msg['time']
            except:
                pass
        self.senders = msg['from']
        self.message_time = msg_time

    def time_string(self):
        if self.message_time is None:
            return '0000-00-00T00:00:00'
        return self.message_time.strftime('%Y-%m-%dT%H:%M:%S')

    def stamp_string(self):
        if self.message_time is None:
            return '00000000_000000'
        return self.message_time.strftime('%Y%m%d_%H%M%S')

    def add_file(self, file, recognized:bool, file_type:str):
        self.files.append({'file': file, 'recognized': recognized, 'type': file_type})

    def return_recipients(self):
        to_addrs = self.msg.get('to', '')
        to_addrs = parse_address_list(to_addrs)
        cc_addrs = self.msg.get('cc', '')
        cc_addrs = parse_address_list(cc_addrs)
        from_addr = parse_address_list(self.senders)
        addrs = [ ]
        addrs.extend(from_addr)
        addrs.extend(to_addrs)
        addrs.extend(cc_addrs)
        o = []
        for a in addrs:
            if a in IGNORE_RECIPIENTS:
                continue
            if a == '':
                continue
            o.append(a)
        return o

    def to_email(self):
        m = []
        show_expected = False
        if len(self.files) == 0:
            show_expected = True
            m.append("No file found in the attachments of the email")
        else:
            m.append("The following file(s) have been processed:")
            for f in self.files:
                if f.get('recognized', False):
                    r = 'Recognized'
                    file_type = f.get('type', None)
                    if file_type is not None:
                        r += " as {} data".format(file_type) 
                else:
                    show_expected = True
                    r = 'Not recognized (check the expected files)'
                m.append(" - '{}' {}".format(f['file'], r))
        if show_expected and source is not None:
            m.append('')
            m.append('Expected file names to provide data:')
            wildcard = False
            for file_def in self.source.files:
                f, file_type = file_def
                if '*' in f:
                    wildcard = True
                m.append(' - {} for {} data'.format(f, file_type))
            if wildcard:
                m.append("character '*' stands for any character after this, with the same extension")
                m.append("Beware that the name of the files are case-sensitive, if you change any character the file might be not recognized.")
        return m

def handle_message(handled: HandledMessage, logger: ImportLogger):
    """
        Handle a parsed message
        Here place for the influenzanet specific works
    """
    source = handled.source
    if source is None:
        return
    msg = handled.msg
    
    if  handled.message_time is None:
            print("Unable to parse date and its required")
            print(msg['date'])
            return None
    handled.notify = True
    for index, a in enumerate(msg['attachements']):
        name = a['filename']
        recognized, file_type = source.is_expected_file(name)
        handled.add_file(name, recognized, file_type)
        if not recognized:
            print("File '{}' is not recognized as an expected file".format(name))
            continue
        file_time = handled.stamp_string()
        target_name = "%s_%d_%s.csv" % (file_type, index, file_time)
        path = source.path + '/' + target_name
        if not os.path.isfile(path):
            print("Writing '%s' for '%s'" % (target_name, name))
            open(path, "wb").write(a['contents']) 
            logger.log(msg['from'], name, source.dir, handled.message_id, handled.time_string(), target_name)
        else:
            print("File '%s' already exists" % (path))

sources: List[SourceDef] = []

for idx, conf in enumerate(SOURCES):
    try:
        source = SourceDef(conf)
    except ValueError as e: 
        print("Problem for source {}".format(idx))
        raise e
    sources.append(source)

def find_source_sender(sender):
    for source in sources:
        if source.is_from(sender):
            return source
    return None

# account credentials
imap = imaplib.IMAP4_SSL(ACCOUNT['hostname'])
# authenticate
imap.login(ACCOUNT['username'], ACCOUNT['password'])

email_sender = MailSender(SENDING_ACCOUNT)

status, messages = imap.select(ACCOUNT['folder'])

print("Connecting..", status, messages)

since = datetime.datetime.now() - datetime.timedelta(weeks=DELAY_WEEKS)
date_since = imap_date(since)

importLog = ImportLogger(LOG_DB_PATH)

query = 'SINCE "'+ date_since  +'"'
    
status, data = imap.search(None, query)

if status != "OK":
    raise Exception("Error during fetch", status, data)
        
ids = []
if len(data) > 0:
    ids = data[0].split()

print("Found emails", ids)

for id in ids:
        
    if importLog.is_message_handled(id):
        print("{} handled, skip.".format(id))
        continue
    # fetch the email message by ID
    print("loading ", int(id))
    res, msgs = imap.fetch(str(int(id)), "(RFC822)")
    if res != "OK":
        print("Unable to fetch %d" % (id,), res, msgs)
        continue
    handled = HandledMessage(id)
    for response in msgs:
        if isinstance(response, tuple):
            if handled.msg is not None:
                print("Unexpected duplicate message part", response)
                continue
            # Only handle tuple parts
            try:
                parsed_msg = parse_message(response)
                #print("from ", parsed_msg.get('from'))
                #print("to ", parsed_msg.get('to'))
                #print("cc ", parsed_msg.get('cc'))
                handled.set_message(parsed_msg)
                if  'attachements' not in parsed_msg:
                    print("No attachment, skip")
                    continue
                source = find_source_sender(handled.senders)
                if source is None:
                    continue    
                handled.source = source
                handle_message(handled, importLog)
            except Exception as e:
                print("Error during handling of message {}".format(id))
                traceback.print_exception(e)

    source_name = 'unknown'
    if handled.source is not None:
        source_name = handled.source.dir
    handled_data = json.dumps(handled.files)
    importLog.message_handled(id, handled.senders, source_name, handled_data)
    if handled.notify:
        send = True
        if sending_only_from is not None:
            if handled.message_time is not None:
                if handled.message_time < sending_only_from:
                    send = False
        if send:
            dest = handled.return_recipients()
            print("Notifying to ", dest)
            subject = 'Re: ' + handled.msg['subject']
            body = [
                'Hi,',
                'Thank you for sending data to Influenzanet',
                'You receive this email because it has been processed by our automatic routine',
                '',
            ]
            body.extend( handled.to_email() )
            body.extend([
                '',
                'If you dont want to receive this email please let us know or define the Reply-To header for your email'
            ])
            try:
                email_sender.send(dest, subject, body)
            except Exception as e:
                print("Error during notification sending of message {}".format(id))
                print("Dest", dest)
                print("Subject:", subject)
                print("Body: ", body)
                traceback.print_exception(e)
