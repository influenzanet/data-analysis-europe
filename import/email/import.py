import imaplib
import email
import datetime
from email.header import decode_header
from email.utils import parseaddr
from typing import Dict,List,Union
import re
import os

from settings import ACCOUNT, OUTPUT_PATH,SOURCES

# account credentials
imap = imaplib.IMAP4_SSL(ACCOUNT['hostname'])
# authenticate
imap.login(ACCOUNT['username'], ACCOUNT['password'])

status, messages = imap.select(ACCOUNT['folder'])

print("Connecting..", status, messages)

since = datetime.datetime.now() - datetime.timedelta(weeks=2)

def imap_date(time: datetime) -> str:
    d = time.date()
    mm = ["Jan" , "Feb" , "Mar" , "Apr" , "May" , "Jun" ,"Jul" , "Aug" , "Sep" , "Oct" , "Nov" , "Dec" ]
    m = mm[d.month - 1]
    return "%0d-%s-%d" % (d.day, m, d.year)

def parse_headers(raw):
    hh = decode_header(raw)
    headers = []
    for h in hh:
        value, encoding = h
        if isinstance(value, bytes):
            if encoding is None:
                encoding = 'utf-8'
            value = value.decode(encoding)
        headers.append(value)
    if len(headers) ==  1:
        headers = headers[0]
    return headers

def parse_message(msg_raw):
    """
    Parse raw message into simple message structure with attachements
    """
    msg = email.message_from_bytes(msg_raw[1])
    rr = {}
    for k in ['From', 'Subject','Date','Message-ID', 'Received','To']:
        n = k.replace('-','_').lower()
        rr[n] = parse_headers(msg.get(k))

    if msg.is_multipart():
        attachements = []
        # iterate over email parts
        for part in msg.walk():
            # extract content type of email
            content_type = part.get_content_type()
            content_disposition = str(part.get("Content-Disposition"))
            if "attachment" in content_disposition:
                # download attachment
                filename = part.get_filename()
                attachements.append({"filename": filename, "contents": part.get_payload(decode=True), "type": content_type})

        rr['attachements'] = attachements

    return rr

def check_address(address, valides:Union[str, List[str]]) -> bool:
    if isinstance(address, str):
        address = [address]
    for a in address:
        _,email = parseaddr(a)
        email = email.lower()
        # We only need to check if it looks like a email not if it's a valid address
        if not re.match(r"[^@]+@[^@]+\.[^@]+", email):
            continue
        if email in valides:
            return True
    return False

def handle_message(msg: dict, source: dict):
    """
        Handle a parsed message
        Here place for the influenzanet specific works
    """
    if not 'attachements' in msg:
        print("No attachment, skip")
        return
    if not check_address(msg['from'], source['from']):
        print("From '%s' is not in expected from" % msg['from'])
        return

    out_path = OUTPUT_PATH + '/' + source['dir']

    if not os.path.exists(out_path):
        os.mkdir(out_path)

    for a in msg['attachements']:
        name = a['filename']

        path = out_path + '/' + name
        if not os.path.isfile(path):
            print("Writing '%s'" % name, )
            open(path, "wb").write(a['contents']) 
        else:
            print("File '%s' already exists" % (path))

date_since = imap_date(since)

for source in SOURCES:

    query = 'SUBJECT "' +  source['subject'] + '" SINCE "'+ date_since  +'"'

    status, data = imap.search(None, query)

    if status != "OK":
        raise Exception("Error during fetch", status, data)
        
    print("Found email ", data)
    if(len(data) == 0):
        print("Empty data")
        continue

    ids = data[0].split()

    if(len(ids) == 0):
        print("No ids found")
        continue

    for id in ids:
        # fetch the email message by ID
        print("loading ", int(id))
        res, msgs = imap.fetch(str(int(id)), "(RFC822)")
        if not res == "OK":
            print("Unable to fetch %d" % (id,), res, msgs)
            continue
        for response in msgs:
            if isinstance(response, tuple):
                m = parse_message(response)
                handle_message(m, source)
    
