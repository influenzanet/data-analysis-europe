from email.header import decode_header
from email.utils import parseaddr, parsedate, parsedate_to_datetime
import email
from typing import Dict,List,Union
import re
import os
import datetime
from email.message import EmailMessage
import smtplib

class MailSender:

    def __init__(self, conf):
        self.hostname = conf['hostname']
        self.user = conf.get('username', '')
        self.password = conf.get('password', '')
        self.debug = conf.get('debug', 0)
        self.from_email = conf['from']
        self.use_ssl = conf.get('ssl', False)
        self.use_starttls = conf.get('starttls', False)
        self.reply_to = conf.get('reply_to', None)
        self.carbon_copy = conf.get('cc', None)
    
    def send(self, targets, subject, body):
        if self.use_ssl:
            s = smtplib.SMTP_SSL(self.hostname)
        else:
            s = smtplib.SMTP(self.hostname)
        if self.debug > 0:
           s.set_debuglevel(self.debug)
        if self.use_starttls:
            s.starttls()
        if self.user != '':
            s.login(self.user, self.password)
        msg = EmailMessage()
        
        msg['From'] = self.from_email
        msg['Subject'] = subject
        msg['To'] = targets
        if self.reply_to is not None:
            msg['Reply-To'] = self.reply_to

        if self.carbon_copy is not None:
            msg['CC'] = self.carbon_copy
        if isinstance(body, list):
            body = "\n".join(body)
        msg.set_content(body)
        s.send_message(msg)
        s.quit()


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
    for k in ['From', 'Subject','Date','Message-ID', 'Received','To', 'CC', 'Reply-To']:
        n = k.replace('-','_').lower()
        h = msg.get(k)
        if h is not None:
            rr[n] = parse_headers(h)

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
    
    if 'date' in rr:
        rr['time'] = parsedate_to_datetime(rr['date'])

    return rr

def parse_address_list(aa):
    if aa is None:
        return []
    if isinstance(aa, str):
        aa = aa.split(',')
    if not isinstance(aa, list):
        return []
    o = []
    for addr in aa :
        _, email = parseaddr(addr)
        email = email.lower()
        if not re.match(r"[^@]+@[^@]+\.[^@]+", email):
            continue
        o.append(email)
    return o

def check_address(address, valides:List[str]) -> bool:
    if isinstance(address, str):
        address = [address]
    for a in address:
        _,email = parseaddr(a)
        email = email.lower()
        # We only need to check if it looks like a email not if it's a valid address
        if not re.match(r"[^@]+@[^@]+\.[^@]+", email):
            continue
        for valid in valides:
          if valid.startswith("@"):
            i = email.rindex("@")
            domain = email[i:]
            if domain == valid:
              return True
          else:
            if email == valid:
              return True
    return False

def add_file_timestamp(name, time):
    p = os.path.splitext(name)
    time = time.replace(':','-')
    n = p[0] + '_' + time
    return n + p[1]
