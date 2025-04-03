from email_utils import MailSender
from settings import SENDING_ACCOUNT

sender = MailSender(SENDING_ACCOUNT)

test = SENDING_ACCOUNT.get('test', None)

if test is None:
    raise ValueError("Test email must be defined in SENDING_ACCOUNT.test")

sender.send(test, 'Test email', 'Test email')