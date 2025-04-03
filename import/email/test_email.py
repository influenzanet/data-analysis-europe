from email_utils import MailSender
from settings import SENDING_ACCOUNT

sender = MailSender(SENDING_ACCOUNT)

sender.send(['clement.turbelin@iplesp.upmc.fr'], 'Test from influnenzanet@epipop.fr', 'Test email')