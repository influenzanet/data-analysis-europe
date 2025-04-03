import sqlite3
import time
from pathlib import Path

class ImportLogger:
  
  def __init__(self, path: str):
    
    db_path = Path(path)
    create_db = not db_path.exists()
    db = sqlite3.connect(db_path)

    if create_db:
        cur = db.cursor()
        cur.execute('CREATE TABLE email_imports ("time" DATETIME, "from" TEXT, "file" TEXT, "dir" TEXT, "msg_id" TEXT, "msg_time" DATETIME, "target_file" TEXT)')
        cur.execute('CREATE TABLE email_handled ("time" DATETIME, "from" TEXT, "msg_id" TEXT, "source" TEXT, handled TEXT)')
        cur.close()

    self.db = db
  
  def message_handled(self, message_id, from_email, source, handled):
    cur = self.db.cursor()
    
    if isinstance(from_email, list):
      from_email = ' '.join(from_email)
    
    d = {
      "time": time.strftime("%Y-%m-%dT%H:%M:%S"),
      "from": from_email,
      "msg_id": message_id,
      "source": source,
      "handled": handled,
    }
    cur.execute('INSERT INTO email_handled ("time", "from", "msg_id", "source", "handled") VALUES(:time, :from, :msg_id, :source, :handled )', d)
    cur.close()
    self.db.commit()
  
  def is_message_handled(self, message_id):
    cur = self.db.cursor()
    res = cur.execute("SELECT * FROM email_handled where msg_id=:msg_id", {'msg_id': message_id})
    return res.fetchone() is not None  
    
  def log(self, from_email, file_name, dir, message_id, message_time, target_file ):
    cur = self.db.cursor()
    
    if isinstance(from_email, list):
      from_email = ' '.join(from_email)
    
    d = {
      "time": time.strftime("%Y-%m-%dT%H:%M:%S"),
      "from": from_email,
      "file": file_name,
      "dir": dir,
      "msg_id": message_id,
      "msg_time": message_time,
      "target_file": target_file
    }
    cur.execute('INSERT INTO email_imports ("time", "from", "file", "dir", "msg_id", "msg_time", "target_file") VALUES(:time, :from, :file, :dir, :msg_id, :msg_time, :target_file )', d)
    cur.close()
    self.db.commit()
  
