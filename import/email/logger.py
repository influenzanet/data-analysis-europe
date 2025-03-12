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
        cur.execute('CREATE TABLE email_imports ("time" DATETIME, "from" TEXT, "file" TEXT, "dir" TEXT, "msg_id" TEXT, "msg_time" DATETIME)')
        cur.close()

    self.db = db
    
  def log(self, from_email, file_name, dir, message_id, message_time ):
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
    }
    print(d)
    cur.execute('INSERT INTO email_imports ("time", "from", "file", "dir", "msg_id", "msg_time") VALUES(:time, :from, :file, :dir, :msg_id, :msg_time )', d)
    cur.close()
    self.db.commit()
  
