# Sending Data to Influenzanet

## File Formats

- [Incidence file format](incidence_file_format.md) : For incidence data

## How to send data

Data are processed by automatic routines and their adaptation capabilities are limited. Please ensure you use the good file format before sending data to allow to routines
to run correctly (it really helps us).

### By email (working but not robust)

The email must fulfill some requirements to be processed :

- File data must be sent following a predetermined file format to allow integration in the database
- Subject of the email must be **fixed** and not change at all (for example the subject must start with a fixed text)
- The list of possible senders address must be defined (or at least email domain of senders), if email is sent with another sender it will not be processed

Data are provided as an attached file in the email, to be identified by the routine, the file name must follow a predetermined pattern :
either the exact same name or starting with a common prefix (like incidence_italy_XXXXX.csv) where XXXX can change in each email

The best option is to provide a fixed file name, timestamp of the email is added to the file

**Note** : If any change occurs in the subject or in the file name, the routine could not recognize the email and the data will not be handled.

### By web request

Web sending is more reliable and robust by email, because we can acknowledge good reception of the data directly and validate the file structure to limit the risk 
of problems during the data integration.

You can send data using a web request to https://send.influenzanet.info. 

An example in R is given there [send_data.R](send_data.R). It's possible to use other tools, a documentation is available at the address above (open it in a browser).

You need to send us a request to obtain an authentication key before to use it.

