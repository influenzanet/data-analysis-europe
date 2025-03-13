# Sending Data to Influenzanet

## File Formats

- [Incidence file format](incidence_file_format.md) : For incidence data

## How to send data

Data are processed by automated routines and their adaptation capabilities are limited. Please make sure you use the good file format before sending data to allow to the routines
to run correctly (it really helps us).

### By email (works but is not very robust)

The email must fulfill some requirements to be processed :

- File data must be sent following the corresponding file format to allow integration in the database
- The subject of the e-mail must be **fixed** and not change at all (e.g. the subject must start with a fixed text)
- The list of possible senders addresses must be defined (or at least the sender's email domain), if email is sent with an unknown sender (or sender email domain) it will not be processed

Data is provided as an attached file in the email, to be identified by the routine. The filename must follow a predetermined pattern :
Either the exact same name or starting with a common prefix (such as incidence_italy_XXXXX.csv where XXXX may vary in each email) or a fixed name (the same name in every email for the same dataset type)

The best option is to provide a fixed file name. In this case a timestamp of the email's sending date will be added to the file name stored on the server.

**Note** : If any change occurs in the subject or in the file name, the routine could not recognize the email and the data will not be handled.

### Sending by web request

Sending by web request is more reliable and robust than sending by email, because we can directly acknowledge the good reception of the data and validate the file structure to limit the risk 
of problems during the data integration.

You can send data using a web request to [https://send.influenzanet.info](https://send.influenzanet.info). 

An example in R is given there [send_data.R](send_data.R). It's possible to use other tools, a documentation is available at the above address (open it in a browser).

You need to send us a request to obtain an authentication key before to use it.

