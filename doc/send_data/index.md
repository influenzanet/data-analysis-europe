# Sending Data to Influenzanet

## File Formats

- [Incidence file format](incidence_file_format.md) : File format to use to send incidence data

## How to send data

Data are processed by automated routines and their adaptation capabilities are limited. Please make sure you use the good file format before sending data to allow to the routines
to run correctly (it really helps us).

To send data, please contact us (at the influenzanet mailing list for example) so we can prepare the data integration.

We will need to define :

- File format (if you never sent data, please use the File formats defined above we will not handle custom format in the future)
- Sending method : Email or web request (each will require to define some )

### By email (works but is not very robust)

The email must respect some requirements to be correctly processed :

- The subject must start with a predetermined text (defined during setup) and must not be changed in the future
- The list of possible senders addresses must be defined (or at least the sender's email domain), if email is sent with an unknown sender (or sender email domain) it will not be processed
- The file name of the provided files must start with a predetermined text (defined during setup) and not change.

The email subject and the attached file name prefixes are determined with the Influenzanet team, please do not change them afterward, the routines would not recognized the file.

Actually, the best and simplest option is to use always the same email subject and attached file name, the routine will use the email sending time to differentiate the file versions.

If you want/need to change subject or attached file name, use a fixed prefix (for example incidence_xxxxx.csv for file name, where "xxxx" part can change) and do not change it in the future.

**Note** : If any change occurs in the subject or in the file name, the routine could not recognize the email and the data will not be handled.

As a summary, we must define:

- The subject of the email (at least a fixed prefix)
- The senders domain
- The name of the attached file (at least a fixed prefix) 

### Sending by web request

Sending by web request is more reliable and robust than sending by email, because we can directly acknowledge the good reception of the data and validate the file structure to limit the risk 
of problems during the data integration.

You can send data using a web request to [https://send.influenzanet.info](https://send.influenzanet.info). The documentation of how to send data using this method is described
on the home page of this web service.

An example in R is given there [send_data.R](send_data.R). It's possible to use other tools, a documentation is available at the above address (open it in a browser).

To use this method we must define with you:
- an authentication key before to use it.

