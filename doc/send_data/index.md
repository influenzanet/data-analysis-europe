# Sending data to Influenzanet

## File formats

- [Incidence file format](incidence_file_format.md) : File format to use when sending incidence data

## How to send data

Files are processed by automated routines and their customisation capabilities are limited. Please make sure you use the good file format before sending data to allow to the routines
to run correctly (it really helps us).

To send data, please contact us (e.g. on the influenzanet mailing list for example) so that we can prepare the data integration.

We will need to define :

- File format (if you have never sent data before, please use the file formats defined above we will not handle custom formats in the future)
- Sending method : email or web request (each will need to set up some elements with Influenzanet team)

### By email (works but is not very robust)

The email **must** respect some requirements to be processed correctly :

- The subject must start with a predefined text (defined during the setup phase) and must not be changed in the future
- The list of possible sender addresses should be defined (or the email domains), if email is sent with an unknown sender (or sender email domain) it will not be processed
- The filename of the attached files must start with a predefined text (defined during setup) and must not change in the future.

The email subject and the attached file name prefixes are set up with the Influenzanet team, please do not change them afterwards, the routines would not recognise the file.

Actually, the best and simplest option is to always use the same email subject and attached file name, the routine will use the time of email (sending date/time) to differentiate the file versions.

If you want/need to change subject or attached filename, please use a fixed prefix (for example incidence_xxxxx.csv for file name, where the "xxxx" part can change) and do not change it in the future.

**Note** : If any change occurs in the subject or in the filename, the routine may not be able to recognize the email and the data will not be processed.

As a summary, we must define:

- The subject of the email (at least a fixed prefix)
- The sender's domain (or the list of possible senders)
- The name of the attached file (at least a fixed prefix)

### Sending by web request

Sending by web request is more reliable and robust than sending by email, because we can directly confirm the good reception of the data and validate the file structure to limit the risk 
of problems during the data integration.

You can send data using a web request to [https://send.influenzanet.info](https://send.influenzanet.info). Documentation on how to send data using this method is described
on the home page of this web service.

An example in R is given there [send_data.R](send_data.R). It's possible to use other tools, a documentation is available at the above address (open it in a browser).

In order to use this method we need to define with you:
- an authentication key before using it.

