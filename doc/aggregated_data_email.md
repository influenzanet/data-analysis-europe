# Send Aggregated data to Influenzanet

It's possible for a platform to send weekly aggregated data with computed indicators for ECDC Indicators

Provided indicators are described in [ecdc_file_format](./ecdc_file_format.md). This file describe the format as produced by 
Influenzanet, it's not expected to provide exactly this format.

## Sending Data by email

Aggregated data files can be sent to Influenzanet by email (email destination will be provided by the Influenzanet team).

The email must fulfill some requirements to be processed :

- Data must be sent using CSV format (no Excel file)
- A part of the subject of the email must be **fixed** and not change at all (for example the subject can start with a fixed text)
- The list of possible senders address must be defined (or at least email domain of senders)

File names must have a fixed part which will always be the same in each sending (for example start with a fixed text)

For example, If the file is 'Incidence_Country_202301', it's expected that the the starting of the file name "Incidence_Country" will remain the same) and clearly identify the purpose of the file (what kind of data we expect in it).

It's also possible to use the same file name for all sending, the file version will be processed on our side.

The file format must conform to the Incidence file format [Incidence Data file format](./send_data/incidence_file_format.md)

