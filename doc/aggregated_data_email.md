# Send Aggregated data to Influenzanet

It's possible for a platform to send weekly aggregated data with computed indicators for ECDC Indicators

Provided indicators are described in [ecdc_file_format](./ecdc_file_format.md). This file describe the format as produced by 
Influenzanet, it's not expected to provide exactly this format.

Send us a file sample so we can check if it's possible to integrate it and if we clearly understand what data are sent.

## Sending Data by email

Aggregated data files can be sent to Influenzanet by email (email destination will be provided by the Influenzanet team).

The email must fulfill some requirements to be processed :

- Data must be sent using CSV format (no Excel file)
- A part of the subject of the email must be **fixed** and not change at all (for example the subject can start with a fixed text)
- The list of possible senders address must be defined 

File names must have a fixed part which will always be the same in each sending (for example start with a fixed text)

For example, If the file is 'Incidence_Country_202301', it's expected that the the starting of the file name "Incidence_Country" will remain the same) and clearly identify the purpose of the file (what kind of data we expect in it).

It's also possible to use the same file name for all sending, the file version will be processed on our side.

It's possible to send several kind of files, of course each kind of file must have a name (or a fixed part in name) to clearly identify its content.

Once defined with our team, please **do not change** without discussing with us, files will be processed automatically, and if you change anything the file could just be dropped.

Example of file names:

- 'France_Incidence.csv'
- 'Italy_Incidence_202301' : the fixed part to recognize the file will be 'Italy_Incidence', we expect incidence
- 'Italy_VisitToGP_202023' : the fixed part to recognize the file will be 'Italy_VisitToGP', we expect visit to GP indicator.


## File format

The file must be in **CSV format** to avoid problem during integration.

Minimal Required data:

- Week number as ISO 8601. Year and week can be provided in separated columns or in a single (2023W01 or 202301). The year must be the year of the week (%G of strftime). It's also possible to provide the date of the monday of the week.

- Number of active participants (number of distinct participants assumed for a given week*)

- Incidence for ILI syndromes, or ARI or both. The syndrome provided must be clearly identified (either in the column name or in a column a vertical organization of the data is used providing only one incidence kind by row).


Incidence can be provided as rate (please inform us about unit %, per 1000, per 10000) or as count (in this case rate will be computed by as count / active_participants)

Optional columns:

- Incidence confidence interval bounds: Use the same columns name as incidence with suffix '_low', '_up' for example

Column names : Unambiguous name, preferred lowercase with no space or special character. *'ili_incidence'*, *'week'* is good.

Once defined for a data source, the file format **must not be modified**.

File example:

Vertical format (one incidence kind by row). This is used if the active_participants count depends on the syndrome (by applying participants correction biais) 

```csv
yearweek,syndrome,incidence_rate,active_participants
202301,ILI,25,5235
202301,ARI,5,5235
```

Horizontal format 
```csv
yearweek,syndrome,ili_incidence_rate,ari_incidence_rate,active_participants
202301,25,5,5235
```


