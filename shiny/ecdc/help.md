# ECDC Incidences

## Datasets

Several kind of datasets are provided :

- Externals : data provided by the platforms directly as aggregated data
- Public data : data prepared to be used in the public website (data are filtered)
- All : all datasets (without filtering)

## Methods

Several computation methods are in use to compute incidence. The name denote the parameter set used to determine active participants:

- wO : Mean data are the surveys submitted during the computed week with no correction
- w1_s2_if2_ex : Active participants have:
  * (s2) at least 2 survey in the season 
  * (w1) and a weekly survey in the week before, after or during the computed week, 
  * (if2) first survey of first season of each participant is removed,
  * (ex): not counted as an incidence case for the week if "same episode" question is responded as "No" 
- unknown : The computation method is not known (used for external datasets when computation method is not provided)