# Incidence File Format

This file describes the file format expected when sending incidence data.
The file format is : **CSV** file with comma separated values. Numerical values must be separated by a period '.' (e.g. use '0.120' instead of '0.120').
Please do not change the file structure (e.g. field separators), it will not be possible to import the data if the file use a different format.

## Columns:

Column names must be as follows, lowercase or uppercase can be used, but **do not change the name**, only case changes are accepted.

Mandatory columns:
- `yearweek`: year week number as ISO week number, see section on year week column.
- `active_count`: Number of active participants for the given week (incidence denominator).
- `incidence_rate`: Incidence rate, if not between [0,1] rate please provide `factor` column to inform us of the scale used

Optional columns:
- `incidence_count`: column to provide the number of participants who are considered as incidence for the given week. 
- `syndrome`: name of the syndrome used to count the participants for the incidence, accepted values are
    - ili': ILI according to the ECDC definition
    - 'covid': Covid specific definition (probably not standardised)
    - Other syndrome: please contact us if you want to provide incidence using a different definition     
- `factor`: scaling factor used to calculate incidence (if per 1000, factor is 1000; if per 100,000, factor is 100,000)
- `incidence_upper_ci`: upper bound for confidence interval, if calculated
- `incidence_lower_ci` : Lower bound for confidence interval if calculated

## YearWeek column

Week number as ISO 8601 with year. The year must be the year of the week (%G of strftime). 
It's also possible to give the date of the Monday of the week.
The value of the week can be represented using several formats (using week 01 of the year 2024 as an example)
- 2024W01
- 2024-W01
- 202401 

If the Monday of the week is provided to represent the week, it must be in the `YYYY-MM-DD` format as `2024-01-01`.

# Examples

Simple file with minimal columns

```csv
yearweek,active_count,incidence_rate
2024W01,1234,0.290
2024W02,1234,0.240
2024W03,1234,0.234
```

Another example, to send data with incidence as per 1000 participants values
In this case you have to add the `factor` column to inform us about the scale of the incidence values

```csv
yearweek,active_count,incidence_rate,factor
2024W01,1234,290,1000
2024W02,1234,240,1000
2024W03,1234,234,1000
```

If you need to transfer data about several syndromes (if you compute incidence using several definitions).
Be aware of the know definitions, if you send data with a syndrome name that is not handled, data will be discarded.
Of course if you think a new definition could be worth having it, contact us !

```csv
yearweek,syndrome,active_count,incidence_rate
2024W01,ili,,1234,0.290
2024W02,ili,1234,0.240
2024W03,ili,1234,0.234
2024W01,covid,,1234,0.290
2024W02,covid,1234,0.240
2024W03,covid,1234,0.234
```