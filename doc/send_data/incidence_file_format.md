# Incidence File Format

This file describes the file format expected when sending incidence data.

This file format is mandatory. Please do not change the file structure (e.g. field delimiters).
The file format is : **CSV** file with comma separated values. Numerical values must be separated by a dot '.' (e.g. use '0.120' instead of '0.120').

Some comment line can be provided inside the file, the line must start with the character '#'

Encoding: The file encoding must be 'UTF-8'

## Columns:

Column names must be as follows, lowercase or uppercase can be used, but **do not change the name**, only case changes are accepted.

Some columns could accept alternate name to keep compatibility with legacy file format.

Mandatory columns:
- `yearweek`: year week number as ISO week number, see section on year week column. Accepted alias :`yw`
- `active_count`: Number of active participants for the given week (incidence denominator). Accepted alias : `active`
- `incidence_rate`: Incidence rate, if not between [0,1] rate please provide `factor` column to inform us of the scale used. Accepted alias `incidence`
- `syndrome`: name of the syndrome used to count the participants for the incidence, accepted values are
    - 'ili.ecdc': ILI according to the ECDC definition
    - 'covid.ecdc': Covid specific definition (probably not standardised)
    - Other syndrome: please contact us if you want to provide incidence using a different definition     

Optional columns:
- `incidence_count`: column to indicate the number of participants who are considered as incidence for the given week. 
- `factor`: scaling factor used to calculate incidence (if per 1000, factor is 1000; if per 100,000, factor is 100,000)
- `incidence_upper_ci`: upper bound for confidence interval, if calculated
- `incidence_lower_ci` : lower bound for confidence interval if calculated

The combination of `yearweek` and `syndrome` must be unique (in other words, only provide one incidence value by week and syndrome).

## YearWeek column

Week number as ISO 8601 with year. The year must be the year of the week (%G of strftime). 
It's also possible to give the date of the Monday of the week.
The value of the week can be represented using several formats (using week 01 of the year 2024 as an example)
- 2024W01
- 2024-W01
- 202401 

If the Monday of the week is given to represent the week, it must be in the format `YYYY-MM-DD` as `2024-01-01`.

# Examples

Simple file with minimal columns

```csv
yearweek,active_count,incidence_rate
2024W01,1234,0.290
2024W02,1234,0.240
2024W03,1234,0.234
```

Another example, to send data with incidence as per 1000 participants values
In this case you need to add the `factor` column to inform us about the scale of the incidence values

```csv
yearweek,active_count,incidence_rate,factor
2024W01,1234,290,1000
2024W02,1234,240,1000
2024W03,1234,234,1000
```

If you need to send data about several syndromes (if you calculate incidence using several definitions).
Be aware of the known definitions, if you send data with a syndrome name that is not handled, the data will be discarded.
Of course if you think a new definition might  be worth having it, please contact us !

```csv
yearweek,syndrome,active_count,incidence_rate
2024W01,ili,,1234,0.290
2024W02,ili,1234,0.240
2024W03,ili,1234,0.234
2024W01,covid,,1234,0.290
2024W02,covid,1234,0.240
2024W03,covid,1234,0.234
```