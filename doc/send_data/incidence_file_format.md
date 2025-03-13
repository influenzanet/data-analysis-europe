# Incidence file format

This file describe the file format expected when you send incidence data.

This file format is mandatory. Please do not change the file structure (like field separator)

The file format is : **CSV** file using comma separated values. Numerical values must use as dot '.', as fractional separator (e.g. use '0.120' instead of '0,120')

## Columns:

Column names must be the following ones, lowercase or upper case can be used, but **do not change the name** (no space, no separator like `_`)

Mandatory columns:
- `yearweek`: year week number as ISO week number, see Yearkweek column section
- `active_count`: Count of active participants for the given week (incidence denominator)
- `incidence_rate`: Incidence proportion, if not between [0,1] proportion please provide `factor` column to inform us of the scale used

Optional columns:
- `incidence_count`: column to provide the count of participant accounted for the incidence for the given week. 
- `syndrome` : Name of the syndrome use to count participant for incidence, accepted values are
    - 'ili': ILI using the ECDC definition
    - 'covid': Covid specific definition (probably not standardized)
    - Other syndrome : please contact us if you want to provide incidence using another     
- `factor`: scale factor used to compute the incidence (if per 1000, factor is 1000; per 100,000 factor is 100,000)
- `incidence_upper_ci` : Upper bound for confidence interval if computed
- `incidence_lower_ci` : Lower bound for confidence interval if computed

## Yearweek column

Week number as ISO 8601 with the year. The year must be the year of the week (%G of strftime). 
It's also possible to provide the date of the monday of the week.
The value of the week can be reprensented using the several formats (using the week 01 of year 2024 as example)
- 2024W01
- 2024-W01
- 202401 

If monday of the week is provided to represent the week, it must be using `YYYY-MM-DD` format as `2024-01-01`

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