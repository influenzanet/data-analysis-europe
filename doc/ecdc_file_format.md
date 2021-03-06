# Formats used to export ECDC data

Description of the file format used to exchange data (from analysis to public website).

## Common definitions

### Week numbers

Week numbers are computed using the ISO 8601 Week rules computation.
This week number has 2 components : Year of the ISOweek ('%G' format of strftime), and week number (%V format of strftime). The "year of the week" can be different
from the year of the dates of the days in a week (for example 2020-W53 includes 2021-01-01).

In all the files, they are encoded in a numeric format using : year_of_the_week * 100 + week. (e.g. '2020-W01' will be provided as 202001 number, '2019-W52' as 201952).

### Season number

Number identifying the current activity period. These activity period used to be the winter season (to monitor influenza). Common season periods are defined from September to August of the next year.
The season number is the year of the starting year of the period, usually the year number of the first day of the period (e.g for the period 2019-2020, starting from the 2019-09-01, we use the starting number year '2019' to identify this period).

### Platform codes

A code is attributed to each platform to identify their data, mostly from ISO country code (with UK exception). Used codes are are defined in share/plaform/eu.r

## Active participant data

- yw : ISO Week number in numeric format (as defined in Common Definitions)
- season : season number (see Common Definitions)
- country : country/platform code
- method : active participant computation method name 
- syndrome : kind of measure, here "active" (extract from a subset of incidence output, could be optional)
- active : number of active participant for the given week, in the country

## Incidence data

- yw : ISO Week number in numeric format (as defined in Common Definitions)
- country : country/platform code
- syndrome : name of the syndrome definition used to count the event
- type: type of adjusment "adj"= adjusted, "crude": not adjusted, public data are provided only with "adj" type
- method: name of the active participant selection method
- incidence : computed incidence rate (float range [0; 1])
- upper: upper bound of the confidence interval of the incidence rate (CI at 95%)
- lower: lower bound of the confidence interval of the incidence rate
- count : number of active participants matching the syndrome definition for this week
- part: active participant number (optional)
- season: season number (see Common Definitions)