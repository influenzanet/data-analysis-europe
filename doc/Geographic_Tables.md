Geographic Tables
==================

Geographic tables handle geographic levels description in each country and demographic data 

Geographic levels
-----------------

The Influenzanet european database uses NUTS levels to describe subdivisions of the european countries.

The database handles 5 levels : country, nuts1, nuts2, nuts4, zip

"zip" level is the atomic level used in the intake survey to describe each participant location. It is at least NUTS3 level (for Ireland) or a more precise location (zip code for example).
Therefore the "zip" level designate a differend kind of geographic subdivision in each participant country (as far as the choice of this level).

Known levels are :
- Italy : Zip code
- France : Municipality administrative code (INSEE)
- Ireland : NUTS 3 level

Each geograhic level is associated with a table, named geo_[level_code]. This table describes all subdivisions at this level.
A subdivision is described by a subdivision code and a title. The table also contains codes from the upper levels the subdivision belongs to.

All the geographic tables follow the same naming convention for columns (no exception allowed)

- "country" : country code
- "code_[level] : subdivision code at the geograhic level [level], where [level] is 'zip','nuts1','nuts2','nuts3'
- "title" : title of a subdivision


Demographic tables
--------------------

These tables contain population data at each geographic levels. 
Two kind of population tables are availables (each kind named using a prefix)
 - "pop_[level]" : by georaphic subdivision and gender
 - "pop_age5_[level]" : by geographic subidivision, gender and 5  year age groups

Common columns:
 - "code_[level]"
 - "country"
 - "all" : all gender population (male + female)
 - "male"
 - "female"
 - "year" : year the population is intented to be used
 - "year_ref": reference year of the population (real year of the population)
 
year_ref and year are used to differentiate a set of populations for a given year (for example population of the year 2014) but using data from different year in each country
As population estimated are not available at the same time in all countries, this table allows to have complete data for all countries for each year.
The last available estimation is used for each countries.

pop_age5 table has 2 more columns :
- age_min: lower age of the group
- age_max: upper age of the group

