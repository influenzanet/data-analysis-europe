Import Geographic levels & Population data from EuroStat
=========================================================

Load Population data from Eurostat
--------------------
* Extract NUTS 2 levels & save it as CSV files using ";" separator

Expected columns are

 NUTS CODE;NUTS LABEL;NUTS LEVEL;COUNTRY CODE;COUNTRIES SORTING ORDER;ORDER

* Extract population data with 5-year-age group and sex at NUTS 2 level, 
 URL :http://epp.eurostat.ec.europa.eu/portal/page/portal/population/data/database
	 http://appsso.eurostat.ec.europa.eu/nui/show.do?query=BOOKMARK_DS-272662_QID_4E7C0E14_UID_-3F171EB0&layout=TIME,C,X,0;GEO,C,Y,0;SEX,C,Z,0;AGE,L,Z,1;INDICATORS,C,Z,2;&zSelection=DS-272662SEX,T;DS-272662AGE,TOTAL;DS-272662INDICATORS,OBS_FLAG;&rankName1=AGE_1_2_-1_2&rankName2=SEX_1_2_-1_2&rankName3=INDICATORS_1_2_-1_2&rankName4=TIME_1_0_0_0&rankName5=GEO_1_2_0_1&sortC=ASC_-1_FIRST&rStp=&cStp=&rDCh=&cDCh=&rDM=true&cDM=true&footnes=false&empty=false&wai=false&time_mode=ROLLING&time_most_recent=true&lang=EN&cfo=%23%23%23%2C%23%23%23.%23%23%23
 Datasetname : Population on 1 January by five years age groups and sex - NUTS 2 regions (demo_r_pjangroup) 
	* All years from 2009 
	* use "With code" option (avoid "label"), with Total
	* Include all age groups (including total, unknown and sometimes several upper bounds)
    * Include "Flag and Footnotes"
	
Expected columns are :
 * "TIME" : Year of population
 * "GEO" : NUTS 2 code
 * "SEX" : "T","F","M" 
 * "AGE" : Age group code for ex. "Y45-49", "TOTAL", "UNK" (for unknown), Y_GE80 (for upper bounds)
 * "Value" : Population for the strata (English notation, "," as thousands separator)
 * "Flag and Footnotes"
 
Save it as CSV ("," sep) into data/eurostat/ (with meaningfull name for example eurostat_population_nuts2_2015.csv)

JSON REST Service
http://ec.europa.eu/eurostat/wdds/rest/data/v2.1/json/en/demo_r_pjangroup?sex=T&precision=1&sinceTimePeriod=2009&age=TOTAL&age=UNK&age=Y10-14&age=Y15-19&age=Y20-24&age=Y25-29&age=Y30-34&age=Y35-39&age=Y40-44&age=Y45-49&age=Y5-9&age=Y50-54&age=Y55-59&age=Y60-64&age=Y65-69&age=Y70-74&age=Y75-79&age=Y80-84&age=Y_GE75&age=Y_GE80&age=Y_GE85&age=Y_LT5


Extract population data for each year & geo levels
--------------------------

Edit eurotstat.r
 * modify output.years, with list of years to extract
 * population.file, with name of the population file saved from eurostat

Run eurostat.r (in root dir)
 
 * Will generate all files in data/nuts
 * Will generate all population files (data/population)
	* total by for each geo levels
	* age_group for each geo levels (country, nuts1, nuts2)
	
For each year from 2011-2013, pick the last available census available in each country (sometimes the last census could not be available for the current year).
The year of the used census for each country will be registred in the "year_ref" column

Some geo levels can be excluded (like overseas regions in France, counted in global population), since those population are not included in InfluenzaNet project yet.

Create SQL for each year
---------------------------

> python bin/import-population.py [year] 
Will create SQL command to import population data into Db for the given year

