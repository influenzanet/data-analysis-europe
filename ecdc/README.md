# ECDC indicator computation

This directory contains scripts dedicated to create ECDC indicators

## Scripts

This subprocject is organized with several kind of scripts:
 
 - indicator computation scripts are doing the computation
 - data management & vizualization script
 - 

### Indicators Computation scripts ( ):

- ecdc_indicator.R : build incidence indicator for one country & season, for all syndromes and methods (active participants selection)
- ecdc_healthcare.R : builds healtchare indicator for one country & season

- ecdc_runner.R is a command line script to parametrized the call of the scripts above, it will run each script for one country and season

```bash
  Rscript ecdc_runner.R country=[country] season=[season]
```

Output Data are stored in the output directory (configured in location.R at the root for this repo once configured) under the path /ecdc/indicator/[country]/

Each one create a rds file, with a date suffix, and a ".last" file (with the same filename but without the date suffix) containing the name of the last generated file. 

Files pattern are : [type]-[season]-[date].rds and [type]-[season].last 

Where :
 type : incidence, healthcare
 season: season number (year number of the starting of the period, usualy november of a given year), so 2019 for 2019-2020, 2020 for 2020-2021, ...
 data : Y-m-d date


### Data managemeent & vizualisation scripts

- build_bundle.R : Collect all results (from computations) and build "bundles" (filtered datasets for ecdc) and "datasets" with all results (unfiltered)
- graph_bundle.R : create graphs for all bundles (incidence, healthcare...)
- graph_all.R : create graphs from full datasets (unfiltered data)

Filters are removing some weeks that are not interpretabe (like the very start week of some seasons, or for some countries when data are not reliable)

### Utiity scripts

- compute_all_season.R : call computations scrpt for all countries and seasons (very long)

## Workflow

Current workfow is :

Computation (season, country) -> build_bundle -> graph_all, graph_bundle

buid_bundle step is 
 
 - collecting rds files for healtcare and incidence in each country, using the last generated .rds for each season (indicated in each '.last' file)
 - creating a datasets.rds file in [global_output_dir]/ecdc/indicator/
 - extracting and filtering all data table for ecdc
 - creating a bundles.rds file in [global_output_dir]/ecdc/indicator/
 - creating a csv file for all tables by country in [global_output_dir]/ecdc/indicator/bundles/

