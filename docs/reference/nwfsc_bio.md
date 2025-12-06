# nwfsc_bio: Biological data for NWFSC survey

Contains biological data from NWFSC surveys for available top species
defined in data-raw/nwfsc_top.rds.

## Usage

``` r
nwfsc_bio
```

## Format

A data frame with 1491160 rows and 11 variables:

- region:

  region of data collection

- survey:

  subregion

- year:

  year specimen was collected

- common_name:

  common name of specien

- scientific_name:

  scientific name of specimen

- sex:

  specimen sex, M, F, or U (unknown)

- length_cm:

  length of specimen, in cm

- weight_kg:

  weight of specimen, in kg

- age_years:

  age of specimen, in years

- otosag_id:

  Unique to nwfsc dataset, defines number of available age structures

- depth_m:

  Depth in meters
