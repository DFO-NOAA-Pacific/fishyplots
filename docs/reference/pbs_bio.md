# pbs_bio: Biological data for PBS survey

Contains biological data from PBS surveys for available top species
defined in data-raw/pbs_top.rds.

## Usage

``` r
pbs_bio
```

## Format

A data frame with 906520 rows and 10 variables:

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

- depth_m:

  Depth in meters
