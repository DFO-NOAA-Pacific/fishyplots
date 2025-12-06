# afsc_bio: Biological data for AFSC survey

Contains biological data from AFSC surveys for available top species
defined in data-raw/afsc_top.rds.

## Usage

``` r
afsc_bio
```

## Format

A data frame with 485258 rows and 10 variables:

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
