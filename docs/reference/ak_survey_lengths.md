# ak_survey_lengths: length count data from AFSC surveys

Contains biological length counts from AFSC surveys for available top
species defined in data-raw/afsc_top.rds, separated by survey group.
Compiled from large external dataset. Filtering can be found in
data-raw/filter_ak_lengths.R

## Usage

``` r
ak_survey_lengths
```

## Format

A data frame with 1158 rows and 5 variables:

- year:

  year of data collection

- survey:

  subregion group within AFSC: AK GULF (Gulf of Alaska) or AK BSAI
  (Bering Sea and Aleutian Islands)

- common_name:

  common name of specien

- scientific_name:

  scientific name of specimen

- length.count:

  number of lengths taken for specimen in survey and year
