# all_catch: Catch data (occurrences) for all surveys/regions

Contains catch data from all surveys/regions for available top species.
Catch data pulled from packages nwfscSurvey and surveyjoin

## Usage

``` r
all_catch
```

## Format

A data frame with 4052 rows and 8 variables:

- region:

  region of data collection

- survey:

  subregion

- common_name:

  common name of specimen

- scientific_name:

  scientific name of specimen

- year:

  year tows were conducted

- n_tows:

  total number of tows conducted

- n_pos:

  number of positive tows

- proportion_pos:

  proportion of total tows that were positive
