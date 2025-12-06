# lw_vb_fit_doc: Documentation of lw and vb predictions

Contains log-log length-weight regression estimates for the top species
of each survey group, separated by sex.

## Usage

``` r
lw_predictions
```

## Format

A data frame with 10122 rows and 9 variables:

- fit_length:

  fish lengths in cm

- fit_weight:

  predicted weight in kg at length in cm

- sex:

  sex, M or F

- a:

  Scaling coefficient, intercept of length-weight regression

- b:

  exponent, slope of length-weight regression

- region:

  science center of data collection

- common_name:

  common species name

- survey:

  survey within region

- scientific_name:

  scientific species name
