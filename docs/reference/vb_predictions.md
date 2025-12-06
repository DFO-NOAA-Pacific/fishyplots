# Alternate data storage for von Bertalanffy predictions

Contains von Bertalanffy growth curve estimates for each fish age
bracket

## Usage

``` r
vb_predictions
```

## Format

A data frame with 3854 rows and 9 variables:

- age_years:

  Fish age in years

- fit:

  Growth estimate (length cm)

- sex:

  Fish sex, male or female

- linf:

  Asymptotic length of growth curve

- k:

  growth coefficient, rate of growth

- t0:

  exponent, slope of length-weight regression

- survey:

  subregion (NWFSC, PBS, AK GULF, AK BSAI)

- common_name:

  Species common name

- scientific_name:

  Species scientific name
