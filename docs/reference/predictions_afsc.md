# Modeled prediction data for AFSC region

Contains prediction data from spatial models on AFSC surveys for select
species.

## Usage

``` r
predictions_afsc
```

## Format

A data frame with 2809818 rows and 10 variables:

- lon:

  Original longitude coordinate

- lat:

  Original latitude coordinate

- X:

  New UTM coordinate

- Y:

  New UTM coordinate

- prediction:

  Model estimate

- species:

  Species specification

- sanity:

  Check if the model/prediction has any issues

- region:

  Region AFSC

- crs:

  UTM crs used in coordinate transformation

- survey:

  Subregion
