# Modeled prediction data for NWFSC region

Contains prediction data from spatial models on NWFSC surveys for select
species.

## Usage

``` r
predictions_nwfsc
```

## Format

A data frame with 2463912 rows and 9 variables:

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

  Region NWFSC

- crs:

  UTM crs used in coordinate transformation
