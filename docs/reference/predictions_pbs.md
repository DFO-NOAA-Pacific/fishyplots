# Modeled prediction data for PBS region

Contains prediction data from spatial models on NWFSC surveys for select
species.

## Usage

``` r
predictions_pbs
```

## Format

A data frame with 321480 rows and 9 variables:

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

  Region PBS

- crs:

  UTM crs used in coordinate transformation
