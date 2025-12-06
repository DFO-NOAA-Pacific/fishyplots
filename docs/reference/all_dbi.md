# Dataset of design based biomass indices for all science centers

Joined data from design based biomass estimates calculated separately by
each science center

## Usage

``` r
all_dbi
```

## Format

### `all_dbi`

A data frame with 4154 rows and 8 columns:

- region:

  Research center that gathered data

- survey_group:

  Grouping of surveys used for shiny app

- survey:

  survey where data was collected

- common_name:

  Common name of specimen collected

- scientific_name:

  Scientific name of specimen collected

- year:

  Year specimen was collected

- est:

  Estimated design-based biomass

- lwr:

  Lower confidence interval of biomass estimation

- upr:

  Upper confidence interval of biomass estimation
