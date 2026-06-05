# Plots distribution of ages across survey years.

Plots distribution of ages across survey years.

## Usage

``` r
age_frequency(
  data,
  subregions = c("AK ALEUTIANS", "AK BERING", "AK GULF", "NWFSC", "PBS"),
  species,
  by_sex = FALSE,
  cutoff = 0.95,
  facet_all = TRUE
)
```

## Arguments

- data:

  biological data containing age and sex information for at least
  regions specified in `subregions`.

- subregions:

  choose NWFSC, PBS, AK GULF, AK ALEUTIANS, and/or AK BERING. Default
  all.

- species:

  species common or scientific name.

- by_sex:

  TRUE or FALSE for if you want to differentiate by sex.

- cutoff:

  define a cutoff for grouping older ages together.

- facet_all:

  if TRUE this will facet all surveys regardless of missing data, if
  FALSE then only the region(s) with data will be faceted

## Value

a ggplot object
