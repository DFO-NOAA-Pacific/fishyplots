# Function to plot length frequency of fish species

Function to plot length frequency of fish species

## Usage

``` r
length_frequency(
  data,
  subregions = c("AK BSAI", "AK GULF", "NWFSC", "PBS"),
  species,
  time_series = TRUE,
  facet_all = TRUE
)
```

## Arguments

- data:

  biological data containing age and length information for at least
  regions specified in `subregions`.

- subregions:

  choose NWFSC, PBS, AK GULF, and/or AK BSAI. Default all.

- species:

  species common or scientific name.

- time_series:

  TRUE or FALSE

- facet_all:

  if TRUE this will facet all surveys regardless of missing data, if
  FALSE then only the region(s) with data will be faceted

## Value

a ggplot object

## Examples

``` r
if (FALSE) { # \dontrun{
data(nwfsc_bio)
data(afsc_bio)
data(pbs_bio)
all_data <- bind_rows(nwfsc_bio, afsc_bio, pbs_bio)

length_frequency(all_data, species = "arrowtooth flounder")
length_frequency(all_data, c("NWFSC", "AK GULF"),species = "anoplopoma fimbria", facet_all = F)
} # }
```
