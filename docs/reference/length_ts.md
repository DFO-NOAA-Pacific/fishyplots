# Main function to plot mean lengths over time by sex

Main function to plot mean lengths over time by sex

## Usage

``` r
length_ts(
  data,
  subregions = c("AK BSAI", "AK GULF", "PBS", "NWFSC"),
  species,
  facet_all = TRUE
)
```

## Arguments

- data:

  biological data containing length information for at least regions
  specified in `subregions`.

- subregions:

  choose NWFSC, PBS, AK GULF, and/or AK BSAI. Default all.

- species:

  species common or scientific name.

- facet_all:

  if TRUE this will facet all surveys regardless of missing data, if
  FALSE then only the region(s) with data will be faceted.#' @return a
  time series plot of mean lengths by sex

## Examples

``` r
if (FALSE) { # \dontrun{
data(pbs_bio)
data(afsc_bio)
data(nwfsc_bio)
all_data <- bind_rows(pbs_bio, afsc_bio, nwfsc_bio)

length_ts(all_data, species = "arrowtooth flounder")
length_ts(all_data, c("NWFSC", "AK GULF"),species = "anoplopoma fimbria", facet_all = F)
} # }
```
