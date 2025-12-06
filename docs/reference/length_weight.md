# Main function to plot length-weight relationships by sex

Main function to plot length-weight relationships by sex

## Usage

``` r
length_weight(
  data,
  subregions = c("AK BSAI", "AK GULF", "PBS", "NWFSC"),
  species,
  subset = TRUE,
  facet_all = TRUE
)
```

## Arguments

- data:

  biological data containing age, length, and sex information for at
  least regions specified in `subregions`.

- subregions:

  choose NWFSC, PBS, AK GULF, and/or AK BSAI. Default all.

- species:

  species common or scientific name.

- subset:

  default TRUE for a faster plotting subset of n = 10000. Set FALSE for
  all available data.

- facet_all:

  if TRUE this will facet all surveys regardless of missing data, if
  FALSE then only the region(s) with data will be faceted.

## Value

a plot of sexed data with log-log regression slope and intercept

## Examples

``` r
if (FALSE) { # \dontrun{
data(pbs_bio)
data(afsc_bio)
data(nwfsc_bio)
all_data <- bind_rows(pbs_bio, afsc_bio, nwfsc_bio)

length_weight(all_data, species = "arrowtooth flounder")
length_weight(all_data, c("NWFSC", "AK GULF"),species = "anoplopoma fimbria", facet_all = F)
} # }
```
