# plot von Bertalanffy function predictions

plot von Bertalanffy function predictions

## Usage

``` r
plot_growth(
  data,
  subregions = c("AK BSAI", "AK GULF", "NWFSC", "PBS"),
  species,
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

  species common or scientific name .

- facet_all:

  if TRUE this will facet all surveys regardless of missing data, if
  FALSE then only the region(s) with data will be faceted.

## Value

a ggplot object

## Examples

``` r
if (FALSE) { # \dontrun{
data(pbs_bio)
data(afsc_bio)
data(nwfsc_bio)
all_data <- bind_rows(pbs_bio, afsc_bio, nwfsc_bio)

plot_growth(all_data, species = "arrowtooth flounder")
plot_growth(all_data, c("NWFSC", "AK GULF"),species = "anoplopoma fimbria", facet_all = F)
} # }
```
