# Function for age-depth plot

Function for age-depth plot

## Usage

``` r
plot_age_depth(
  data,
  subregions = c("NWFSC", "PBS", "AK BSAI", "AK GULF"),
  species,
  by_sex = FALSE,
  facet_all = TRUE
)
```

## Arguments

- data:

  biological data containing age and depth information for at least
  regions specified in `subregions`.

- subregions:

  choose NWFSC, PBS, AK GULF, and/or AK BSAI. Default all.

- species:

  species common or scientific name.

- by_sex:

  show sex differentiation

- facet_all:

  if TRUE this will facet all surveys regardless of missing data, if
  FALSE then only the region(s) with data will be faceted

## Value

a ggplot object

## Examples

``` r
if (FALSE) { # \dontrun{
data(afsc_bio)
data(pbs_bio)
data(nwfsc_bio)
all_data <- bind_rows(afsc_bio, pbs_bio, nwfsc_bio)

plot_age_depth(all_data, species = "arrowtooth flounder")
plot_age_depth(all_data, c("NWFSC", "AK GULF"),
 species = "anoplopoma fimbria",
 by_sex = T, facet_all = F)
} # }
```
