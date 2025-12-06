# Plots distribution of ages across survey years.

Plots distribution of ages across survey years.

## Usage

``` r
age_frequency(
  data,
  subregions = c("AK BSAI", "AK GULF", "NWFSC", "PBS"),
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

  choose NWFSC, PBS, AK GULF, and/or AK BSAI. Default all.

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

## Examples

``` r
if (FALSE) { # \dontrun{
data(nwfsc_bio)
data(afsc_bio)
data(pbs_bio)
all_data <- dplyr::bind_rows(nwfsc_bio, afsc_bio, pbs_bio)

age_frequency(data = all_data, 
subregions = c("NWFSC", "AK GULF", "PBS", "AK BSAI"),
 species = "arrowtooth flounder")
age_frequency(data = all_data,
  subregions =  c("NWFSC", "AK GULF"), 
  species = "anoplopoma fimbria", 
  by_sex = T, facet_all = F)
} # }
```
