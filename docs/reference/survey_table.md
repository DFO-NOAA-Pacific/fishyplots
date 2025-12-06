# Main function to display survey specimen counts

Main function to display survey specimen counts

## Usage

``` r
survey_table(
  data,
  subregions = c("AK BSAI", "AK GULF", "NWFSC", "PBS"),
  species,
  form = 2,
  facet_all = TRUE
)
```

## Arguments

- data:

  biological data containing length and depth information for at least
  regions specified in `subregions`.

- subregions:

  choose NWFSC, PBS, AK GULF, and/or AK BSAI. Default all.

- species:

  species common or scientific name.

- form:

  choose 1 for tibble or 2 for ggplot

- facet_all:

  if TRUE this will facet all surveys regardless of missing data, if
  FALSE then only the region(s) with data will be faceted

## Value

a tibble or ggplot object

## Examples

``` r
if (FALSE) { # \dontrun{
data(nwfsc_bio)
data(afsc_bio)
data(pbs_bio)
all_data <- rbind(nwfsc_bio, afsc_bio, pbs_bio)

survey_table(all_data, species = "arrowtooth flounder", form = 1)
survey_table(all_data, species = "arrowtooth flounder", form = 2)
survey_table(all_data, c("NWFSC", "AK GULF"), species = "anoplopoma fimbria", facet_all = F)

} # }
```
