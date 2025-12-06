# Function and formatted data to plot design based index

Function and formatted data to plot design based index

## Usage

``` r
plot_dbi(species, surveys, facet_all = TRUE)
```

## Arguments

- species:

  common or scientific name of species of interest. See examples for
  options per dataset.

- surveys:

  surveys or survey group, required to specify servey/region to be
  plotted. See examples for options per dataset.

- facet_all:

  if TRUE this will facet all surveys regardless of missing data, if
  FALSE then only the region(s) with data will be faceted

## Value

a ggplot object, plot of design based indicies

## Examples

``` r
if (FALSE) { # \dontrun{
# To see options for species and surveys in regions: "NWFSC", "AFSC", "PBS"
data(all_dbi)

all_dbi %>%
filter(region == "AFSC") %>%
 distinct(common_name)

all_dbi %>%
filter(region == "AFSC") %>%
distinct(survey) 


# usage examples
plot_dbi("sablefish", "U.S. West Coast")
plot_dbi("dover sole", "U.S. Gulf of Alaska")
plot_dbi("arrowtooth flounder", "SYN QCS")
} # }
```
