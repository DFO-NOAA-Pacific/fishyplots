# Function and formatted data to plot standardized design based index

Function and formatted data to plot standardized design based index

## Usage

``` r
plot_stan_dbi(species, surveys)
```

## Arguments

- species:

  common or scientific name of species of interest.

- surveys:

  region or surveys to be plotted. Takes a character list of survey
  names OR a single region ( "NWFSC", "AFSC", or "PBS").

## Value

a ggplot object, plot of standardized design based indicies of 1+
surveys.

## Examples

``` r
if (FALSE) { # \dontrun{
plot_stan_dbi("sablefish", "PBS")
plot_stan_dbi("sablefish", c("U.S. West Coast", "U.S. Gulf of Alaska"))
} # }
```
