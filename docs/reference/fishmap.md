# Function for modeled CPUE map based on prediction data

Function for modeled CPUE map based on prediction data

## Usage

``` r
fishmap(data, subregion = c("NWFSC", "PBS", "AK BSAI", "AK GULF"), common_name)
```

## Arguments

- data:

  prediction data from fishfit scripts

- subregion:

  choose AK BSAI, AK GULF, PBS, NWFSC

- common_name:

  species common name

## Value

a ggplot object

## Examples

``` r
if (FALSE) { # \dontrun{
data(predictions_afsc)
data(predictions_pbs)
data(predictions_nwfsc)
data <- bind_rows(predictions_afsc, predictions_pbs, predictions_nwfsc)

fishmap(data, c("AK BSAI", "AK GULF", "PBS", "NWFSC"), "arrowtooth flounder")
fishmap(data, "PBS", "dover sole")
} # }
```
