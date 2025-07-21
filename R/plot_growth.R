#' plot von Bertalanffy function predictions
#'
#' @param data biological data containing age, length, and sex information
#' @return a ggplot object
#' @importFrom dplyr filter
#' @importFrom FSA findGrowthStarts
#' @importFrom stats nls predict
#' @importFrom ggplot2 ggplot aes geom_jitter scale_color_manual geom_line theme_bw labs annotate
#' @export
#'
#' @examples
#' \dontrun{
#' data(vb_predictions)
#' data(pbs_bio)
#' plot_growth(pbs_bio, vb_predictions, "PBS", "arrowtooth flounder")
#' }
plot_growth <- function(data, predictions, region, species) {
  data <- data |>
    filter(!is.na(length_cm)) |>
    filter(!is.na(age_years)) |>
    filter(sex == "F" | sex == "M") |>
    filter(region == region) |>
    filter(common_name == species)
  
  predictions <- predictions |>
    filter(center == region) |>
    filter(common == species)
  
  if (nrow(predictions) == 0) {
    message("No age data available.")
    return(NULL)
  }
  
  linf_m <- predictions |> filter(sex == "M") |> pull(linf) |> unique()
  k_m <- predictions |> filter(sex == "M") |> pull(k) |> unique()
  t0_m <- predictions |> filter(sex == "M") |> pull(t0) |> unique()
  
  linf_f <- predictions |> filter(sex == "F") |> pull(linf) |> unique()
  k_f <- predictions |> filter(sex == "F") |> pull(k) |> unique()
  t0_f <- predictions |> filter(sex == "F") |> pull(t0) |> unique()
  
  ggplot(data = data, aes(x = age_years, y = length_cm, color = sex)) +
    geom_jitter(alpha = 0.1) +
    scale_color_manual(values = c("M" = "#E69F00", "F" = "#009E73")) +
    geom_line(data = predictions, aes(x = age_years, y = fit, color = sex), inherit.aes = FALSE) +
    theme_bw() +
    labs(x = "Age (years)", y = "Length (cm)", title = "Growth") +
    annotate("label", x = Inf, y = -Inf, label = paste0("k = ", k_m, "; t0 = ", t0_m, "; Linf = ", linf_m), 
             color = "#E69F00", vjust= -1.5, hjust = 1.05, fill = "#E69F00", alpha = 0.2) +
    annotate("label", x = Inf, y = -Inf, label = paste0("k = ", k_f, "; t0 = ", t0_f, "; Linf = ", linf_f), 
             color = "#009E73", fill = "#009E73", vjust = -0.5, hjust = 1.05, alpha = 0.2)
}