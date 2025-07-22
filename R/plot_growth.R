#' plot von Bertalanffy function predictions
#'
#' @param data biological data containing age, length, and sex information
#' @param predictions von Bertalanffy prediction data in /data folder
#' @param region region AFSC, NWFSC, and/or PBS
#' @param species species common name 
#' @return a ggplot object
#' @importFrom dplyr filter rename pull mutate summarize group_by
#' @importFrom ggsidekick theme_sleek
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual scale_fill_manual geom_line labs geom_label
#' @export
#'
#' @examples
#' \dontrun{
#' data(vb_predictions)
#' data(pbs_bio)
#' data(afsc_bio)
#' data(nwfsc_bio)
#' data <- rbind(pbs_bio, afsc_bio, nwfsc_bio)
#' 
#' plot_growth(data, vb_predictions, "AFSC", "walleye pollock")
#' plot_growth(data, vb_predictions, region = c("AFSC", "NWFSC", "PBS"), "arrowtooth flounder")
#' }
plot_growth <- function(data, predictions, region, species) {
  data <- data |>
    filter(!is.na(length_cm)) |>
    filter(!is.na(age_years)) |>
    filter(sex == "F" | sex == "M") |>
    filter(science_center %in% region) |>
    rename(center = science_center) |>
    filter(common_name == species)
  
  predictions <- predictions |>
    filter(center %in% region) |>
    filter(common == species)
  
  data$center <- factor(data$center, levels = c("AFSC", "PBS", "NWFSC"),
                        labels = c("Alaska", "Canada", "U.S. West Coast"))
  predictions$center <- factor(predictions$center, levels = c("AFSC", "PBS", "NWFSC"),
                               labels = c("Alaska", "Canada", "U.S. West Coast"))
  
  if (nrow(predictions) == 0) {
    message("No age data available.")
    return(NULL)
  }
  
  linf_m <- (predictions |> filter(sex == "M") |> pull(linf) |> unique())[1]
  k_m <- (predictions |> filter(sex == "M") |> pull(k) |> unique())[1]
  t0_m <- (predictions |> filter(sex == "M") |> pull(t0) |> unique())[1]
  
  linf_f <- (predictions |> filter(sex == "F") |> pull(linf) |> unique())[1]
  k_f <- (predictions |> filter(sex == "F") |> pull(k) |> unique())[1]
  t0_f <- (predictions |> filter(sex == "F") |> pull(t0) |> unique())[1]
  
  label_data <- predictions |>
    group_by(center, sex) |>
    summarize(linf = round(unique(linf)[1], 2),
              k = round(unique(k)[1], 2),
              t0 = round(unique(t0)[1], 2),
              .groups = "drop") |>
    mutate(vjust = ifelse(sex == "M", -0.5, -1.5)) |>
    mutate(hjust = 1.05) |>
    mutate(sex_label = ifelse(sex == "M", "Male", "Female"))
  
  graph <- ggplot(data = data, aes(x = age_years, y = length_cm, color = sex)) +
    geom_point(alpha = 0.1, show.legend = FALSE) +
    scale_color_manual(values = c("M" = "#E69F00", "F" = "#009E73")) +
    scale_fill_manual(values = c("M" = "#E69F00", "F" = "#009E73")) +
    geom_line(data = predictions, aes(x = age_years, y = fit, color = sex), inherit.aes = FALSE, linewidth = 1, show.legend = FALSE) +
    theme_sleek() +
    labs(x = "Age (years)", y = "Length (cm)", title = "Growth") +
    geom_label(
      data = label_data, aes(x = Inf, y = -Inf, color = sex, fill = sex, hjust = hjust, vjust = vjust,
                             label = paste0(sex_label, ": k = ", k, "; t0 = ", t0, "; Linf = ", linf)), 
      alpha = 0.2, inherit.aes = FALSE, show.legend = FALSE, size = 3.3)
  
  if (length(region) > 1) {
    graph <- graph +
      facet_wrap(~center)
  }
  
  return(graph)
}