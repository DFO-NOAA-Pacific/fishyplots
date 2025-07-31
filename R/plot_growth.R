#' plot von Bertalanffy function predictions
#'
#' @param data biological data containing age, length, and sex information
#' @param predictions von Bertalanffy prediction data in /data folder
#' @param subregions choose NWFSC, PBS, AK Gulf, and/or AK BSAI
#' @param common species common name 
#' @param facet_all if TRUE this will facet all surveys regardless of missing data, if FALSE then only the region(s) with data will be faceted 
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
#' nwfsc_bio <- nwfsc_bio |> select(-otosag_id)
#' data <- rbind(pbs_bio, afsc_bio, nwfsc_bio)
#' 
#' plot_growth(data, vb_predictions, "AFSC", "walleye pollock")
#' plot_growth(data, vb_predictions, ("AK BSAI", "AK GULF", "NWFSC", "PBS"), "arrowtooth flounder")
#' }
plot_growth <- function(data, predictions, subregions, common, facet_all = TRUE) {
  data <- data |>
    filter(!is.na(length_cm)) |>
    filter(!is.na(age_years)) |>
    filter(sex == "F" | sex == "M") |>
    filter(survey %in% subregions) |>
    filter(common_name == common)
  
  predictions <- predictions |>
    filter(survey %in% subregions) |>
    #rename(region = center) |>
    filter(common_name == common)
  
  data$survey <- factor(data$survey, levels = c("AK BSAI", "AK GULF", "PBS", "NWFSC"),
                        labels = c("Aleutians/Bering Sea", "Gulf of Alaska", "Canada", "U.S. West Coast"))
  predictions$survey <- factor(predictions$survey, levels = c("AK BSAI", "AK GULF", "PBS", "NWFSC"),
                               labels = c("Aleutians/Bering Sea", "Gulf of Alaska", "Canada", "U.S. West Coast"))
  
  # Exit if no data
  if (nrow(predictions) == 0) {
    #message(paste0("No age data available for ", common, " in ", paste(subregions, collapse = ","), "."))
    return(ggplot() + theme_void() + ggtitle("No growth data available."))
  }
  
  linf_m <- (predictions |> filter(sex == "M") |> pull(linf) |> unique())[1]
  k_m <- (predictions |> filter(sex == "M") |> pull(k) |> unique())[1]
  t0_m <- (predictions |> filter(sex == "M") |> pull(t0) |> unique())[1]
  
  linf_f <- (predictions |> filter(sex == "F") |> pull(linf) |> unique())[1]
  k_f <- (predictions |> filter(sex == "F") |> pull(k) |> unique())[1]
  t0_f <- (predictions |> filter(sex == "F") |> pull(t0) |> unique())[1]
  
  label_data <- predictions |>
    group_by(survey, sex) |>
    summarize(linf = sprintf("%.2f", unique(linf)[1]),
              k = sprintf("%.2f", unique(k)[1]),
              t0 = sprintf("%.2f", unique(t0)[1]),
              .groups = "drop") |>
    mutate(vjust = ifelse(sex == "M", -0.5, -1.5)) |>
    mutate(hjust = 1.05) |>
    mutate(sex_label = ifelse(sex == "M", "Male", "Female"))
  
  graph <- ggplot(data = data, aes(x = age_years, y = length_cm, color = sex)) +
    geom_point(alpha = 0.1, show.legend = FALSE) +
    scale_color_manual(values = c("M" = "#E69F00", "F" = "#009E73")) +
    scale_fill_manual(values = c("M" = "#E69F00", "F" = "#009E73")) +
    geom_line(data = predictions, aes(x = age_years, y = fit, color = sex), inherit.aes = FALSE, linewidth = 1, show.legend = FALSE) +
    theme_bw() +
    labs(x = "Age (years)", y = "Length (cm)", title = "Growth") +
    geom_label(
      data = label_data, aes(x = Inf, y = -Inf, color = sex, fill = sex, hjust = hjust, vjust = vjust,
                             label = paste0(sex_label, ": k = ", k, "; t0 = ", t0, "; Linf = ", linf)), 
      alpha = 0.2, inherit.aes = FALSE, show.legend = FALSE, size = 3.3) +
    theme(panel.grid = element_blank())
  
  if (length(subregions) > 1) {
    if (facet_all == TRUE) {
      graph <- graph + facet_wrap(~survey, ncol = 4, drop = FALSE)
    } else if (facet_all == FALSE) {
      graph <- graph + facet_wrap(~survey, ncol = 4)
    }
    
  }
  
  return(graph)
}
