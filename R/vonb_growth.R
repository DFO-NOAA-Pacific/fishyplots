#' von Bertalanffy growth function plot
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
#' US West Coast
#' load("data/nwfsc_bio.rda")
#' data <- nwfsc_bio |> filter(common_name == "arrowtooth flounder")
#' vonb_growth(data)
#' 
#' Canada
#' load("data/pbs_bio.rda")
#' data <- pbs_bio |> filter(common_name == "arrowtooth flounder")
#' vonb_growth(data)
#' 
#' Alaska
#' load("data/afsc_bio.rda")
#' data <- afsc_bio |> filter(common_name == "arrowtooth flounder")
#' vonb_growth(data)
#' }
vonb_growth <- function(data) {
  # Clean data
  data_clean <- data |>
    filter(!is.na(length_cm)) |>
    filter(!is.na(age_years)) |>
    filter(sex == "F" | sex == "M")
  
  # Define the von Bertalanffy growth function
  vb <- length_cm ~ Linf * (1 - exp(-K * (age_years - t0)))
  
  # Subset data by sex
  data_male <- subset(data_clean, sex == "M")
  data_female <- subset(data_clean, sex == "F")
  
  # Get starting values for Linf, K, and t0 using FSA package
  starts_male <- findGrowthStarts(formula = length_cm ~ age_years, data = data_male)
  starts_female <- findGrowthStarts(formula = length_cm ~ age_years, data = data_female)
  
  # Fit models using the vb and starts
  model_male <- nls(vb, data = data_male, start = starts_male)
  model_female <- nls(vb, data = data_female, start = starts_female)
  
  xy <- coef(model_male) |> round(2)
  xx <- coef(model_female) |> round(2)
  
  # Extract all unique ages
  age_seq_male <- sort(unique(data_male$age_years))
  age_seq_female <- sort(unique(data_female$age_years))
  
  # Create data frames for predictions
  pred_male <- data.frame(
    age_years = age_seq_male,
    fit = predict(model_male, newdata = data.frame(age_years = age_seq_male)),
    sex = "M")
  
  pred_female <- data.frame(
    age_years = age_seq_female,
    fit = predict(model_female, newdata = data.frame(age_years = age_seq_female)),
    sex = "F")
  
  # Combine predictions
  growth_preds <- rbind(pred_male, pred_female)
  
  # Plot growth function
  ggplot(data = data_clean, aes(x = age_years, y = length_cm, color = sex)) +
    geom_jitter(alpha = 0.1) +
    scale_color_manual(values = c("M" = "#E69F00", "F" = "#009E73")) +
    geom_line(data = growth_preds, aes(x = age_years, y = fit, color = sex), inherit.aes = FALSE) +
    theme_bw() +
    labs(x = "Age (years)", y = "Length (cm)", title = "Growth") +
    annotate("label", x = Inf, y = -Inf, label = paste0("k = ", xx[2], "; Lmin = ", xx[3], "; Linf = ", xx[1]), 
             color = "#009E73", vjust= -1.5, hjust = 1.05, fill = "#009E73", alpha = 0.2) +
    annotate("label", x = Inf, y = -Inf, label = paste0("k = ", xy[2], "; Lmin = ", xy[3], "; Linf = ", xy[1]), 
             color = "#E69F00", fill = "#E69F00", vjust = -0.5, hjust = 1.05, alpha = 0.2)
}