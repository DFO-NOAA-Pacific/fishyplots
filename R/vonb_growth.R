#' von Bertalanffy growth function plot
#'
#' @param data catch data from pull_bio(), containing Length_cm, Age_years, and Sex
#' @return a ggplot object
#' @importFrom dplyr filter
#' @importFrom FSA findGrowthStarts
#' @importFrom stats nls predict
#' @importFrom ggplot2 ggplot aes geom_jitter scale_color_manual geom_line theme_bw labs annotate
#' @export
#'
#' @examples
#' \dontrun{
#' vonb_growth(bio_data)
#' }


vonb_growth <- function(data) {
  # Clean data
  data_clean <- data |>
    filter(!is.na(Length_cm)) |>
    filter(!is.na(Age_years)) |>
    filter(Sex == "F" | Sex == "M")
  
  # Define the von Bertalanffy growth function
  vb <- Length_cm ~ Linf * (1 - exp(-K * (Age_years - t0)))
  
  # Subset data by Sex
  data_male <- subset(data_clean, Sex == "M")
  data_female <- subset(data_clean, Sex == "F")
  
  # Get starting values for Linf, K, and t0 using FSA package
  starts_male <- findGrowthStarts(formula = Length_cm ~ Age_years, data = data_male)
  starts_female <- findGrowthStarts(formula = Length_cm ~ Age_years, data = data_female)
  
  # Fit models using the vb and starts
  model_male <- nls(vb, data = data_male, start = starts_male)
  model_female <- nls(vb, data = data_female, start = starts_female)
  
  xy <- coef(model_male) |> round(2)
  xx <- coef(model_female) |> round(2)
  
  # Extract all unique ages
  age_seq_male <- sort(unique(data_male$Age_years))
  age_seq_female <- sort(unique(data_female$Age_years))
  
  # Create data frames for predictions
  pred_male <- data.frame(
    Age_years = age_seq_male,
    fit = predict(model_male, newdata = data.frame(Age_years = age_seq_male)),
    Sex = "M")
  
  pred_female <- data.frame(
    Age_years = age_seq_female,
    fit = predict(model_female, newdata = data.frame(Age_years = age_seq_female)),
    Sex = "F")
  
  # Combine predictions
  growth_preds <- rbind(pred_male, pred_female)
  
  # Plot growth function
  ggplot(data = data_clean, aes(x = Age_years, y = Length_cm, color = Sex)) +
    geom_jitter(alpha = 0.1) +
    scale_color_manual(values = c("M" = "#008b8b", "F" = "#daa520")) +
    geom_line(data = growth_preds, aes(x = Age_years, y = fit, color = Sex), inherit.aes = FALSE) +
    theme_bw() +
    labs(x = "Age (years)", y = "Length (cm)", title = "Growth") +
    annotate("label", x = Inf, y = -Inf, label = paste0("k = ", xx[2], "; Lmin = ", xx[3], "; Linf = ", xx[1]), 
             color = "#daa520", vjust= -1.5, hjust = 1.05, fill = "#daa520", alpha = 0.2) +
    annotate("label", x = Inf, y = -Inf, label = paste0("k = ", xy[2], "; Lmin = ", xy[3], "; Linf = ", xy[1]), 
             color = "#008b8b", fill = "#008b8b", vjust = -0.5, hjust = 1.05, alpha = 0.2)
}
