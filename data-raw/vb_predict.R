# Create prediction data frame for von Bertalanffy growth function
library(FSA)
library(dplyr)
library(minpack.lm)

# Combine survey data from all regions
data(pbs_bio)
data(afsc_bio)
data(nwfsc_bio)
akbsai <- afsc_bio |> filter(survey == "AK BSAI")
akgulf <- afsc_bio |> filter(survey == "AK GULF")
bio_data <- list(pbs = pbs_bio, akbsai = akbsai, akgulf = akgulf, nwfsc = nwfsc_bio)

# Function
vb_predict <- function(data) {
  species <- unique(data$common_name)[1]
  survey <- unique(data$survey)[1]
  #browser()
  # Clean data
  data_clean <- data |>
    filter(!is.na(length_cm)) |>
    filter(!is.na(age_years)) |>
    filter(sex == "F" | sex == "M")
  
  if (length(data_clean$age_years) < 20) {
    message(paste0(species, " in ", survey, " -- not enough age data."))
    return(NULL)
  } 
  
  # Define the von Bertalanffy growth function
  vb <- length_cm ~ Linf * (1 - exp(-K * (age_years - t0)))
  
  # Subset data by Sex
  data_male <- subset(data_clean, sex == "M")
  data_female <- subset(data_clean, sex == "F")
  
  # Get starting values for Linf, K, and t0 using FSA package
  starts_male <- try(findGrowthStarts(formula = length_cm ~ age_years, data = data_male), silent = T)
  starts_female <- try(findGrowthStarts(formula = length_cm ~ age_years, data = data_female), silent = T)
  
  # Make fixed starts if needed
  if (inherits(starts_male, "try-error")) {
    message(paste0("Male ", species, " in ", survey, " starts fail, using fallback values."))
    starts_male <- c(Linf = unname(quantile(data_male$length_cm, 0.99, na.rm = TRUE)), K = 0.2, t0 = -1)
  }
  
  if (inherits(starts_female, "try-error")) {
    message(paste0("Female ", species, " in ", survey, " starts fail, using fallback values."))
    starts_female <- c(Linf = unname(quantile(data_female$length_cm, 0.99, na.rm = TRUE)), K = 0.2, t0 = -1)
  }
  
  #findGrowthStarts(length_cm ~ age_years, data = data_male, plot = TRUE) 
  
  # Check t0 isn't too low
  t0_m <- if (!inherits(starts_male, "try-error") && "t0" %in% names(starts_male)) starts_male["t0"] else NA
  t0_f <- if (!inherits(starts_female, "try-error") && "t0" %in% names(starts_female)) starts_female["t0"] else NA
  if (!is.na(t0_m) && t0_m < -6) {
    message(paste0("Male ", species, " in ", survey, " t0 of ", t0_m, " too low. Trying with fixed t0."))
    starts_male["t0"] <- -3
    #return(data.frame())
  }
  if (!is.na(t0_f) && t0_f < -6) {
    message(paste0("Female ", species, " in ", survey, " t0 of ", t0_f, " too low. Trying with fixed t0."))
    starts_female["t0"] <- -3
    #return(data.frame())
  }

  
  # Fit models using the vb and starts
  model_male <- try(nls(vb, data = data_male, start = starts_male), silent = T)
  model_female <- try(nls(vb, data = data_female, start = starts_female), silent = T)
  
  # Debugging models for some species
  if (inherits(model_female, "try-error")) {
    message("nls estimation from stats package failure. Trying minpack.lm.")
    model_female <- try(nlsLM(length_cm ~ Linf * (1 - exp(-K * (age_years - t0))),
                              data = data_female,
                              start = starts_female), silent = TRUE)
  }
  
  # Extract model summary coefficients
  xy <- if (!inherits(model_male, "try-error")) round(coef(model_male), 2) else c(Linf = NA, K = NA, t0 = NA)
  xx <- if (!inherits(model_female, "try-error")) round(coef(model_female), 2) else c(Linf = NA, K = NA, t0 = NA)
  
  # Extract all unique ages
  age_seq_male <- sort(unique(data_male$age_years))
  age_seq_female <- sort(unique(data_female$age_years))
  
  # Create data frames for predictions
  pred_male <- if (!inherits(model_male, "try-error")) {
    data.frame(
      age_years = age_seq_male,
      fit = predict(model_male, newdata = data.frame(age_years = age_seq_male)),
      sex = "M",
      linf = xy[1],
      k = xy[2],
      t0 = xy[3]) 
  }
  else {
    message(paste0("Male ", species, " ", survey, " model did not converge."))
    data.frame()
  }
  
  pred_female <- if (!inherits(model_female, "try-error")) {
    data.frame(
      age_years = age_seq_female,
      fit = predict(model_female, newdata = data.frame(age_years = age_seq_female)),
      sex = "F",
      linf = xx[1],
      k = xx[2],
      t0 = xx[3])
  }
  else {
    message(paste0("Female ", species, " ", survey, " model did not converge."))
    data.frame()
  }
  
  # Combine male and female predictions
  growth_preds <- rbind(pred_male, pred_female)
  
  # Add metadata to the data frame
  growth_preds <- growth_preds |>
    mutate(survey = unique(data$survey)[1]) |>
    mutate(common_name = unique(data$common_name)[1]) |>
    mutate(scientific_name = unique(data$scientific_name)[1])
  
  return(growth_preds)
}


# Create empty prediction data frame
predictions <- data.frame()

# Loop through each region, then each species to apply the prediction function
for (center in names(bio_data)) {
  dataset <- bio_data[[center]]
  # Get unique species in this dataset
  species_list <- unique(dataset$scientific_name)
  # Loop over each species
  for (species in species_list) {
    # Subset data to this species
    sp_data <- dataset |> filter(scientific_name == species)
    outputs <- vb_predict(sp_data)
    
    #outputs <- full_join(lw_predict(sp_data), vb_predict(sp_data), by = c("center","common", "scientific", "sex"))
    predictions <- bind_rows(predictions, outputs)
  }
} 
vb_predictions <- predictions
# Write dataframe
usethis::use_data(vb_predictions, overwrite = TRUE)


#nwfsc_bio1 <- nwfsc_bio |> select(-otosag_id)
#bio_data <- rbind(nwfsc_bio1, afsc_bio, pbs_bio)
#plot_growth(bio_data, vb_predictions, "NWFSC", "walleye pollock")
