### Script to get generate length-weight and growth prediction estimate table for all top species in each region



##### Length/weight function, tibble output #####
lw_predict <- function(data) {
  #remove unsexed and lw NAs, transform
  log_data <- data %>% 
    filter(!sex == "U", !is.na(length_cm), !is.na(weight_kg)) %>%  
    mutate(loglength = log(length_cm), logweight = log(weight_kg)) %>% 
    filter(!is.infinite(logweight), !is.infinite(loglength))
  
  #subset male and female for ease
  male <- subset(log_data, log_data$sex == "M")
  female <- subset(log_data, log_data$sex == "F")

  #regression
  lw_mod_M <- lm(logweight ~ loglength, data = male)
  lw_mod_F <- lm(logweight ~ loglength, data = female)
  
  
  #extract parameter estimates into tibble
  lw.tbble <- tibble(
    center = unique(data$science_center),
    common = unique(data$common_name),
    scientific = unique(data$scientific_name), #see data structure later to name region and species dynamically
    sex = c("M", "F"),
    a = c(if (!inherits(lw_mod_M, "try-error")) exp(coef(lw_mod_M)[1]) else NA,
          if (!inherits(lw_mod_F, "try-error")) exp(coef(lw_mod_F)[1]) else NA),
    b = c(if (!inherits(lw_mod_M, "try-error")) coef(lw_mod_M)[2] else NA,
          if (!inherits(lw_mod_F, "try-error")) coef(lw_mod_F)[2] else NA))

  return(lw.tbble)
}

##### Growth function, tibble output #####
vb_predict <- function(data) {

  # Clean data
  data_clean <- data |>
    filter(!is.na(length_cm)) |>
    filter(!is.na(age_years)) |>
    filter(sex == "F" | sex == "M")

  # Define the von Bertalanffy growth function
  vb <- length_cm ~ Linf * (1 - exp(-K * (age_years - t0)))

  # Subset data by Sex
  data_male <- subset(data_clean, sex == "M")
  data_female <- subset(data_clean, sex == "F")

  # Get starting values for Linf, K, and t0 using FSA package
  starts_male <- try(findGrowthStarts(formula = length_cm ~ age_years, data = data_male), silent = T)
  starts_female <- try(findGrowthStarts(formula = length_cm ~ age_years, data = data_female), silent = T)

  # Fit models using the vb and starts
  model_male <- try(nls(vb, data = data_male, start = starts_male), silent = T)
  model_female <- try(nls(vb, data = data_female, start = starts_female), silent = T)

  xy <- if (!inherits(model_male, "try-error")) round(coef(model_male), 2) else NA
  xx <- if (!inherits(model_female, "try-error")) round(coef(model_female), 2) else NA

  growth.tbble <- tibble(
    center = unique(data$science_center),
    common = unique(data$common_name),
    scientific = unique(data$scientific_name),
    sex = c("M", "F"),
    k = c(xy[2], xx[2]),
    Linf = c(xy[1], xx[1]),
    t0 = c(xy[3], xx[3]))
  return(growth.tbble)
}


##### Join tibbles #####
#empty master df
lw_vb_predictions <- data.frame()

#load bio data
data(pbs_bio)
data(afsc_bio)
data(nwfsc_bio)
nwfsc_bio <-  filter(nwfsc_bio, !scientific_name == "Gadus chalcogrammus") #remove Alaska pollock from nwfsc, only one observation
bio_data <- list(pbs = pbs_bio,
  afsc = afsc_bio,
  nwfsc = nwfsc_bio)

for (center in names(bio_data)) {
  
  dataset <- bio_data[[center]]
  
  # Get unique species in this dataset
  species_list <- unique(dataset$scientific_name)
  
  # Loop over each species
  for (species in species_list) {
    # Subset data to this species
    sp_data <- dataset %>% filter(scientific_name == species)
    
    
    outputs <- full_join(lw_predict(sp_data), vb_predict(sp_data), by = c("center","common", "scientific", "sex"))
    lw_vb_predictions <- bind_rows(lw_vb_predictions, outputs)
    
    
  }
} 
# save predictions
usethis::use_data(lw_vb_predictions)
