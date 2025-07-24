### Script to get generate a length-weight prediction estimate table for all top species in each region/survey



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
  
  # set default NA
  a_m <- b_m <- a_f <- b_f <- NA
  
  # Run regression only if male data exists
  if (nrow(male) > 1) {
    lw_mod_M <- lm(logweight ~ loglength, data = male)
    a_m <- exp(coef(lw_mod_M)[1])
    b_m <- coef(lw_mod_M)[2]
  }
  
  # Run regression only if female data exists
  if (nrow(female) > 1) {
    lw_mod_F <- lm(logweight ~ loglength, data = female)
    a_f <- exp(coef(lw_mod_F)[1])
    b_f <- coef(lw_mod_F)[2]
  }

  
  #extract parameter estimates into tibble
  lw.tbble <- tibble(
    region = unique(data$region),
    survey = unique(data$survey),
    common = unique(data$common_name),
    scientific = unique(data$scientific_name),
    sex = c("M", "F"),
    a = c(a_m, a_f),
    b = c(b_m, b_f)
  )
  return(lw.tbble)
}

#empty master df
lw_predictions <- data.frame()

#load bio data
data(pbs_bio)
data(afsc_bio)
data(nwfsc_bio)
bio_data <- list(pbs = pbs_bio,
  afsc = afsc_bio,
  nwfsc = nwfsc_bio)

for (region in names(bio_data)) {
  
  dataset <- bio_data[[region]]
  
  for (survey_name in unique(dataset$survey)) {
    
    # Subset data to this survey
    survey_data <- dataset %>% filter(survey == survey_name)
    
    # Get unique species in this survey subset
    species_list <- unique(survey_data$scientific_name)
    
  # Loop over each species
  for (species in species_list) {
    # Subset data to this species
    sp_data <- survey_data %>% filter(scientific_name == species)
    
    outputs <- lw_predict(sp_data)
    lw_predictions <- bind_rows(lw_predictions, outputs) %>% arrange(survey, common) %>% na.omit()
    
  }
  } 
}
# save predictions
usethis::use_data(lw_predictions, overwrite = TRUE)
