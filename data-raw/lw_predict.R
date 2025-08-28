### Script to get generate a length-weight prediction estimate table for all top species in each region/survey



##### Length/weight fit function, tibble output #####
lw_fit <- function(data) {
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
lw_fits <- data.frame()

###### #load bio data ####
data(pbs_bio)
data(afsc_bio)
data(nwfsc_bio)
akbsai <- afsc_bio |> filter(survey == "AK BSAI")
akgulf <- afsc_bio |> filter(survey == "AK GULF")
bio_data <- list(pbs = pbs_bio, akbsai = akbsai, akgulf = akgulf, nwfsc = nwfsc_bio) 

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
    
    outputs <- lw_fit(sp_data)
    lw_fits <- bind_rows(lw_fits, outputs) %>% arrange(survey, common) %>% na.omit()
    
  }
  } 
}


##### Length/weight prediction function, tibble output #####

lw_predict <- function(data) {
  
  data <- data %>% 
    filter(!sex == "U", !is.na(length_cm), !is.na(weight_kg))
  
  out_list <- list()
  
  # get regression fit for m and f of species in data
  # Females
  F_pred <- lw_fits %>% 
    filter(survey == unique(data$survey),
           (common == unique(data$common_name) | scientific == unique(data$scientific_name)),
           sex == "F")
  
  if (nrow(F_pred) > 0 && any(data$sex == "F")) { # predict only if there is female data
    F.rg <- range(subset(data, sex == "F")$length_cm, na.rm = TRUE)
    if (all(is.finite(F.rg))) {
      pred_df_F <- data.frame(
        x = seq(floor(F.rg[1]), ceiling(F.rg[2]), by = 1),
        survey = unique(data$survey)
      ) %>%
        mutate(y = F_pred$a * x^F_pred$b)
      
      out_list[["F"]] <- data.frame(
        fit = pred_df_F, sex = "F",
        a = unique(F_pred$a), b = unique(F_pred$b)
      )
    }
  }
  
  # --- MALES ---
  M_pred <- lw_fits %>% 
    filter(survey == unique(data$survey),
           (common == unique(data$common_name) | 
              scientific == unique(data$scientific_name)),
           sex == "M")
  
  if (nrow(M_pred) > 0 && any(data$sex == "M")) {
    M.rg <- range(subset(data, sex == "M")$length_cm, na.rm = TRUE)
    if (all(is.finite(M.rg))) {
      pred_df_M <- data.frame(
        x = seq(floor(M.rg[1]), ceiling(M.rg[2]), by = 1),
        survey = unique(data$survey)
      ) %>%
        mutate(y = M_pred$a * x^M_pred$b)
      
      out_list[["M"]] <- data.frame(
        fit = pred_df_M, sex = "M",
        a = unique(M_pred$a), b = unique(M_pred$b)
      )
    }
  }
  
  # --- combine results ---
  if (length(out_list) == 0) return(data.frame())  # nothing to return
  
  pred.output <- dplyr::bind_rows(out_list) %>%
    mutate(region     = unique(data$region),
           survey     = unique(data$survey),
           common     = unique(data$common_name),
           scientific = unique(data$scientific_name))
  
  return(pred.output)
} 


###### loops ######

# combos of survey and species that have lw fit data
valid_combos <- lw_fits %>%
  distinct(region, survey, scientific, sex)

lw_predictions <- data.frame()

for (region in names(bio_data)) { #for each region
  
  dataset <- bio_data[[region]]
  
  for (survey_name in unique(dataset$survey)) { 
    
    survey_data <- dataset %>%
      filter(survey == survey_name, !sex == "U",
             !is.na(length_cm), !is.na(weight_kg)) # remove unsexed and rows with no l or w data
    
    species_list <- unique(survey_data$scientific_name)
    
    for (species in species_list) { # for each species
      for (sx in c("M","F")) { # and for each sex
      
      # Skip if not in lw_fits (if some combo of spec/reg/sex has no data)
      if (!any(valid_combos$survey == survey_name &
               valid_combos$scientific == species &
               valid_combos$sex == sx)) {
        message("Skipping ", species,"", sx, " (no model fit)")
        next
      }
      
      sp_data <- survey_data %>%
        filter(scientific_name == species, sex == sx)
      
      message("Now processing: ", species, # helps to track errors when computing
              " (survey: ", survey_name, 
              ", region: ", region,
              ", sex: ", sx, ")")
      
      outputs <- lw_predict(sp_data)
      
      lw_predictions <- bind_rows(lw_predictions, outputs) %>%
        arrange(survey, common, sex) %>%
        na.omit()
    }
  }
  }
}


# rename and reformat
lw_predictions <- lw_predictions%>% 
  select(!fit.survey) %>% 
  rename(
         fit_length = fit.x,
         fit_weight = fit.y,
         common_name = common,
         scientific_name = scientific
         )


####### save predictions #######
usethis::use_data(lw_predictions,  overwrite = TRUE)
