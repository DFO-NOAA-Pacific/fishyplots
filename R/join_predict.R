
# ### Script to get length-weight and growth prediction estimates 
# # made to work with NWFSC data, expand to accept other data formatting later
# 
# 
# ##### Length/weight function, tibble output #####
# lw_predict <- function(data) { 
#   #remove unsexed and lw NAs, transform
#   log_data <- data %>%  
#     filter(!Sex == "U") %>% 
#     filter(!is.na(Length_cm))  %>%
#     filter(!is.na(Weight_kg)) %>% 
#     mutate(loglength = log(Length_cm), logweight = log(Weight_kg))
#   
#   #subset male and female for ease
#   male <- filter(log_data, log_data$Sex %in% c("M", "male", "Male"))
#   female <- filter(log_data, log_data$Sex %in% c("F", "female", "Female"))
#   
#   #regression
#   lw_mod_M <- try(lm(logweight ~ loglength, data = male), silent = T)
#   lw_mod_F <- try(lm(logweight ~ loglength, data = female), silent = T)
#   
#   #extract parameter estimates into tibble
#   lw.tbble <- tibble(
#     species = unique(data$Common_name), #see data structure later to name region and species dynamically
#     sex = c("M", "F"),
#     a = c(if (!inherits(lw_mod_M, "try-error")) exp(coef(lw_mod_M)[1]) else NA,
#           if (!inherits(lw_mod_F, "try-error")) exp(coef(lw_mod_F)[1]) else NA),
#     b = c(if (!inherits(lw_mod_M, "try-error")) coef(lw_mod_M)[2] else NA,
#           if (!inherits(lw_mod_F, "try-error")) coef(lw_mod_F)[2] else NA))
#   
#   return(lw.tbble)
# }
# ##### Growth function, tibble output #####
# vb_predict <- function(data) {
#   
#   # Clean data
#   data_clean <- data |>
#     filter(!is.na(Length_cm)) |>
#     filter(!is.na(Age_years)) |>
#     filter(Sex == "F" | Sex == "M")
#   
#   # Define the von Bertalanffy growth function
#   vb <- Length_cm ~ Linf * (1 - exp(-K * (Age_years - t0)))
#   
#   # Subset data by Sex
#   data_male <- subset(data_clean, Sex == "M")
#   data_female <- subset(data_clean, Sex == "F")
#   
#   # Get starting values for Linf, K, and t0 using FSA package
#   starts_male <- try(findGrowthStarts(formula = Length_cm ~ Age_years, data = data_male), silent = T)
#   starts_female <- try(findGrowthStarts(formula = Length_cm ~ Age_years, data = data_female), silent = T)
#   
#   # Fit models using the vb and starts
#   model_male <- try(nls(vb, data = data_male, start = starts_male), silent = T)
#   model_female <- try(nls(vb, data = data_female, start = starts_female), silent = T)
#   
#   xy <- if (!inherits(model_male, "try-error")) round(coef(model_male), 2) else NA
#   xx <- if (!inherits(model_female, "try-error")) round(coef(model_female), 2) else NA
#   
#   growth.tbble <- tibble( 
#     species = unique(data$Common_name),
#     sex = c("M", "F"),
#     k = c(xy[2], xx[2]),
#     Linf = c(xy[1], xx[1]),
#     t0 = c(xy[3], xx[3]))
#   return(growth.tbble)
# }
# 
# 
# ##### Join tibbles #####
# #empty master df
# nwfsc_master_df <- data.frame()
# 
# # Loop species data thru functions
# for (spec in unique(nwfsc.top.data$Common_name)) {
#   
#   data_spec <- nwfsc.top.data %>% filter(Common_name == spec) # Filter data for given species
#   
#   # Join
#   outputs <- full_join(lw_predict(data_spec), vb_predict(data_spec), by = c("species", "sex"))
#   nwfsc_master_df <- bind_rows(nwfsc_master_df, outputs)
# }
# 
# ###### NOTE: lots of NAs, needs review and editing to take other data formats and check work
