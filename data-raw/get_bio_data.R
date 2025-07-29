# Tidy names and columns for bio data from NWFSC, AFSC, and PBS

devtools::load_all() #for internals
library(dplyr)
library(janitor)
library(stringr)

###### NWFSC data ######
NWFSC_spp_list <- read.csv("data-raw/nwfsc_joined.csv") # top species in NWFSC
nwfsc_survey <- nwfscSurvey::pull_bio(survey = "NWFSC.Combo", common_name = NWFSC_spp_list$common_name) #pull bio data for top species
copy <- nwfsc_survey
# tidy names, select columns of interest
nwfsc_bio <- janitor::clean_names(nwfsc_survey) %>%
  dplyr::mutate(region = "NWFSC", survey = "NWFSC") %>% # add science center
  dplyr::select(region, survey, year, common_name, scientific_name, sex, length_cm, weight_kg, age_years, otosag_id, depth_m) %>%  #otosag_id needed for age structures calculation in survey_table function
  clean_fishnames()
###### AFSC data ######

afsc_haul  <- readRDS("data-raw/afsc-haul.rds")
afsc_specimen  <- readRDS("data-raw/afsc-specimen.rds")
afsc_spp_list <- read.csv("data-raw/afsc_joined.csv")

afsc_years <- data.frame(event_id = afsc_haul$event_id, year = afsc_haul$year, survey = afsc_haul$survey_name, depth_m = afsc_haul$depth_m) #haul IDs and years conducted, and survey

afsc_bio <- dplyr::inner_join(afsc_specimen, afsc_years, by = "event_id") %>% # join years by haul ID
  dplyr::filter(common_name %in% afsc_spp_list$common_name) %>% # keep only species of interest
  dplyr::mutate(region = "AFSC") %>%  # add science center name
  dplyr::mutate("length_cm" = length_mm/10, "weight_kg" = weight_g/1000, sex = convert_sex(sex)) %>% #convert mm -> cm and g -> kg, convert numeric sex
  dplyr::rename(age_years = age) %>% 
  dplyr::select(region, survey, year, common_name, scientific_name, sex, length_cm, weight_kg, age_years, depth_m) %>% 
  group_survey() %>% 
  clean_fishnames()
  
###### PBS data ######

pbs_data <- readRDS("data-raw/pbs-gfdata.rds")
pbs_spp_list <- read.csv("data-raw/pbs_joined.csv")

pbs_bio <- do.call(rbind, lapply(pbs_spp_list$common_name,function(spec){
    df <- pbs_data[[spec]]$survey_samples #pull $survey_samples df from each species
    return(df)}))%>% #apply function to each species data in list, bind together
  dplyr::mutate(region = "PBS") %>%
  dplyr::filter(usability_code == c(0, 1, 2, 6)) %>% #filter for usability codes
  dplyr::mutate("weight_kg" = weight/1000, sex = convert_sex(sex)) %>% #g -> kg, convert sex
  dplyr::rename(length_cm = length, common_name = species_common_name, scientific_name = species_science_name, age_years = age, survey = survey_abbrev) %>% 
  dplyr::select(region, survey, year, common_name, scientific_name, sex, length_cm, weight_kg, age_years, survey_id, fishing_event_id) %>% 
  group_survey()%>% 
  clean_fishnames()

# Adding depth
depth_lookup <- do.call(rbind, lapply(pbs_spp_list$common_name,function(spec){
  df <- pbs_data[[spec]]$survey_sets
  return(df)})) |>
  select(fishing_event_id, survey_id, depth_m) |>
  distinct()
pbs_bio <- left_join(pbs_bio, depth_lookup) |>
  select(-fishing_event_id) |>
  select(-survey_id)

#alternative: using tidy_lengths_raw() and tidy_ages_raw() from gfplot package

#save bio datasets 
usethis::use_data(nwfsc_bio, pbs_bio, afsc_bio, overwrite = TRUE)
