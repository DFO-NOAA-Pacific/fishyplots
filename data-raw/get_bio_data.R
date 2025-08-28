# Tidy names and columns for bio data from NWFSC, AFSC, and PBS

devtools::load_all() #for internals
library(dplyr)
library(janitor)
library(stringr)

###### NWFSC data ######

NWFSC_spp_list <- read.csv("data-raw/nwfsc_joined.csv") %>% # top species in NWFSC
  mutate(scientific_name = str_to_sentence(scientific_name)) #capitalize sci name to match pull_bio requirements
nwfsc_survey <- nwfscSurvey::pull_bio(survey = "NWFSC.Combo", sci_name = NWFSC_spp_list$scientific_name) #pull bio data for top species
# to check if all species went through, compare:
# sort(unique(NWFSC_spp_list$common_name)) (remove n. pacific hake (not in NW) and n. pacific spiny dogfish (same as pac. spiny dogfish))
# sort(unique(nwfsc_survey$Common_name))
# 

# tidy names, select columns of interest
nwfsc_bio <- janitor::clean_names(nwfsc_survey) %>%
  dplyr::mutate(region = "NWFSC", survey = "NWFSC") %>% # add science center
  dplyr::select(region, survey, year, common_name, scientific_name, sex, length_cm, weight_kg, age_years, otosag_id, depth_m) %>%  #otosag_id needed for age structures calculation in survey_table function
  clean_fishnames()
###### AFSC data ######

afsc_haul  <- readRDS("data-raw/afsc-haul.rds")
afsc_specimen  <- readRDS("data-raw/afsc-specimen.rds") # all afsc bio data
afsc_spp_list <- read.csv("data-raw/afsc_joined.csv") %>% 
  mutate(scientific_name = str_to_sentence(scientific_name))

afsc_years <- data.frame(event_id = afsc_haul$event_id, year = afsc_haul$year, survey = afsc_haul$survey_name, depth_m = afsc_haul$depth_m) #haul IDs and years conducted, and survey

afsc_bio <- dplyr::inner_join(afsc_specimen, afsc_years, by = "event_id") %>% # join years by haul ID
  dplyr::filter(scientific_name %in% afsc_spp_list$scientific_name) %>% # keep only species of interest
  dplyr::mutate(region = "AFSC") %>%  # add science center name
  dplyr::mutate("length_cm" = length_mm/10, "weight_kg" = weight_g/1000, sex = convert_sex(sex)) %>% #convert mm -> cm and g -> kg, convert numeric sex
  dplyr::rename(age_years = age) %>% 
  dplyr::select(region, survey, year, common_name, scientific_name, sex, length_cm, weight_kg, age_years, depth_m) %>% 
  group_survey() %>% 
  clean_fishnames() %>% 
  #remove early afsc years where arrowtooth and kamchatka flounder were conflated
  filter(!(survey == "AK BSAI" & common_name %in% c("arrowtooth flounder", "kamchatka founder") & year %in% 1982:1991)) %>% 
  filter(!(survey == "AK GULF" & common_name == "arrowtooth flounder" & year %in% 1982:1991)) %>% 
  filter(!(survey == "AK GULF" & common_name == "kamchatka founder" & year %in% 1982:1994))
  
###### PBS data ######

pbs_data <- readRDS("data-raw/pbs-gfdata.rds")
pbs_spp_list <- read.csv("data-raw/pbs_joined.csv")

pbs_bio <- do.call(rbind, lapply(pbs_spp_list$common_name,function(spec){ #have to use common name
    df <- pbs_data[[spec]]$survey_samples #pull $survey_samples df from each species
    return(df)}))%>% #apply function to each species data in list, bind together
  dplyr::mutate(region = "PBS") %>%
  dplyr::filter(usability_code %in% c(0, 1, 2, 6)) %>% #filter for usability codes
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
