# Tidy names and columns for bio data from NWFSC, AFSC, and PBS

devtools::load_all()

###### NWFSC data ######
NWFSC_spp_list <- read.csv("data-raw/nwfsc_joined.csv") # top species in NWFSC
nwfsc_bio <- pull_bio(survey = "NWFSC.Combo", common_name = NWFSC_spp_list$common_name) #pull bio data for top species

# tidy names, select columns of interest
nwfsc_bio_clean <- janitor::clean_names(nwfsc_bio) %>%
  dplyr::mutate(science_center = "NWFSC") %>% # add science center
  dplyr::select(science_center, year, common_name, scientific_name, sex, length_cm, weight_kg, age_years, otosag_id) #otosag_id needed for age structures calculation in survey_table function

###### AFSC data ######

afsc_haul  <- readRDS("data-raw/afsc-haul.rds")
afsc_specimen  <- readRDS("data-raw/afsc-specimen.rds")
afsc_spp_list <- read.csv("data-raw/afsc_joined.csv")

afsc_years <- data.frame(event_id = afsc_haul$event_id, year = afsc_haul$year) #haul IDs and years conducted

afsc_bio_clean <- dplyr::inner_join(afsc_specimen, afsc_years, by = "event_id") %>% # join years by haul ID
  dplyr::filter(common_name %in% afsc_spp_list$common_name) %>% # keep only species of interest
  dplyr::mutate(science_center = "AFSC") %>%  # add science center name
  dplyr::mutate("length_cm" = length_mm/10, "weight_kg" = weight_g/1000, sex = convert_sex(sex)) %>% #convert mm -> cm and g -> kg, convert numeric sex
  dplyr::rename(age_years = age) %>% 
  dplyr::select(science_center, year, common_name, scientific_name, sex, length_cm, weight_kg, age_years)
  
###### PBS data ######

pbs_data <- readRDS("data-raw/pbs_gfdata.rds")
pbs_spp_list <- read.csv("data-raw/pbs_joined.csv")

pbs_bio_clean <- do.call(rbind, lapply(pbs_spp_list$common_name,function(spec){
    df <- pbs_data[[spec]]$survey_samples #pull $survey_samples df from each species
    return(df)}))%>% #apply function to each species data in list, bind together
  dplyr::mutate(science_center = "PBS") %>%
  dplyr::filter(usability_code == c(0, 1, 2, 6)) %>% #filter for usability codes
  dplyr::mutate("weight_kg" = weight/1000, sex = convert_sex(sex)) %>% #g -> kg, convert sex
  dplyr::rename(length_cm = length, common_name = species_common_name, scientific_name = species_science_name, age_years = age) %>% 
  dplyr::select(science_center, year, common_name, scientific_name, sex, length_cm, weight_kg, age_years)

#alternative: using tidy_lengths_raw() and tidy_ages_raw() from gfplot package


