# join design based indices data for each center to be used in plotting functions
   

#pull data and reformat: 
load(here::here("data-raw", "nwfsc_biomass.rda")) # nwfsc index data: object nwfsc_biomass
nwfsc_biomass <- nwfsc_biomass %>% 
  select(!area)

load(here::here("data-raw", "afsc_biomass.rda")) # afsc index data: object afsc_biomass
afsc_biomass <- afsc_biomass %>% 
  select(!area) %>% 
  filter(!region == "U.S. Eastern Bering Sea Standard Region")

pbs_biomass <- readRDS(here::here("data-raw", "pbs-gfdata.rds")) #read in pbs index data
pbs_biomass <- do.call(rbind, lapply(names(pbs_biomass),function(spec){
  df <- pbs_biomass[[spec]]$survey_index #pull $survey_index df from each species list
  return(df)}))%>% #apply function to each species data in list, bind together
  mutate(science_center = "PBS") %>% 
  select(science_center, survey_abbrev, species_common_name, species_science_name, year, biomass, lowerci, upperci) %>%
  rename(common_name = species_common_name,
          region = survey_abbrev, 
          scientific_name = species_science_name ,
          est = biomass,
          lwr = lowerci,
          upr = upperci)#rename to have same format as NOAA data : object pbs_biomass

#join
all.dbi <- rbind(pbs_biomass, afsc_biomass, nwfsc_biomass)

usethis::use_data(all.dbi, overwrite = TRUE)
