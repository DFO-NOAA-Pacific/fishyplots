# script to get catch occurence counts for top species

library(janitor)
library(stringr)
library(surveyjoin)
#get top species
afsc_top <- read.csv("data-raw/afsc_joined.csv")
pbs_top <- read.csv("data-raw/pbs_joined.csv") 
nwfsc_top <- read.csv("data-raw/nwfsc_joined.csv") %>% # top species in NWFSC
  mutate(scientific_name = str_to_sentence(scientific_name)) #capitalize sci name to match pull_catch requirements

#### NWFSC DATA ####
nwfsc_surveys <- nwfscSurvey::pull_catch(survey = "NWFSC.Combo", sci_name = nwfsc_top$scientific_name) #pull catch data for top species

nwfsc_catch <- nwfsc_surveys %>%
  select(Common_name, Scientific_name, Year, Trawl_id, total_catch_numbers, total_catch_wt_kg) %>%
  group_by(Common_name,Scientific_name, Year) %>% 
  clean_names() %>% 
  summarise(n_tows = n(),                               
            n_pos = sum(total_catch_numbers != 0 | total_catch_wt_kg != 0, na.rm = T), #use either lengths and weights to count as positive
            .groups = "drop") %>% 
  mutate(proportion_pos = n_pos/n_tows, # calculate positive proportion
         region = "NWFSC",
         survey = "NWFSC") %>% 
  group_by(region, survey, common_name, scientific_name) %>% 
  complete(year = 1982:2024, fill = list(n_tows = 0, n_pos = 0, proportion_pos = 0)) %>% 
  fishyplots:::clean_fishnames()
# all species accounted for (more catch data than bio data)

#### PBS DATA ####

pbs_surveys <- get_data(scientific = pbs_top$scientific_name,
                            regions = "pbs")
pbs_catch <- pbs_surveys %>% 
  mutate(survey = "PBS") %>% 
  clean_names() %>% 
  mutate(region = toupper(region)) %>% 
  select(region, survey, common_name, scientific_name, year, event_id, catch_numbers, catch_weight) %>% 
  filter(region == "PBS") %>% 
  group_by(region, survey, common_name,scientific_name, year) %>% 
  summarise(n_tows = n(),                               
            n_pos = sum(catch_numbers != 0| catch_weight != 0, na.rm = TRUE),  
            .groups = "drop") %>% 
  mutate(proportion_pos = n_pos/n_tows) %>% 
  group_by(region, survey, common_name, scientific_name) %>% 
  complete(year = 1982:2024, fill = list(n_tows = 0, n_pos = 0, proportion_pos = 0)) %>% 
  fishyplots:::clean_fishnames() 
#rockfish not in surveyjoin catch database: redstripe, silvergrey, yellowmouth, roughteye/blackspotted complex

#### AFSC DATA (GULF AND BSAI) ####
afsc_surveys <- get_data(scientific = afsc_top$scientific_name,
                        regions = "afsc")
akgulf_surveys <- afsc_surveys %>% 
  filter(survey_name == "Gulf of Alaska") %>% 
  mutate(survey = "AK GULF")
akbsai_surveys <- afsc_surveys %>% 
  filter(survey_name != "Gulf of Alaska") %>% 
  mutate(survey = "AK BSAI")

akgulf_catch <- akgulf_surveys%>% 
  group_by(region, survey, common_name,scientific_name, year) %>% 
  summarise(n_tows = n(),                               
            n_pos = sum(catch_numbers != 0 | catch_weight != 0, na.rm = TRUE),  
            .groups = "drop") %>% 
  mutate(proportion_pos = n_pos/n_tows) %>% 
  group_by(region, survey, common_name, scientific_name) %>% 
  complete(year = 1982:2024, fill = list(n_tows = 0, n_pos = 0, proportion_pos = 0)) %>% 
  fishyplots:::clean_fishnames() %>% 
  #remove early afsc years where arrowtooth and kamchatka flounder were conflated
  filter(!(common_name == "arrowtooth flounder" & year %in% 1982:1991)) %>% # remove species in years with no pos catch data
  filter(!(common_name == "kamchatka flounder" & year %in% 1982:1994))

akbsai_catch <- akbsai_surveys%>% 
  group_by(region, survey, common_name,scientific_name, year) %>% 
  summarise(n_tows = n(),                               
            n_pos = sum(catch_numbers != 0| catch_weight != 0, na.rm = TRUE),  
            .groups = "drop") %>% 
  mutate(proportion_pos = n_pos/n_tows) %>% 
  group_by(region, survey, common_name, scientific_name) %>% 
  complete(year = 1982:2024, fill = list(n_tows = 0, n_pos = 0, proportion_pos = 0)) %>% 
  fishyplots:::clean_fishnames() %>% 
  #remove early afsc years where arrowtooth and kamchatka flounder were conflated
  filter(!(common_name %in% c("arrowtooth flounder", "kamchatka founder") & year %in% 1982:1991)) # remove species in years with no pos catch data (species unidentifiable)
# species catch data not in surveyjoin: Alaska plaice, atka mackerel, hardnose skates, northern rockfish, shortraker rockfish, starry flounder, yellowfin sole


# add all together with row_bind, save
all_catch <- bind_rows(nwfsc_catch, pbs_catch, akbsai_catch, akgulf_catch)
usethis::use_data(all_catch, overwrite = TRUE)
#documentation stored with bio data documentation
