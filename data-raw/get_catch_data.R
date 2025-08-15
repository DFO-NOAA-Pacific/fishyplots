# script to get catch occurence counts for top species

library(janitor)
library(stringr)
library(surveyjoin)
#get top species
afsc_top <- read.csv("data-raw/afsc_joined.csv")
pbs_top <- read.csv("data-raw/pbs_joined.csv")
nwfsc_top <- read.csv("data-raw/nwfsc_joined.csv")

#### NWFSC DATA ####
nwfsc_surveys <- nwfscSurvey::pull_catch(survey = "NWFSC.Combo", common_name = nwfsc_top$common_name) #pull catch data for top species

nwfsc_catch <- nwfsc_surveys %>%
  select(Common_name, Scientific_name, Year, Trawl_id, total_catch_numbers) %>%
  group_by(Common_name,Scientific_name, Year) %>% 
  clean_names() %>% 
  summarise(n_tows = n(),                               
            n_pos = sum(total_catch_numbers != 0),  
            .groups = "drop") %>% 
  mutate(proportion_pos = n_pos/n_tows,
         region = "NWFSC",
         survey = "NWFSC") %>% 
  group_by(region, survey, common_name, scientific_name) %>% 
  complete(year = 1982:2024, fill = list(n_tows = 0, n_pos = 0, proportion_pos = 0)) %>% 
  fishyplots:::clean_fishnames()

nwfsc_catch2 <- nwfsc_surveys %>%
  select(Common_name, Scientific_name, Year, Trawl_id, total_catch_numbers, total_catch_wt_kg) %>%
  group_by(Common_name,Scientific_name, Year) %>% 
  clean_names() %>% 
  summarise(n_tows = n(),                               
            n_pos = sum(total_catch_numbers != 0 | total_catch_wt_kg != 0, na.rm = T),
            .groups = "drop") %>% 
  mutate(proportion_pos = n_pos/n_tows,
         region = "NWFSC",
         survey = "NWFSC") %>% 
  group_by(region, survey, common_name, scientific_name) %>% 
  complete(year = 1982:2024, fill = list(n_tows = 0, n_pos = 0, proportion_pos = 0)) %>% 
  fishyplots:::clean_fishnames()

#### PBS DATA ####

pbs_surveys <- get_data(common = pbs_top$common_name,
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


#### AFSC DATA (GULF AND BSAI) ####
afsc_surveys <- get_data(common = afsc_top$common_name,
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
  fishyplots:::clean_fishnames() 

akbsai_catch <- akbsai_surveys%>% 
  group_by(region, survey, common_name,scientific_name, year) %>% 
  summarise(n_tows = n(),                               
            n_pos = sum(catch_numbers != 0| catch_weight != 0, na.rm = TRUE),  
            .groups = "drop") %>% 
  mutate(proportion_pos = n_pos/n_tows) %>% 
  group_by(region, survey, common_name, scientific_name) %>% 
  complete(year = 1982:2024, fill = list(n_tows = 0, n_pos = 0, proportion_pos = 0)) %>% 
  fishyplots:::clean_fishnames() 



# add all together with row_bind, save
all_catch <- bind_rows(nwfsc_catch, pbs_catch, akbsai_catch, akgulf_catch)
usethis::use_data(all_catch, overwrite = TRUE)
#documentation stored with bio data documentation
