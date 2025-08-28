# Alaska length collection frequency data 
# to be used in survey_table

#local large file: 
#afsc_lengths <- read.csv(local file : ak_akfin_length_composition.csv) 
afsc_haul  <- readRDS("data-raw/afsc-haul.rds")
afsc_specimen  <- readRDS("data-raw/afsc-specimen.rds")
afsc_spp_list <- read.csv("data-raw/afsc_joined.csv")%>% 
  mutate(scientific_name = str_to_sentence(scientific_name))

ak_lengths_year <- afsc_lengths %>%
  rename("event_id" = HAULJOIN, "species_code" = SPECIES_CODE) %>% 
  left_join(afsc_haul %>% select(event_id, survey_name, year), by = "event_id")

species_lookup <- afsc_specimen %>%
  select(species_code, common_name, scientific_name) %>%
  distinct(species_code, .keep_all = TRUE)

ak_survey_lengths <- ak_lengths_year %>%
  left_join(species_lookup, by = "species_code") %>% 
  filter(scientific_name %in% afsc_spp_list$scientific_name) %>% 
  clean_fishnames() %>%
  rename(survey = survey_name) %>% 
  group_survey() %>% 
  select(survey, year, common_name, scientific_name, LENGTH_MM, FREQUENCY) %>% 
  group_by(year, survey, common_name, scientific_name) %>%
  summarise(length.count = sum(FREQUENCY)) %>% 
  #remove early afsc years where arrowtooth and kamchatka flounder were conflated
  filter(!(survey == "AK BSAI" & common_name %in% c("arrowtooth flounder", "kamchatka founder") & year %in% 1982:1991)) %>% 
  filter(!(survey == "AK GULF" & common_name == "arrowtooth flounder" & year %in% 1982:1991)) %>% 
  filter(!(survey == "AK GULF" & common_name == "kamchatka founder" & year %in% 1982:1994))

  usethis::use_data(ak_survey_lengths, overwrite = TRUE)
  