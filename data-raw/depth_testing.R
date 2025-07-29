devtools::load_all() #for internals
library(dplyr)
library(janitor)
library(stringr)
library(ggplot2)
library(dplyr)

###### NWFSC data ######
NWFSC_spp_list <- read.csv("data-raw/nwfsc_joined.csv") # top species in NWFSC
nwfsc_survey <- nwfscSurvey::pull_bio(survey = "NWFSC.Combo", common_name = "arrowtooth flounder") #pull bio data for top species

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
  dplyr::select(region, survey, survey_id, fishing_event_id, year, common_name, scientific_name, sex, length_cm, weight_kg, age_years) %>% 
  group_survey()%>% 
  clean_fishnames()

depth_lookup <- do.call(rbind, lapply(pbs_spp_list$common_name,function(spec){
  df <- pbs_data[[spec]]$survey_sets
  return(df)})) |>
  select(fishing_event_id, survey_id, depth_m) |>
  distinct()

pbs_bio <- left_join(pbs_bio, depth_lookup)

#alternative: using tidy_lengths_raw() and tidy_ages_raw() from gfplot package

#save bio datasets 
#usethis::use_data(nwfsc_bio, pbs_bio, afsc_bio, overwrite = TRUE)




test <- afsc_bio |> filter(common_name == "pacific ocean perch")
if (all(is.na(test$depth_m))) {
  stop("No valid depth data available")
}

test <- test |>
  mutate(
    depth_bin = cut(depth_m, breaks = seq(0, max(depth_m), by = 25)),
    age_group = cut(age_years, breaks = seq(0, 90, by = 1))
  )

test2 <- test |>
  mutate(
    depth_bin = cut(depth_m, breaks = seq(0, max(depth_m), by = 50))
  )

age_depth_counts1 <- test |>
  group_by(sex, depth_bin, age_group) |>
  summarise(count = n(), .groups = "drop") |>
  filter(!is.na(age_group)) |>
  group_by(sex, depth_bin) |>
  mutate(
    count = count,
    prop = count / sum(count),
    depth_mid = (as.numeric(sub("\\[|\\(|\\]", "", sub(",.*", "", as.character(depth_bin)))) +
                   as.numeric(sub("\\]|\\)", "", sub(".*,", "", as.character(depth_bin))))) / 2
  ) |>
  ungroup()

age_depth_counts2 <- test2 |>
  group_by(sex, depth_bin, age_years) |>
  summarise(count = n(), .groups = "drop") |>
  filter(!is.na(age_years)) |>
  group_by(sex, depth_bin) |>
  mutate(
    count = count,
    prop = count / sum(count),
    depth_mid = (as.numeric(sub("\\[|\\(|\\]", "", sub(",.*", "", as.character(depth_bin)))) +
                   as.numeric(sub("\\]|\\)", "", sub(".*,", "", as.character(depth_bin))))) / 2
  ) |>
  ungroup()

age_depth_counts1$sex <- factor(age_depth_counts1$sex, levels = c("U", "M", "F"), labels = c("Unsexed", "Male", "Female"))
age_depth_counts2$sex <- factor(age_depth_counts2$sex, levels = c("U", "M", "F"), labels = c("Unsexed", "Male", "Female"))

age_depth_counts1 <- age_depth_counts1 |> filter(is.finite(depth_mid), is.finite(age_group), is.finite(prop))
age_depth_counts2 <- age_depth_counts2 |> filter(is.finite(depth_mid), is.finite(age_years), is.finite(prop))

age_levels_to_show <- levels(age_depth_counts1$age_group)[seq(1, length(levels(age_depth_counts1$age_group)), by = 10)]
#age_depth_counts1$depth_mid <- factor(age_depth_counts1$depth_mid,  levels = sort(unique(age_depth_counts1$depth_mid)))

p1 <- ggplot(age_depth_counts1, aes(x = depth_mid, y = count, fill = age_group)) +
  geom_col(position = "stack", width = 25) +
  facet_wrap(~sex) +
  scale_fill_viridis_d(name = "Age (years)",
                       breaks = age_levels_to_show,
                       labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90"),
                       direction = -1,
                       option = "magma") +
  coord_cartesian(expand = FALSE) +
  labs(x = "Depth (m)", y = "Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(breaks = seq(200, 600, by = 200))


p2 <- ggplot(age_depth_counts2, aes(x = depth_mid, y = age_years, fill = prop)) +
  geom_tile() +
  facet_wrap(~sex) +
  scale_fill_viridis_c(option = "magma", name = "Proportion", direction = -1) +
  coord_cartesian(expand = FALSE) +
  labs(x = "Depth (m)", y = "Age (years)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.grid = element_blank()) +
  coord_flip() +
  scale_x_reverse()
p2

library(patchwork)
p1 + p2 + plot_layout(ncol = 1)
