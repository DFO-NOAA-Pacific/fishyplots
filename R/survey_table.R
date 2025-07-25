#' Main function to display survey specimen counts
#'
#' @param data a data frame of bio data
#' @param species species of interest common or scientific name 
#' @param form choose 1 for tibble or 2 for ggplot
#' @return a tibble or ggplot object
#' @importFrom dplyr group_by summarize mutate arrange bind_rows
#' @importFrom gfplot theme_pbs
#' @importFrom tidyr pivot_wider
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_distiller theme_minimal theme element_blank element_text ggtitle
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' \dontrun{
#' data("nwfsc_bio")
#' data("pbs_bio")
#' data("afsc_bio")
#' akgulf_bio <- afsc_bio %>% filter(survey == "AK GULF")
#' akbsai_bio <- afsc_bio %>% filter(survey == "AK BSAI")
#' all.data <- bind_rows(akgulf_bio, akbsai_bio, nwfsc_bio, pbs_bio)
#' 
#' 
#' survey_table(akgulf_bio, "atka mackerel", form = 1)
#' survey_table(nwfsc_bio, "sebastes zacentrus", form = 2)
#' survey_table(pbs_bio,"arrowtooth flounder", form = 2)
#' }


survey_table <- function(data, species, form = c(1,2)) {
  
  
  #filter species
  if (any(data$common_name == species | data$scientific_name == species)) {
  spec.data <- filter(data, common_name == species | scientific_name == species)}
  else (stop(paste("Species name", "'", species,"'", "not found in ", "data.")))
  
  
  #make subsets with just the wanted data (l/w/a and year)
  #number of lengths counted per year
  length_count <- data.frame(length = spec.data$length_cm, yr = spec.data$year, survey = spec.data$survey) %>%
    na.omit() %>% #omit rows with no data from being counted
    group_by(yr, survey) %>%
    summarize(n_samples=n())%>% #sum sample number over grouping (year)
    mutate(sample_type = "Length") #label as length count
  
  if(any(spec.data$region == "AFSC")){ #special dataset for AFSC lengths, overwrite previous lengths for AFSC surveys
    data("ak_survey_lengths")
    ak_length_count <-  ak_survey_lengths %>% 
      group_by(year, survey) %>%
      summarize(n_samples=sum(length.count))%>%
      mutate(sample_type = "Length") %>% 
      rename(yr = year) %>% 
      na.omit()
    
    #which afsc surveys are being called
    which.asfc<- unique(spec.data$survey[spec.data$region == "AFSC"])
    
    # Only replace those surveys
    length_count <- length_count %>%
      filter(!(survey %in% which.asfc)) %>%
      bind_rows(ak_length_count %>% filter(survey %in% which.asfc))
  }

    
#weight counts
weight_count <- data.frame(weight = spec.data$weight_kg, yr = spec.data$year, survey = spec.data$survey) %>%
  na.omit() %>% 
  group_by(yr, survey) %>%
  summarize(n_samples=n())%>%
  mutate(sample_type = "Weight")

#age counts, fill with 0 if no ages taken (ex nwfsc Sebastes zacentrus)
age_count <- data.frame(age = spec.data$age_years, yr = spec.data$year, survey = spec.data$survey) %>%
  na.omit() %>% 
  group_by(yr, survey) %>%
  summarize(n_samples=n())%>%
  mutate(sample_type = "Age")
if(nrow(age_count) == 0) {
  age_count <- data.frame(survey = unique(spec.data$survey), n_samples = 0, yr = weight_count$yr, sample_type = "Age")}

# unread age structures: nwfsc has otolith column for this, otherwise use age=NA as unread
agestr_count <-  data.frame(age = spec.data$age_years, yr = spec.data$year, survey = spec.data$survey) %>% 
  group_by(yr, survey) %>%
  summarize(n_samples=n())%>%
  mutate(sample_type = "Age Structures") 

if ("otosag_id" %in% names(data)) { #overwrite if dealing with nwfsc data
nwfsc_count <- spec.data %>%
  filter(region == "NWFSC", !is.na(otosag_id)) %>%
  group_by(survey, yr = year) %>%
  summarize(n_samples = n(), .groups = "drop") %>%
  mutate(sample_type = "Age Structures (otosag_id)")

# Count for other regions using age_years
# other_count <- spec.data %>%
#   filter(region != "NWFSC", !is.na(age_years)) %>%
#   group_by(survey, yr = year) %>%
#   summarize(n_samples = n(), .groups = "drop") %>%
#   mutate(sample_type = "Age Structures (age_years)")
other_count <-  data.frame(age = spec.data$age_years, yr = spec.data$year, survey = spec.data$survey) %>% 
  group_by(yr, survey) %>%
  summarize(n_samples=n())%>%
  mutate(sample_type = "Age Structures") %>% 
  filter(survey != "NWFSC")


# Combine both into agestr_count
agestr_count <- bind_rows(nwfsc_count, other_count)
}

unread_count <- left_join(agestr_count, age_count, by = c("yr", "survey")) %>%
  mutate(n_ages = ifelse(is.na(n_samples.y), 0, n_samples.y), #change NA to 0 for calculation ease
         n_unread = n_samples.x - n_ages, #unread age strugtures = n age str - n ages read
         n_samples = ifelse(n_unread == 0, NA, n_unread)) %>% # make 0 into NA, use common name
  select(survey, yr, n_samples) %>%
  mutate(sample_type = "Unread Age Structures") %>% #unread structures only
  na.omit() #omit NAs where n age structures = n read ages


# Combine data into one DF
bio_data <-length_count %>%
  bind_rows(weight_count) %>%
  bind_rows(age_count) %>%
  bind_rows(unread_count) %>% 
  arrange(yr) %>%
  mutate(sample_type=factor(sample_type, levels=c("Unread Age Structures", "Age", "Weight", "Length"))) %>% #set order for plotting later
  mutate(common = unique(spec.data$common_name))


# output table format if argument form = 1
if (form == 1) {
  # Return wide table
  table <- bio_data %>%
    pivot_wider(
      names_from = sample_type,
      values_from = n_samples
    ) %>% 
    select(common, everything())
  return(table)
}

# output graph format if argument form = 2
if (form == 2) {

# Convert count to label with "K" for thousands
bio_data$label <- ifelse(bio_data$n_samples < 100,
                              as.character(bio_data$n_samples), # keep as number if under 100
                              ifelse(bio_data$n_samples < 950, #
                                     as.character(round(bio_data$n_samples, digits = -2)),  # round to nearest hundred if under 950 (above will round to 1000 -> 1K)
                                     paste0(round(bio_data$n_samples / 1000), "K") )) # if over 1000, round to nearest and label with k

#create plot
plot <- ggplot(bio_data, aes(yr, sample_type)) +
  ggplot2::geom_tile(aes(fill = n_samples, alpha = n_samples != 0), colour = "white", show.legend = FALSE) + # to preserve rows where all years have 0 data, color 0 tiles white
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0)) +
  scale_fill_gradientn(
    colours = c("#ffffb2", "#fecc5c", "#fd8d3c", "#e31a1c", "red4"),
    trans = "log10",
    na.value = "white"
  ) + 
  theme_pbs() +
  theme(
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  ggplot2::guides(fill = "none") + xlab("") + ylab("") +
  geom_text(aes(label = label),
            colour = "black", 
            size = 3, alpha = 1
  ) +
  ggplot2::scale_y_discrete(position = "left")+
  scale_x_continuous(breaks = seq(min(bio_data$yr)+1, max(bio_data$yr), by = 2))+ # keep year labels on even years
  ggplot2::ggtitle(paste("Survey Specimen Counts -", unique(spec.data$common_name))) +
  coord_cartesian(expand = FALSE) +
  facet_wrap(~ survey, ncol = 1)

return(plot)
}
}


