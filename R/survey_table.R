#' Main function to display survey specimen counts
#'
#' @param data a data frame of target species including Length_cm, Weight_kg, Age_years, Otosag_id, and Year.
#' @param form choose 1 for tibble or 2 for ggplot
#' @return a tibble or ggplot object
#' @importFrom dplyr group_by summarize mutate arrange bind_rows
#' @importFrom gfplots theme_pbs
#' @importFrom tidyr pivot_wider
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_distiller theme_minimal theme element_blank element_text ggtitle
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' \dontrun{
#' survey_table(bio.data, form = 2)
#' }


survey_table <- function(data, form = c(1,2)) {
  
  #makes subsets with just the wanted data (l/w/a and year)
length_count <- data.frame(length_cm = data$Length_cm, year = data$Year) %>%
  na.omit() %>% #omit rows with no data from being counted
  group_by(year) %>%
  summarize(n_samples=n())%>% #sum sample number over grouping (year)
  mutate(sample_type = "Length") #label as length count

weight_count <- data.frame(weight_cm = data$Weight_kg, year = data$Year) %>%
  na.omit() %>% 
  group_by(year) %>%
  summarize(n_samples=n())%>%
  mutate(sample_type = "Weight")

age_count <- data.frame(age_yr = data$Age_years, year = data$Year) %>%
  na.omit() %>% 
  group_by(year) %>%
  summarize(n_samples=n())%>%
  mutate(sample_type = "Age")

agestr_count <- data.frame(otosag = data$Otosag_id, year = data$Year) %>% 
  na.omit() %>% 
  group_by(year) %>%
  summarize(n_samples=n())%>%
  mutate(sample_type = "Age Structures") #all structures collected

unread_count <- left_join(agestr_count, age_count, by = "year") %>%
  mutate(n_ages_read = ifelse(is.na(n_samples.y), 0, n_samples.y),
    n_samples = n_samples.x - n_ages_read
  ) %>%
  select(year, n_samples) %>%
  mutate(sample_type = "Unread Age Structures") #unread structures only

# put all into one df
bio_data <-length_count %>%
  bind_rows(weight_count) %>%
  bind_rows(age_count) %>%
  bind_rows(unread_count) %>% 
  arrange(year) %>%
  mutate(sample_type=factor(sample_type, levels=c("Unread Age Structures", "Age", "Weight", "Length"))) #set order for plotting later


# output table format if argument form = 1
if (form == 1) {
  # Return wide table
  table <- bio_data %>%
    pivot_wider(
      names_from = sample_type,
      values_from = n_samples
    )
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
plot <- ggplot(bio_data, aes(year, sample_type)) +
  ggplot2::geom_tile(aes(fill = n_samples), colour = "white") +
  scale_fill_distiller(direction = 1) + 
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
  scale_x_continuous(breaks = seq(min(bio_data$year)+1, max(bio_data$year), by = 2))+
  ggplot2::ggtitle("Survey Specimen Counts") +
  coord_cartesian(expand = FALSE)

return(plot)
}
}

