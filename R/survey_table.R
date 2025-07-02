#' Main function to display survey specimen counts
#'
#' @param data a data frame of target species including Length_cm, Weight_kg, Age_years, and Year.
#' @param form choose 1 for tibble or 2 for ggplot
#' @return a tibble or ggplot object
#' @importFrom dplyr group_by summarize mutate arrange bind_rows
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
  mutate(sample_type = "length") #label as length count

weight_count <- data.frame(weight_cm = data$Weight_kg, year = data$Year) %>%
  na.omit() %>% 
  group_by(year) %>%
  summarize(n_samples=n())%>%
  mutate(sample_type = "weight")

age_count <- data.frame(age_yr = data$Age_years, year = data$Year) %>%
  na.omit() %>% 
  group_by(year) %>%
  summarize(n_samples=n())%>%
  mutate(sample_type = "age")

# put all into one df
bio_data <-length_count %>%
  bind_rows(weight_count) %>%
  bind_rows(age_count) %>%
  arrange(year) %>%
  mutate(sample_type=factor(sample_type, levels=c("age", "weight", "length"))) #set order for plotting later


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
                              ifelse(bio_data$n_samples < 1000,
                                     as.character(round(bio_data$n_samples, digits = -2)),  # round to nearest hundred if under 1000
                                     paste0(round(bio_data$n_samples / 1000), "K") )) # if over 1000, round to nearest and label with k

plot <- ggplot(bio_data, aes(x=year, y=sample_type, fill=n_samples)) +
  geom_tile() +
  geom_text(aes(label=label), color="black") + # add labels to tiles
  scale_fill_distiller(direction = 1) +  # set color scale for fill
  theme_minimal() +
  ggtitle("Survey specimen counts") +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11),legend.position="none") #legend and axis text adjustments
return (plot)
}
}

