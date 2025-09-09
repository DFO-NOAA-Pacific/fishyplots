#' Plots distribution of ages across survey years.
#'
#' @param data biological data containing age and sex information for at least regions specified in `subregions`.
#' @param subregions choose NWFSC, PBS, AK GULF, and/or AK BSAI. Default all.
#' @param species species common or scientific name.
#' @param by_sex TRUE or FALSE for if you want to differentiate by sex.
#' @param cutoff define a cutoff for grouping older ages together.
#' @param facet_all if TRUE this will facet all surveys regardless of missing data, if FALSE then only the region(s) with data will be faceted
#' @return a ggplot object
#' @importFrom dplyr filter mutate distinct arrange case_when
#' @importFrom stringr str_extract
#' @importFrom tidyr complete full_seq
#' @importFrom stats quantile
#' @importFrom ggplot2 ggplot aes theme_bw labs scale_x_continuous theme scale_fill_manual theme_void ggtitle
#' @importFrom ggridges geom_density_ridges 
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' data(nwfsc_bio)
#' data(afsc_bio)
#' data(pbs_bio)
#' all_data <- rbind(nwfsc_bio, afsc_bio, pbs_bio)
#' 
#' age_frequency(all_data, species = "arrowtooth flounder")
#' age_frequency(all_data, c("NWFSC", "AK GULF"), species = "anoplopoma fimbria", by_sex = T, facet_all = F)
#' 
#' # For best axis visibility, it is recommended that regions are viewed one at a time.
#' age_frequency(all_data, subregion = "NWFSC", species = "anoplopoma fimbria", facet_all = F, cutoff = 0.1)
#' }
age_frequency <- function(data, subregions = c("AK BSAI", "AK GULF", "NWFSC", "PBS"), species, by_sex = FALSE, cutoff = 0.95, facet_all = TRUE) {
  # Clean data
  data_clean <- data |>
    filter(!is.na(.data$age_years)) |>
    filter(.data$sex != "U") |>
    filter(.data$survey %in% subregions) |>
    filter(species == .data$common_name | species == .data$scientific_name) 
  
  # Exit if no data
  if (nrow(data_clean) == 0) {
    #message(paste0("No age data available for ", common, " in ", paste(subregions, collapse = ","), "."))
    return(ggplot() + theme_void() + ggtitle("No age frequency data available."))
  }
  
  # Define a cutoff to start grouping bins together
  cutoff <- as.numeric(quantile(data_clean$age_years, cutoff, na.rm = TRUE))
  
  # Group together bins above the cutoff
  # Eg. cutoff = 7 and age = 15, so 15-7 = 8, 8/5 = 1.6, floor = 1, 5*1+7 = 12, 12 is the lower bound of the cutoff
  data_clean <- data_clean |>
    mutate(age_group = case_when(
      .data$age_years < cutoff ~ as.character(.data$age_years),
      .data$age_years >= cutoff ~ paste0(5 * floor((.data$age_years - cutoff) / 5) + cutoff, "-",
                                   5*floor((.data$age_years - cutoff) / 5) + cutoff + 4)))
  
  # Create an ordered list of age_groups by their lower bounds
  age_levels <- data_clean |>
    distinct(.data$age_group) |>
    mutate(age_group_lower = as.numeric(str_extract(.data$age_group, "^[0-9]+"))) |>
    arrange(.data$age_group_lower)
  
  # Convert age_group to a factor with customized sorting from age_levels 
  data_clean <- data_clean |>
    mutate(age_group = factor(.data$age_group, levels = age_levels$age_group)) |>
    mutate(age_group_num = as.numeric(.data$age_group))
  
  # Create labels based off the age_group (we will use age_group_num to plot)
  labels <- levels(data_clean$age_group)
  
  # Re factor survey labels
  region_levels <- c("AK BSAI", "AK GULF", "PBS", "NWFSC")
  region_labels <- c("Aleutians/Bering Sea", "Gulf of Alaska", "Canada", "U.S. West Coast")
  data_clean$survey <- factor(data_clean$survey, levels = region_levels, labels = region_labels)
  
  # Create base graph 
  if (by_sex == FALSE) {
    graph <- data_clean |> 
      ggplot(aes(x = .data$age_group_num, y = factor(.data$year))) +
      geom_density_ridges(stat = "binline", bins = length(unique(data_clean$age_group)), scale = 1, draw_baseline = TRUE)
  } else if (by_sex == TRUE) {
    graph <- data_clean |> 
      ggplot(aes(x = .data$age_group_num, y = factor(.data$year), fill = .data$sex)) +
      geom_density_ridges(alpha = 0.5, stat = "binline", bins = length(unique(data_clean$age_group)), scale = 1, draw_baseline = TRUE) +
      scale_fill_manual(values = c("M" = "#008b8b", "F" = "#daa520"))
    }
  
  #facet wrap if plotting all regions
  if (length(subregions) > 1) {
    if (facet_all == TRUE) {
      graph <- graph + facet_wrap(~survey, ncol = 4, drop = FALSE)
      # find which regions have no data
      empty_surveys <- setdiff(levels(data_clean$survey), unique(data_clean$survey))
      
      if (length(empty_surveys) > 0) {
        # plot "no data" message in empty facets
        graph <- graph +
          geom_text(
            data = data.frame(survey = factor(empty_surveys,
                                              levels = levels(data_clean$survey))),
            aes(x = mean(range(data_clean$age_group_num)), y = levels(factor(data_clean$year))[ceiling(length(levels(factor(data_clean$year))) / 2)], label = "No data"),
            inherit.aes = FALSE)
      }
    }
    else if (facet_all == FALSE) {
      graph <- graph + facet_wrap(~survey, ncol = 4)
    }}
  
  # Add aesthetics
  graph <- graph + 
    theme_bw() +
    labs(x = "Age (years)", y = "", title = "Age frequency") +
    scale_x_continuous(breaks = 1:length(labels), labels = labels)+ #, guide = guide_axis(n.dodge=2)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(panel.grid = element_blank())

  return(graph)
}
