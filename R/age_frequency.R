#' Plots distribution of ages across survey years.
#'
#' @param data biological fisheries data containing age and sex information
#' @param subregions choose NWFSC, PBS, AK Gulf, and/or AK BSAI
#' @param common species common name
#' @param sex TRUE or FALSE for if you want to differentiate by sex
#' @param cutoff define a cutoff for grouping older ages together
#' @param facet_all if TRUE this will facet all surveys regardless of missing data, if FALSE then only the region(s) with data will be faceted
#' @return a ggplot object
#' @importFrom dplyr filter mutate distinct arrange case_when
#' @importFrom stringr str_extract
#' @importFrom tidyr complete full_seq
#' @importFrom stats quantile
#' @importFrom ggplot2 ggplot aes theme_bw labs scale_x_continuous theme scale_fill_manual theme_void ggtitle
#' @importFrom ggridges geom_density_ridges 
#' @export
#'
#' @examples
#' \dontrun{
#' data("nwfsc_bio")
#' data("afsc_bio")
#' data("pbs_bio")
#' nwfsc_bio <- nwfsc_bio |> select(-otosag_id)
#' all_data <- rbind(afsc_bio, nwfsc_bio, pbs_bio)
#' 
#' age_frequency(all_data, c("PBS", "NWFSC", "AK BSAI", "AK GULF"), "yellowtail rockfish")
#' }
age_frequency <- function(data, subregions, common, sex = FALSE, cutoff = 0.95, facet_all = TRUE) {
  # Clean data
  data_clean <- data |>
    filter(!is.na(age_years)) |>
    filter(sex != "U") |>
    filter(survey %in% subregions) |>
    filter(common_name == common) 
  
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
      age_years < cutoff ~ as.character(age_years),
      age_years >= cutoff ~ paste0(5 * floor((age_years - cutoff) / 5) + cutoff, "-",
                                   5*floor((age_years-cutoff) / 5) + cutoff + 4)))
  
  # Create an ordered list of age_groups by their lower bounds
  age_levels <- data_clean |>
    distinct(age_group) |>
    mutate(age_group_lower = as.numeric(str_extract(age_group, "^[0-9]+"))) |>
    arrange(age_group_lower)
  
  # Convert age_group to a factor with customized sorting from age_levels 
  data_clean <- data_clean |>
    mutate(age_group = factor(age_group, levels = age_levels$age_group)) |>
    mutate(age_group_num = as.numeric(age_group))
  
  # Create labels based off the age_group (we will use age_group_num to plot)
  labels <- levels(data_clean$age_group)
  
  # Re factor survey labels
  region_levels <- c("AK BSAI", "AK GULF", "PBS", "NWFSC")
  region_labels <- c("Aleutians/Bering Sea", "Gulf of Alaska", "Canada", "U.S. West Coast")
  data_clean$survey <- factor(data_clean$survey, levels = region_levels, labels = region_labels)
  
  # Create dummy data so that all surveys are faceted regardless of missing data
  # dummy <- data_clean |>
  #   complete(survey = factor(region_labels, levels = region_labels),
  #            year = full_seq(min(data_clean$year, na.rm = TRUE):max(data_clean$year, na.rm = TRUE), 1),
  #            fill = list(age_years = NA, age_group = NA, age_group_num = NA))
  # 
  # Create base graph 
  if (sex == FALSE) {
    graph <- data_clean |> 
      ggplot(aes(x = age_group_num, y = factor(year))) +
      geom_density_ridges(stat = "binline", bins = length(unique(data_clean$age_group)), scale = 1, draw_baseline = TRUE)
  } else if (sex == TRUE) {
    graph <- data_clean |> 
      ggplot(aes(x = age_group_num, y = factor(year), fill = sex)) +
      geom_density_ridges(alpha = 0.5, stat = "binline", bins = length(unique(data_clean$age_group)), scale = 1, draw_baseline = TRUE) +
      scale_fill_manual(values = c("M" = "#008b8b", "F" = "#daa520"))
    }
  
  # Add facet wrap option for multiple regions
  if (length(subregions) > 1) {
    if (facet_all == TRUE) {
      graph <- graph + facet_wrap(~survey, ncol = 4, drop = FALSE)
    } else if (facet_all == FALSE) {
      graph <- graph + facet_wrap(~survey, ncol = 4)
    }
  }
  
  # Add aesthetics
  graph <- graph + 
    theme_bw() +
    labs(x = "Age (years)", y = "", title = "Age frequency") +
    scale_x_continuous(breaks = 1:length(labels), labels = labels) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(panel.grid = element_blank())
    #geom_blank(data = dummy, aes(x = 1, y = factor(min(year, na.rm = TRUE))))
  
  return(graph)
}
