#' Plots distribution of ages across survey years.
#'
#' @param data biological fisheries data containing age and sex information
#' @param sex TRUE or FALSE for if you want to differentiate by sex
#' @param cutoff define a cutoff for grouping older ages together
#' @return a ggplot object
#' @importFrom dplyr filter mutate distinct arrange case_when
#' @importFrom stringr str_extract
#' @importFrom tidyr complete full_seq
#' @importFrom stats quantile
#' @importFrom ggplot2 ggplot aes theme_bw labs scale_x_continuous theme scale_fill_manual
#' @importFrom ggridges geom_density_ridges 
#' @export
#'
#' @examples
#' \dontrun{
#' # US West Coast
#' load("data/nwfsc_bio.rda")
#' data <- nwfsc_bio |> filter(common_name == "arrowtooth flounder")
#' age_frequency(data)
#' 
#' # Canada
#' load("data/pbs_bio.rda")
#' data <- pbs_bio |> filter(common_name == "arrowtooth flounder")
#' age_frequency(data, sex = TRUE)
#' 
#' # Alaska 
#' load("data/afsc_bio.rda")
#' data <- afsc_bio |> filter(common_name == "arrowtooth flounder")
#' age_frequency(data, cutoff = 0.75)
#' }
age_frequency <- function(data, sex = FALSE, cutoff = 0.95) {
  # Clean data
  data_clean <- data |>
    filter(!is.na(age_years)) |>
    filter(sex != "U") |>
    complete(year = full_seq(year, 1))
  
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
  
  if (sex == FALSE) {
    graph <- data_clean |> 
      ggplot(aes(x = age_group_num, y = factor(year))) +
      geom_density_ridges(stat = "binline", bins = length(unique(data_clean$age_group)), scale = 1, draw_baseline = TRUE) +
      theme_bw() +
      labs(x = "Age (years)", y = "", title = "Age frequency") +
      scale_x_continuous(breaks = 1:length(labels), labels = labels) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(graph)
  }
  else if (sex == TRUE) {
    graph <- data_clean |> 
      ggplot(aes(x = age_group_num, y = factor(year), fill = sex)) +
      geom_density_ridges(alpha = 0.5, stat = "binline", bins = length(unique(data_clean$age_group)), scale = 1, draw_baseline = TRUE) +
      theme_bw() +
      labs(x = "Age (years)", y = "", title = "Age frequency") +
      scale_x_continuous(breaks = 1:length(labels), labels = labels) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = c("M" = "#008b8b", "F" = "#daa520"))
    return(graph)
  }
}


