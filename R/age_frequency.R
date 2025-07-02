#' Age frequency function. Plots distribution of ages across survey years, and can differentiate by sex.
#'
#' @param data biological data from pull_bio(), containing Age_years
#' @param sex choose "y" or "n" for if you want to differentiate by sex 
#' @return a ggplot object
#' @importFrom dplyr filter
#' @importFrom tidyr complete full_seq
#' @importFrom ggplot2 ggplot aes theme_bw labs
#' @importFrom ggridges geom_density_ridges
#' @export
#'
#' @examples
#' \dontrun{
#' age_frequency(bio_data, sex = "n")
#' }
age_frequency <- function(data, sex) {
  if (sex == "n") {
    plot <- data |>
      filter(!is.na(Age_years)) |>
      filter(Sex != "U") |>
      complete(Year = full_seq(Year, 1)) |>
      ggplot(aes(x = Age_years, y = factor(Year))) +
      geom_density_ridges(stat = "binline", bins = length(unique(data$Age_years)), scale = 1, draw_baseline = TRUE) +
      theme_bw() +
      labs(x = "Age (years)", y = "", title = "Age frequency")
    return(plot)
  } else if (sex == "y") {
    plot <- data |>
      filter(!is.na(Age_years)) |>
      filter(Sex != "U") |>
      complete(Year = full_seq(Year, 1)) |>
      ggplot(aes(x = Age_years, y = factor(Year), fill = Sex)) +
      geom_density_ridges(alpha = 0.5, stat = "binline", bins = length(unique(data$Age_years)), scale = 1, draw_baseline = TRUE) +
      theme_bw() +
      labs(x = "Age (years)", y = "", title = "Age frequency")
    return(plot)
  } else {
    message("Invalid input.")
  }
}


