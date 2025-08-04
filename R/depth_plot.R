#' Function for age-depth plot
#'
#' @param data biological data
#' @param subregion choose PBS, NWFSC, AK BSAI, AK GULF
#' @param common species common name 
#' @param sex show sex differentiation 
#' @return a ggplot object
#' @importFrom dplyr filter mutate group_by summarise ungroup
#' @importFrom ggplot2 ggplot theme_void ggtitle geom_col geom_tile facet_wrap scale_fill_viridis_c scale_fill_viridis_d coord_cartesian labs theme_bw theme scale_x_continuous coord_flip scale_x_reverse facet_grid
#' @importFrom patchwork plot_layout
#' @export
#'
#' @examples
#' \dontrun{
#' all_data <- bind_rows(afsc_bio, pbs_bio, nwfsc_bio)
#' depth_plot(all_data, "AFSC", "arrowtooth flounder")
#' }

depth_plot <- function(data, subregion = c("NWFSC", "PBS", "AK BSAI", "AK GULF"), common, sex = FALSE) {
  # Sex differentiation automatic for 1 region
  if (length(subregion) == 1) {
    sex <- TRUE
    data <- data |> filter(survey == subregion)
  }
  
  # Clean data and check
  clean_data <- data |>
    filter(common_name == common) |>
    filter(!is.na(depth_m)) |>
    filter(!is.na(age_years)) |>
    filter(sex != "U")
  
  if (nrow(clean_data) == 0) {
    return(ggplot() + theme_void() + ggtitle("No age-depth data available."))
  }
  
  # Create age and depth bins
  clean_data <- clean_data |>
    mutate(
      depth_bin1 = cut(depth_m, breaks = seq(0, max(depth_m, na.rm = TRUE), by = 25)),
      depth_bin2 = cut(depth_m, breaks = seq(0, max(depth_m, na.rm = TRUE), by = 50)),
      age_group = cut(age_years, breaks = seq(0, 90, by = 1))
    )
  
  # Counts for each graph
  counts1 <- clean_data |>
    group_by(survey, sex, depth_bin1, age_group) |>
    summarise(count = n(), .groups = "drop") |>
    filter(!is.na(age_group)) |>
    group_by(survey, sex, depth_bin1) |>
    mutate(
      count = count,
      prop = count / sum(count),
      depth_mid = (as.numeric(sub("\\[|\\(|\\]", "", sub(",.*", "", as.character(depth_bin1)))) +
                     as.numeric(sub("\\]|\\)", "", sub(".*,", "", as.character(depth_bin1))))) / 2
    ) |>
    ungroup()
  
  counts2 <- clean_data |>
    group_by(survey, sex, depth_bin2, age_years) |>
    summarise(count = n(), .groups = "drop") |>
    filter(!is.na(age_years)) |>
    group_by(survey, sex, depth_bin2) |>
    mutate(
      count = count,
      prop = count / sum(count),
      depth_mid = (as.numeric(sub("\\[|\\(|\\]", "", sub(",.*", "", as.character(depth_bin2)))) +
                     as.numeric(sub("\\]|\\)", "", sub(".*,", "", as.character(depth_bin2))))) / 2
    ) |>
    ungroup()
  
  # Factor relevel sex and survey
  counts1$sex <- factor(counts1$sex, levels = c("M", "F"), labels = c("Male", "Female"))
  counts2$sex <- factor(counts2$sex, levels = c("M", "F"), labels = c("Male", "Female"))
  counts1$survey <- factor(counts1$survey, levels = c("AK BSAI", "AK GULF", "PBS", "NWFSC"), labels = c("Aleutians/Bering Sea", "Gulf of Alaska", "Canada", "US West Coast"))
  counts2$survey <- factor(counts2$survey, levels = c("AK BSAI", "AK GULF", "PBS", "NWFSC"), labels = c("Aleutians/Bering Sea", "Gulf of Alaska", "Canada", "US West Coast"))
  
  # Debug
  counts1 <- counts1 |> filter(is.finite(depth_mid), is.finite(age_group), is.finite(prop))
  counts2 <- counts2 |> filter(is.finite(depth_mid), is.finite(age_years), is.finite(prop))
  
  # Check that there is enough data
  if (nrow(subset(counts1, sex == "Male")) < 5 & nrow(subset(counts1, sex == "Female")) < 5) {
    return(ggplot() + theme_void() + ggtitle("Not enough age-depth data available."))
  }
  
  # Determine levels for plotting
  # Get all age levels
  age_levels_all <- levels(counts1$age_group) #[seq(1, length(levels(counts1$age_group)), by = 10)]
  # Get max age
  max_age <- max(counts2$age_years)
  # Extract age from age group
  start_ages <- as.numeric(gsub("^\\((\\d+),.*$", "\\1", age_levels_all))
  # Get the indices for the actual ages of that species
  valid_indices <- which(start_ages <= max_age)
  # Get the actual ages from all ages based on those indices
  age_levels <- age_levels_all[valid_indices]
  # Get the indices for actual ages by 10 for the scale
  selected_indices <- seq(1, length(age_levels), by = 10)
  # Get the by-10 age levels from valid ages
  age_levels <- age_levels[selected_indices]
  
  # Determine labels for plotting
  # Extract all ages from age groups
  age_labels <- gsub("^\\((\\d+),.*$", "\\1", age_levels)
  age_labels <- as.numeric(age_labels)
  # Format labels
  age_labels <- paste0(age_labels, "-", age_labels + 10)
  

  # Plotting
  p1 <- ggplot(counts1, aes(x = depth_mid, y = count, fill = age_group)) +
    geom_col(position = "stack", width = 25) +
    facet_grid(sex ~ survey, drop = FALSE) +
    scale_fill_viridis_d(name = "Age (years)",
                         breaks = age_levels,
                         labels = age_labels, #c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90"),
                         #drop = FALSE,
                         direction = -1,
                         option = "magma") +
    coord_cartesian(expand = FALSE) +
    labs(title = "Age distribution across depth", x = "Depth (m)", y = "Count") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(breaks = if (max(counts1$depth_mid, na.rm = TRUE) >= 100) 
      seq(100, max(counts1$depth_mid), by = 100) else pretty(counts1$depth_mid))
  
  p2 <- ggplot(counts2, aes(x = depth_mid, y = age_years, fill = prop)) +
    geom_tile() +
    facet_grid(sex ~ survey, drop = FALSE) +
    scale_fill_viridis_c(option = "magma", name = "Proportional \nage \ndistribution \nby depth", direction = -1) +
    labs(title = "Age-depth heatmap", x = "Depth (m)", y = "Age (years)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(panel.grid = element_blank()) +
    coord_flip(expand = FALSE) +
    scale_x_reverse()
  
  # Construct plots based on conditionals
  if (length(subregion) > 1) {
    if (sex == TRUE) {
      p1 <- p1 + facet_grid(sex ~ survey, drop = FALSE)
      p2 <- p2 + facet_grid(sex ~ survey, drop = FALSE)
    } else if (sex == FALSE) {
      p1 <- p1 + facet_wrap(~survey, drop = FALSE, ncol = 4)
      p2 <- p2 + facet_wrap(~survey, drop = FALSE, ncol = 4)
    }
  } else if (length(subregion) == 1) {
    if (sex == TRUE) {
      p1 <- p1 + facet_wrap(~sex, ncol = 1)
      p2 <- p2 + facet_wrap(~sex, ncol = 1)
    } 
  }
  
  return(p1 + p2 + plot_layout(ncol = 1))
  
}
