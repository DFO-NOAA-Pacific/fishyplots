#' Function for age-depth plot
#'
#' @param data biological data containing age and depth information for at least regions specified in `subregions`.
#' @param subregions choose NWFSC, PBS, AK GULF, and/or AK BSAI. Default all.
#' @param species species common or scientific name.
#' @param by_sex show sex differentiation 
#' @param facet_all if TRUE this will facet all surveys regardless of missing data, if FALSE then only the region(s) with data will be faceted
#' @return a ggplot object
#' @importFrom dplyr filter mutate group_by summarise ungroup n
#' @importFrom ggplot2 ggplot theme_void ggtitle geom_col geom_tile facet_wrap scale_fill_viridis_c scale_fill_viridis_d coord_cartesian labs theme_bw theme scale_x_continuous coord_flip scale_x_reverse facet_grid
#' @importFrom patchwork plot_layout
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' data(afsc_bio)
#' data(pbs_bio)
#' data(nwfsc_bio)
#' all_data <- bind_rows(afsc_bio, pbs_bio, nwfsc_bio)
#' 
#' plot_age_depth(all_data, species = "arrowtooth flounder")
#' plot_age_depth(all_data, c("NWFSC", "AK GULF"),species = "anoplopoma fimbria", by_sex = T, facet_all = F)
#' }

plot_age_depth <- function(data, subregions = c("NWFSC", "PBS", "AK BSAI", "AK GULF"), species, by_sex = FALSE, facet_all = TRUE) {
  # Sex differentiation automatic for 1 region
  if (length(subregions) == 1) {
    by_sex <- TRUE
    data <- data |> filter(.data$survey == subregions)
  }
  
  # Clean data 
  clean_data <- data |>
    filter(.data$survey %in% subregions) |>
    filter(species == .data$common_name | species == .data$scientific_name) |>
    filter(!is.na(.data$depth_m)) |>
    filter(!is.na(.data$age_years)) |>
    filter(.data$sex != "U")
  
  # Remove unsexed fish if user inputs sex differentiation
  if (by_sex) {
    clean_data <- clean_data |> filter(.data$sex != "U")
  }
  
  # Check for enough data
  if (nrow(clean_data) == 0) {
    return(ggplot() + theme_void() + ggtitle("No age-depth data available."))
  }
  
  # Create age, length, sex, and depth bins
  clean_data <- clean_data |>
    mutate(
      sex_group = if (by_sex == TRUE) .data$sex else "all", # dummy grouping for when no sex differentiation
      depth_bin1 = cut(.data$depth_m, breaks = seq(0, max(.data$depth_m, na.rm = TRUE), by = 25)),
      depth_bin2 = cut(.data$depth_m, breaks = seq(0, max(.data$depth_m, na.rm = TRUE), by = 50)),
      age_group = cut(.data$age_years, breaks = seq(0, 90, by = 1))
    )
  
  # Counts for each graph
  counts1 <- clean_data |>
    group_by(.data$survey, .data$sex_group, .data$depth_bin1, .data$age_group) |>
    summarise(count = n(), .groups = "drop") |>
    filter(!is.na(.data$age_group)) |>
    group_by(.data$survey, .data$sex_group, .data$depth_bin1) |>
    mutate(
      prop = .data$count / sum(.data$count),
      depth_mid = (as.numeric(sub("\\[|\\(|\\]", "", sub(",.*", "", as.character(.data$depth_bin1)))) +
                     as.numeric(sub("\\]|\\)", "", sub(".*,", "", as.character(.data$depth_bin1))))) / 2
    ) |>
    ungroup()
  
  counts2 <- clean_data |>
    filter(!is.na(.data$age_years)) |>
    group_by(.data$survey, .data$sex_group, .data$depth_bin2, .data$age_years) |>
    summarise(count = n(), .groups = "drop") |>
    group_by(.data$survey, .data$sex_group, .data$depth_bin2) |>
    mutate(
      prop = .data$count / sum(.data$count),
      depth_mid = (as.numeric(sub("\\[|\\(|\\]", "", sub(",.*", "", as.character(.data$depth_bin2)))) +
                     as.numeric(sub("\\]|\\)", "", sub(".*,", "", as.character(.data$depth_bin2))))) / 2
    ) |>
    ungroup()
  
  # Factor relevel sex and survey
  if (by_sex) {
    counts1$sex_group <- factor(counts1$sex_group, levels = c("M", "F"), labels = c("Male", "Female"))
    counts2$sex_group <- factor(counts2$sex_group, levels = c("M", "F"), labels = c("Male", "Female"))
  }
  
  counts1$survey <- factor(counts1$survey, levels = c("AK BSAI", "AK GULF", "PBS", "NWFSC"), labels = c("Aleutians/Bering Sea", "Gulf of Alaska", "Canada", "US West Coast"))
  counts2$survey <- factor(counts2$survey, levels = c("AK BSAI", "AK GULF", "PBS", "NWFSC"), labels = c("Aleutians/Bering Sea", "Gulf of Alaska", "Canada", "US West Coast"))
  
  # Debug
  counts1 <- counts1 |> filter(is.finite(.data$depth_mid), is.finite(.data$age_group), is.finite(.data$prop))
  counts2 <- counts2 |> filter(is.finite(.data$depth_mid), is.finite(.data$age_years), is.finite(.data$prop))
  
  # Check that there is enough data
  if (by_sex == TRUE) {
    if (nrow(subset(counts1, counts1$sex_group == "Male")) < 5 & nrow(subset(counts1, counts1$sex_group == "Female")) < 5) {
      return(ggplot() + theme_void() + ggtitle("Not enough age-depth data available."))
    }
  } else {
    if (nrow(counts1) < 5) {
      return(ggplot() + theme_void() + ggtitle("Not enough age-depth data available."))
    }
  }
  #browser()
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
  p1 <- ggplot(counts1, aes(x = .data$depth_mid, y = .data$count, fill = .data$age_group)) +
    geom_col(position = "stack", width = 25) +
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
  
  p2 <- ggplot(counts2, aes(x = .data$depth_mid, y = .data$age_years, fill = .data$prop)) +
    geom_tile() +
    scale_fill_viridis_c(option = "magma", name = "Proportional \nage \ndistribution \nby depth", direction = -1) +
    labs(title = "Age-depth heatmap", x = "Depth (m)", y = "Age (years)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(panel.grid = element_blank()) +
    coord_flip(expand = FALSE) +
    scale_x_reverse()
  
  # Construct plots based on conditionals
  caption <- "Note: unsexed fish have been removed."
  
  if (length(subregions) > 1) {
    
    if (by_sex == TRUE) {
      # Multiple regions, by sex
      p1 <- p1 + facet_grid(sex_group ~ survey, drop = !facet_all)
      p2 <- p2 + facet_grid(sex_group ~ survey, drop = !facet_all) + labs(caption = caption)
      
      # Window 1 midpoint
      totals <- counts1 %>%
        group_by(depth_mid,survey, sex_group) %>%
        summarise(total = sum(count, na.rm = TRUE), .groups = "drop")
      mp1 <- max(totals$total, na.rm = TRUE)/2
      
      
    } else {
      # Multiple regions, not by sex
      p1 <- p1 + facet_wrap(~survey, ncol = 4, drop = !facet_all)
      p2 <- p2 + facet_wrap(~survey, ncol = 4, drop = !facet_all)
      
      totals <- counts1 %>%
        group_by(depth_mid,survey) %>%
        summarise(total = sum(count, na.rm = TRUE), .groups = "drop")
      mp1 <- max(totals$total, na.rm = TRUE)/2
    }
    
    # Add "No data" text if keeping all regions (drop = FALSE)
    if (facet_all) {
      empty_surveys <- setdiff(levels(counts1$survey), unique(counts1$survey))
      
      if (length(empty_surveys) > 0) {
        
        p1 <- p1 +
          geom_text(
            data = data.frame(survey = factor(empty_surveys, levels = levels(counts1$survey))),
            aes(x = mean(range(counts1$depth_mid)), y = mp1, label = "No data"),
            inherit.aes = FALSE
          )
        
        p2 <- p2 +
          geom_text(
            data = data.frame(survey = factor(empty_surveys, levels = levels(counts2$survey))),
            aes(x = mean(range(counts2$depth_mid)), y = mean(range(counts2$age_years)), label = "No data"),
            inherit.aes = FALSE
          )
      }
    }
    
  } else if (length(subregions) == 1) {
    
    if (by_sex == TRUE) {
      # Single region, by sex
      p1 <- p1 + facet_wrap(~sex_group, ncol = 1)
      p2 <- p2 + facet_wrap(~sex_group, ncol = 1) + labs(caption = caption)
    }
  }
  
  return(p1 + p2 + plot_layout(ncol = 1))
  
}
