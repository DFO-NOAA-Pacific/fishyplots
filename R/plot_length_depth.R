#' Function for length-depth plot
#'
#' @param data biological data
#' @param subregion choose PBS, NWFSC, AK BSAI, AK GULF
#' @param common species common name 
#' @param by_sex show sex differentiation 
#' @return a ggplot object
#' @importFrom dplyr filter mutate group_by summarise ungroup distinct slice_min ungroup arrange
#' @importFrom ggplot2 ggplot theme_void ggtitle geom_col geom_tile facet_wrap scale_fill_viridis_c scale_fill_viridis_d coord_cartesian labs theme_bw theme scale_x_continuous coord_flip scale_x_reverse facet_grid
#' @importFrom patchwork plot_layout
#' @export
#'
#' @examples
#' \dontrun{
#' data(afsc_bio)
#' data(pbs_bio)
#' data(nwfsc_bio)
#' all_data <- bind_rows(afsc_bio, pbs_bio, nwfsc_bio)
#' plot_length_depth(all_data, c("AK BSAI", "AK GULF", "PBS", "NWFSC"), "arrowtooth flounder")
#' }
plot_length_depth <- function(data, subregion = c("NWFSC", "PBS", "AK BSAI", "AK GULF"), common, by_sex = FALSE) {
  # Sex differentiation automatic for 1 region
  if (length(subregion) == 1) {
    by_sex <- TRUE
    data <- data |> filter(survey == subregion)
  }
  
  # Clean data 
  clean_data <- data |>
    filter(survey %in% subregion) |>
    filter(common_name == common) |>
    filter(!is.na(depth_m)) |>
    filter(!is.na(length_cm))
  
  # Remove unsexed fish if user inputs sex differentiation
  if (by_sex) {
    clean_data <- clean_data |> filter(sex != "U")
  }
  
  # Get rid of decimals in length
  # Correct for even number bias by creating function instead of using round()
  get_rounded_lengths <- function(x) {
    floor(x + 0.5)
  }
  clean_data$length_cm <- get_rounded_lengths(clean_data$length_cm)
  
  # Check for enough data
  if (nrow(clean_data) == 0) {
    return(ggplot() + theme_void() + ggtitle("No length-depth data available."))
  }
  
  # Create age, length, sex, and depth bins
  clean_data <- clean_data |>
    mutate(
      sex_group = if (by_sex == TRUE) sex else "all", # dummy grouping for when no sex differentiation
      depth_bin1 = cut(depth_m, breaks = seq(0, max(depth_m, na.rm = TRUE), by = 25)),
      depth_bin2 = cut(depth_m, breaks = seq(0, max(depth_m, na.rm = TRUE), by = 50)),
      length_group = cut(length_cm, breaks = seq(0, max(length_cm, na.rm = TRUE), by = 1))
    )
  
  # Counts for each graph
  counts1 <- clean_data |>
    group_by(survey, sex_group, depth_bin1, length_group) |>
    summarise(count = n(), .groups = "drop") |>
    filter(!is.na(length_group)) |>
    group_by(survey, sex_group, depth_bin1) |>
    mutate(
      prop = count / sum(count),
      depth_mid = (as.numeric(sub("\\[|\\(|\\]", "", sub(",.*", "", as.character(depth_bin1)))) +
                     as.numeric(sub("\\]|\\)", "", sub(".*,", "", as.character(depth_bin1))))) / 2
    ) |>
    ungroup()
  
  counts2 <- clean_data |>
    filter(!is.na(length_cm)) |>
    group_by(survey, sex_group, depth_bin2, length_cm) |>
    summarise(count = n(), .groups = "drop") |>
    group_by(survey, sex_group, depth_bin2) |>
    mutate(
      prop = count / sum(count),
      depth_mid = (as.numeric(sub("\\[|\\(|\\]", "", sub(",.*", "", as.character(depth_bin2)))) +
                     as.numeric(sub("\\]|\\)", "", sub(".*,", "", as.character(depth_bin2))))) / 2
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
  counts1 <- counts1 |> filter(is.finite(depth_mid), is.finite(length_group), is.finite(prop))
  counts2 <- counts2 |> filter(is.finite(depth_mid), is.finite(length_cm), is.finite(prop))
  
  # Check that there is enough data in the counts
  if (by_sex == TRUE) {
    if (nrow(subset(counts1, sex_group == "Male")) < 5 & nrow(subset(counts1, sex_group == "Female")) < 5) {
      return(ggplot() + theme_void() + ggtitle("Not enough length-depth data available."))
    }
  } else {
    if (nrow(counts1) < 5) {
      return(ggplot() + theme_void() + ggtitle("Not enough length-depth data available."))
    }
  }
  
  # Determine levels for plotting
  # Create a new column for 20cm bins
  counts1$length_group <- factor(counts1$length_group)
  counts1 <- counts1 |>
    mutate(length_start = as.numeric(gsub("^\\((\\d+),.*$", "\\1", length_group)),
           length_group20 = paste0(
             floor(length_start / 20) * 20, "-",
             floor(length_start / 20) * 20 + 20
           ))
  # For each 20cm bin, pick the first representative break
  rep_bins <- counts1 |>
    distinct(length_group, length_group20, length_start) |>
    group_by(length_group20) |>
    slice_min(order_by = length_start, n = 1) |>   # first 1cm bin per 20cm group
    ungroup() |>
    arrange(length_start)
  breaks <- rep_bins$length_group
  labels <- rep_bins$length_group20
  
  # Plotting
  p1 <- ggplot(counts1, aes(x = depth_mid, y = count, fill = length_group)) +
    geom_col(position = "stack", width = 25) +
    scale_fill_viridis_d(name = "Length (cm)",
                         breaks = breaks,
                         labels = labels,
                         direction = -1,
                         option = "mako") +
    coord_cartesian(expand = FALSE) +
    labs(title = "Length distribution across depth", x = "Depth (m)", y = "Count") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(breaks = if (max(counts1$depth_mid, na.rm = TRUE) >= 100) 
      seq(100, max(counts1$depth_mid), by = 100) else pretty(counts1$depth_mid))
  
  p2 <- ggplot(counts2, aes(x = depth_mid, y = length_cm, fill = prop)) +
    geom_tile() +
    scale_fill_viridis_c(option = "mako", name = "Proportional \nlength \ndistribution \nby depth", direction = -1) +
    labs(title = "Length-depth heatmap", x = "Depth (m)", y = "Length (cm)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(panel.grid = element_blank()) +
    coord_flip(expand = FALSE) +
    scale_x_reverse()
  
  # Construct plots based on conditionals
  caption <- "Note: unsexed fish have been removed."
  if (length(subregion) > 1) {
    if (by_sex == TRUE) {
      p1 <- p1 + facet_grid(sex_group ~ survey, drop = FALSE) 
      p2 <- p2 + facet_grid(sex_group ~ survey, drop = FALSE) + labs(caption = caption)
    } else if (by_sex == FALSE) {
      p1 <- p1 + facet_wrap(~survey, drop = FALSE, ncol = 4)
      p2 <- p2 + facet_wrap(~survey, drop = FALSE, ncol = 4)
    }
  } else if (length(subregion) == 1) {
    if (by_sex == TRUE) {
      p1 <- p1 + facet_wrap(~sex_group, ncol = 1)
      p2 <- p2 + facet_wrap(~sex_group, ncol = 1) + labs(caption = caption)
    } 
  }
  
  return(p1 + p2 + plot_layout(ncol = 1))
  
}
