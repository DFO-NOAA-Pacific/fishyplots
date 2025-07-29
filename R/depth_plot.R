#' Function for age-depth plot
#'
#' @param data prediction data from fishfit scripts
#' @param region choose AFSC, PBS, NWFSC (choosing AK BSAI and AK GULF will default to AFSC)
#' @param common_name species common name 
#' @return a ggplot object
#' @importFrom dplyr filter mutate group_by summarise ungroup
#' @importFrom ggplot2 ggplot theme_void ggtitle geom_col geom_tile facet_wrap scale_fill_viridis_c scale_fill_viridis_d coord_cartesian labs theme_bw theme scale_x_continuous coord_flip scale_x_reverse
#' @importFrom patchwork plot_layout
#' @export
#'
#' @examples
#' \dontrun{
#' # Still need depth column in the regular _bio data frames
#' # Once that is added this code will work
#' 
#' all_data <- bind_rows(afsc_bio, pbs_bio, nwfsc_bio)
#' depth_plot(all_data, "AFSC", "arrowtooth flounder")
#' }

depth_plot <- function(data, subregion = c("AFSC", "NWFSC", "PBS", "AK BSAI", "AK GULF"), common, option = c("a", "b", "c")) {
  option <- match.arg(option)
  
  clean_data <- data |>
    filter(region %in% subregion) |>
    filter(common_name == common)
  
  if (all(is.na(clean_data$depth_m)) | (nrow(clean_data) == 0)) {
    return(ggplot() + theme_void() + ggtitle("No age-depth data available."))
  }
  
  clean_data <- clean_data |>
    mutate(
      depth_bin1 = cut(depth_m, breaks = seq(0, max(depth_m), by = 25)),
      depth_bin2 = cut(depth_m, breaks = seq(0, max(depth_m), by = 50)),
      age_group = cut(age_years, breaks = seq(0, 90, by = 1))
    )
  
  counts1 <- clean_data |>
    group_by(sex, depth_bin1, age_group) |>
    summarise(count = n(), .groups = "drop") |>
    filter(!is.na(age_group)) |>
    group_by(sex, depth_bin1) |>
    mutate(
      count = count,
      prop = count / sum(count),
      depth_mid = (as.numeric(sub("\\[|\\(|\\]", "", sub(",.*", "", as.character(depth_bin1)))) +
                     as.numeric(sub("\\]|\\)", "", sub(".*,", "", as.character(depth_bin1))))) / 2
    ) |>
    ungroup()
  
  counts2 <- clean_data |>
    group_by(sex, depth_bin2, age_years) |>
    summarise(count = n(), .groups = "drop") |>
    filter(!is.na(age_years)) |>
    group_by(sex, depth_bin2) |>
    mutate(
      count = count,
      prop = count / sum(count),
      depth_mid = (as.numeric(sub("\\[|\\(|\\]", "", sub(",.*", "", as.character(depth_bin2)))) +
                     as.numeric(sub("\\]|\\)", "", sub(".*,", "", as.character(depth_bin2))))) / 2
    ) |>
    ungroup()
  
  counts1$sex <- factor(counts1$sex, levels = c("U", "M", "F"), labels = c("Unsexed", "Male", "Female"))
  counts2$sex <- factor(counts2$sex, levels = c("U", "M", "F"), labels = c("Unsexed", "Male", "Female"))
  
  counts1 <- counts1 |> filter(is.finite(depth_mid), is.finite(age_group), is.finite(prop))
  counts2 <- counts2 |> filter(is.finite(depth_mid), is.finite(age_years), is.finite(prop))
  
  age_levels_to_show <- levels(counts1$age_group)[seq(1, length(levels(counts1$age_group)), by = 10)]
  
  p1 <- ggplot(counts1, aes(x = depth_mid, y = count, fill = age_group)) +
    geom_col(position = "stack", width = 25) +
    facet_wrap(~sex) +
    scale_fill_viridis_d(name = "Age (years)",
                         breaks = age_levels_to_show,
                         labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90"),
                         direction = -1,
                         option = "magma") +
    coord_cartesian(expand = FALSE) +
    labs(x = "Depth (m)", y = "Count") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(panel.grid = element_blank()) +
    scale_x_continuous(breaks = seq(200, 600, by = 200))
  
  p2 <- ggplot(counts2, aes(x = depth_mid, y = age_years, fill = prop)) +
    geom_tile() +
    facet_wrap(~sex) +
    scale_fill_viridis_c(option = "magma", name = "Proportion", direction = -1) +
    coord_cartesian(expand = FALSE) +
    labs(x = "Depth (m)", y = "Age (years)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(panel.grid = element_blank()) +
    coord_flip() +
    scale_x_reverse()
  
  if (option == "a") {
    return(p1)
  } else if (option == "b") {
    return(p2) 
  } else if (option == "c") {
    return(p1 + p2 + plot_layout(ncol = 1))
  }
}
