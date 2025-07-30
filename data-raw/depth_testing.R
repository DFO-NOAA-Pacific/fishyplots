data(afsc_bio)
data(nwfsc_bio)
data(pbs_bio)
all_data <- bind_rows(afsc_bio, nwfsc_bio, pbs_bio)

depth_plot <- function(data, subregion = c("NWFSC", "PBS", "AK BSAI", "AK GULF"), common, option = c("a", "b", "c"), sex = FALSE) {
  option <- match.arg(option)
  if (length(subregion) == 1) {
    sex <- TRUE
  }
  
  plot_list <- list()
  
  for (i in subregion) {
    pretty_name <- case_when(
      i == "NWFSC" ~ "US West Coast",
      i == "PBS" ~ "Canada",
      i == "AK BSAI" ~ "Aleutians/Bering Sea",
      i == "AK GULF" ~ "Gulf of Alaska"
    )
    clean_data <- data |>
      filter(survey %in% i) |>
      filter(common_name == common) |>
      filter(!is.na(depth_m)) |>
      filter(!is.na(age_years)) |>
      filter(sex != "U")
    
    if (all(is.na(clean_data$depth_m)) | (nrow(clean_data) == 0)) {
      p1 <- ggplot() + theme_void() + ggtitle(paste0(pretty_name, "\nNo age-depth data available."))
      p2 <- ggplot() + theme_void() + ggtitle("No age-depth data available.")
      p <- p1 + p2 + plot_layout(ncol = 1)
      plot_list[[length(plot_list) + 1]] <- p
      next
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
    
    if (nrow(counts1) < 10 | nrow(counts2) < 10) {
      p1 <- ggplot() + theme_void() + ggtitle(paste0(pretty_name, "\nNot enough data available."))
      p2 <- ggplot() + theme_void() + ggtitle("Not enough data available.")
      p <- p1 + p2 + plot_layout(ncol = 1)
      plot_list[[length(plot_list) + 1]] <- p
      next
    }
    
    counts1$sex <- factor(counts1$sex, levels = c("U", "M", "F"), labels = c("Unsexed", "Male", "Female"))
    counts2$sex <- factor(counts2$sex, levels = c("U", "M", "F"), labels = c("Unsexed", "Male", "Female"))
    
    counts1 <- counts1 |> filter(is.finite(depth_mid), is.finite(age_group), is.finite(prop))
    counts2 <- counts2 |> filter(is.finite(depth_mid), is.finite(age_years), is.finite(prop))
    
    used_age_levels <- sort(unique(counts1$age_group))
    age_levels_to_show <- used_age_levels[seq(1, length(used_age_levels), by = 10)]
    
    generate_age_labels <- function(levels) {
      lower <- as.numeric(sub("\\((.*),.*", "\\1", levels))
      decade_start <- floor(lower / 10) * 10
      decade_end <- decade_start + 10
      paste0(decade_start, "–", decade_end)
    }
    labels <- generate_age_labels(age_levels_to_show)
    #browser()
    p1 <- ggplot(counts1, aes(x = depth_mid, y = count, fill = age_group)) +
      geom_col(position = "stack", width = 25) +
      #facet_wrap(~sex, ncol = 1) +
      scale_fill_viridis_d(name = "Age (years)",
                           breaks = age_levels_to_show,
                           labels = labels, #c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90"),
                           #drop = FALSE,
                           direction = -1,
                           option = "magma") +
      coord_cartesian(expand = FALSE) +
      labs(title = paste0(pretty_name, "\nAge distribution across depth"), x = "Depth (m)", y = "Count") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(panel.grid = element_blank()) +
      scale_x_continuous(breaks = if (max(counts1$depth_mid, na.rm = TRUE) >= 100) 
        seq(100, max(counts1$depth_mid), by = 100) else pretty(counts1$depth_mid))
    
    p2 <- ggplot(counts2, aes(x = depth_mid, y = age_years, fill = prop)) +
      geom_tile() +
      #facet_wrap(~sex, ncol = 1) +
      scale_fill_viridis_c(option = "magma", name = "Proportional \nage \ndistribution \nby depth", direction = -1) +
      labs(title = "Age-depth heatmap", x = "Depth (m)", y = "Age (years)") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      theme(panel.grid = element_blank()) +
      coord_flip(expand = FALSE) +
      scale_x_reverse()
    
    if (sex == TRUE) {
      p1 <- p1 + facet_wrap(~sex, ncol = 1)
      p2 <- p2 + facet_wrap(~sex, ncol = 1)
    }
    
    p <- p1 + p2 + plot_layout(ncol = 1)
    
    plot_list[[length(plot_list) + 1]] <- p
  }
  if (length(plot_list) == 1) {
    return(plot_list[[1]])
  } else {
    return(wrap_plots(plot_list, ncol = 4))
  }
  
}
depth_plot(all_data, c("AK BSAI", "AK GULF", "PBS", "NWFSC"), "arrowtooth flounder")
