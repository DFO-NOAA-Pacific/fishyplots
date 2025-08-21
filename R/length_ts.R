#' Main function to plot mean lengths over time by sex
#'
#' @param data region bio data to be plotted. Preload from data file, see examples.
#' @param species common or scientific name of target species
#' @return a time series plot of mean lengths by sex
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_color_manual scale_fill_manual theme_bw theme element_blank element_text xlab ylab geom_label geom_ribbon
#' @importFrom dplyr filter mutate
#' @importFrom stats sd
#' @export
#'
#' @examples
#' \dontrun{
#' data("nwfsc_bio")
#' data("pbs_bio")
#' data("afsc_bio")
#' akgulf_bio <- afsc_bio %>% filter(survey == "AK GULF")
#' akbsai_bio <- afsc_bio %>% filter(survey == "AK BSAI")
#' all_data <- bind_rows(akgulf_bio, akbsai_bio, nwfsc_bio, pbs_bio)
#' 
#' length_ts(all_data, "arrowtooth flounder")
#' length_ts(akgulf_bio, "atka mackerel")
#' }

length_ts <- function(data, species) {
  
  #subset data
  if (any(data$common_name == species | data$scientific_name == species)) {
    spec.data <- data  %>% 
      filter(species == .data$common_name | species == .data$scientific_name) %>%
      filter(!.data$sex == "U", !is.na(.data$length_cm)) #remove U sex, rows with NA lengths or widths
  } else (
    #stop(paste("Species name", "'", species,"'", "not found in this dataset.")))
    stop(return(ggplot() + theme_void() + ggtitle("No length data available."))))
  
  spec.data$survey <- factor(spec.data$survey, levels = c("AK BSAI", "AK GULF", "PBS", "NWFSC"),
                             labels = c("Aleutians/Bering Sea", "Gulf of Alaska", "Canada", "U.S. West Coast"))
  
  plot.data <- spec.data %>% 
    select(.data$survey, .data$year, .data$common_name, .data$sex, .data$length_cm ) %>% 
    group_by(.data$survey, .data$year, .data$sex ) %>% 
    summarise(avg.length = mean(.data$length_cm),
              se = sd(.data$length_cm, na.rm = TRUE) / sqrt(n())) 
  
  total.data <- spec.data %>% 
    select(.data$survey, .data$year, .data$common_name, .data$length_cm ) %>% 
    group_by(.data$survey, .data$year) %>% 
    summarise(avg.length.tot = mean(.data$length_cm),
              se.tot = sd(.data$length_cm, na.rm = TRUE) / sqrt(n())) 
  
  sex.color <- c("M" = "#E69F00", "F" = "#009E73")
  legend <- data.frame(label = c("M", "F"),
                       color = c("M","F"),
                       hjust = c(2.5,1.75),
                       vjust = c(3,2))
  
  plot <-  ggplot(data = plot.data, mapping = aes(x = .data$year, y = .data$avg.length, color = .data$sex, fill = .data$sex)) +
    geom_point()+
    geom_line(linewidth = 1)+
    geom_ribbon(aes(x = .data$year, ymin = .data$avg.length - .data$se, ymax = .data$avg.length + .data$se), alpha = 0.2, color = NA)+
    scale_color_manual(values = sex.color,
                       labels = c("M" = "Male", "F" = "Female"))+
    scale_fill_manual(values = sex.color,
                      labels = c("M" = "Male", "F" = "Female"))+
    ggtitle("Average Length") +
    ylab("Length (cm)") +
    xlab("Year") +
    theme_bw()+ 
    theme(legend.title = element_blank(),
          panel.grid = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1))+
    geom_label( #f/m legend
      data = legend %>% filter(.data$label == "M"),
      aes(x = -Inf, y = Inf, label = .data$label, color = .data$color, fill = .data$color),
      hjust = -1.6, vjust = 1,
      alpha = 0.2, inherit.aes = FALSE, show.legend = FALSE, size = 3.3) +
    geom_label( #f/m legend
      data = legend %>% filter(.data$label == "F"),
      aes(x = -Inf, y = Inf, label = .data$label, color = .data$color, fill = .data$color),
      hjust = -0.5, vjust = 1,
      alpha = 0.2, inherit.aes = FALSE, show.legend = FALSE, size = 3.3)
  
  #make a note if containing afsc data ( length data used for survey_table uses a larger count of lengths that we do not have access to in our afsc_bio dataset)
  if(any(spec.data$region %in% "AFSC")) {
    plot <- plot + labs(caption = "Note: AFSC may have additional length samples not available in this app's datasets. Please see our 'Data' tab.") 
  } 
  
  #facet wrap if plotting all regions
  if(length(unique(spec.data$survey)) > 1){
    plot <-  plot + facet_wrap( ~ survey, nrow = 1, drop = FALSE)
  }
 
  
  return(plot)
} 
