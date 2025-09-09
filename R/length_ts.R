#' Main function to plot mean lengths over time by sex
#'
#' @param data biological data containing length information for at least regions specified in `subregions`.
#' @param subregions choose NWFSC, PBS, AK GULF, and/or AK BSAI. Default all.
#' @param species species common or scientific name.
#' @param facet_all if TRUE this will facet all surveys regardless of missing data, if FALSE then only the region(s) with data will be faceted.#' @return a time series plot of mean lengths by sex
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_color_manual scale_fill_manual theme_bw theme element_blank element_text xlab ylab geom_label geom_ribbon
#' @importFrom dplyr filter mutate
#' @importFrom stats sd
#' @export
#'
#' @examples
#' \dontrun{
#' data(vb_predictions)
#' data(pbs_bio)
#' data(afsc_bio)
#' data(nwfsc_bio)
#' all_data <- bind_rows(pbs_bio, afsc_bio, nwfsc_bio)
#' 
#' length_ts(all_data, species = "arrowtooth flounder")
#' length_ts(all_data, c("NWFSC", "AK GULF"),species = "anoplopoma fimbria", facet_all = F)
#' }

length_ts <- function(data, subregions = c("AK BSAI", "AK GULF", "PBS", "NWFSC"), species, facet_all = TRUE) {
  
  spec.data <- data |>
    filter(!is.na(.data$length_cm), !.data$sex == "U") |>
    filter(.data$survey %in% subregions) |>
    filter(.data$common_name == species | .data$scientific_name == species)
  
  # Exit if no data
  if (nrow(spec.data) == 0) {
    return(ggplot() + theme_void() + ggtitle("No length data available."))
  }
  
  #set region names
  spec.data$survey <- factor(spec.data$survey, levels = c("AK BSAI", "AK GULF", "PBS", "NWFSC"),
                             labels = c("Aleutians/Bering Sea", "Gulf of Alaska", "Canada", "U.S. West Coast"))
  
  #plot version of the data
  plot.data <- spec.data |> 
    select(.data$survey, .data$year, .data$common_name, .data$sex, .data$length_cm ) |> 
    group_by(.data$survey, .data$year, .data$sex ) |> 
    summarise(avg.length = mean(.data$length_cm),
              se = sd(.data$length_cm, na.rm = TRUE) / sqrt(n()))
  
  sex.color <- c("M" = "#E69F00", "F" = "#009E73")
  legend <- data.frame(label = c("M", "F"),
                       color = c("M","F"),
                       hjust = c(2.5,1.75),
                       vjust = c(3,2))
  
  plot <-  ggplot(data = plot.data, mapping = aes(x = .data$year, y = .data$avg.length, color = .data$sex, fill = .data$sex)) +
    geom_point(size = 2.5)+
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
      data = legend |> filter(.data$label == "M"),
      aes(x = -Inf, y = Inf, label = .data$label, color = .data$color, fill = .data$color),
      hjust = -1.6, vjust = 1,
      alpha = 0.2, inherit.aes = FALSE, show.legend = FALSE, size = 3.3) +
    geom_label( #f/m legend
      data = legend |> filter(.data$label == "F"),
      aes(x = -Inf, y = Inf, label = .data$label, color = .data$color, fill = .data$color),
      hjust = -0.5, vjust = 1,
      alpha = 0.2, inherit.aes = FALSE, show.legend = FALSE, size = 3.3)
  
  #make a note if containing afsc data ( length data used for survey_table uses a larger count of lengths that we do not have access to in our afsc_bio dataset)
  if(any(spec.data$region %in% "AFSC")) {
    plot <- plot + labs(caption = "Note: AFSC may have additional length samples not available in this app's datasets. Please see our 'Data' tab.") 
  } 
  
  #facet wrap if plotting all regions
  if (length(subregions) > 1) {
    if (facet_all == TRUE) {
      plot <- plot + facet_wrap(~survey, ncol = 4, drop = FALSE)
      # find which regions have no data
      empty_surveys <- setdiff(levels(spec.data$survey), unique(spec.data$survey))
      
      if (length(empty_surveys) > 0) {
        # plot "no data" message in empty facets
        plot <- plot +
          geom_text(
            data = data.frame(survey = factor(empty_surveys, levels = levels(plot.data$survey))),
            aes(x = mean(range(plot.data$year)), y = mean(range(plot.data$avg.length)), label = "No data"),
            inherit.aes = FALSE)
      }
    }
    else if (facet_all == FALSE) {
      plot <- plot + facet_wrap(~survey, ncol = 4)
    }}
  
  
  return(plot)
} 
