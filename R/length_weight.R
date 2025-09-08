#' Main function to plot length-weight relationships by sex
#'
#' @param data region bio data to be plotted. Preload from data file, see examples.
#' @param subregions choose NWFSC, PBS, AK Gulf, and/or AK BSAI
#' @param species common or scientific name of target species
#' @param subset default TRUE for a faster plotting subset of n = 10000. Set FALSE for all available data.
#' @param facet_all if TRUE this will facet all surveys regardless of missing data, if FALSE then only the region(s) with data will be faceted 
#' @return a plot of sexed data with log-log regression slope and intercept
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_linetype_manual theme_classic theme element_blank element_text xlab ylab annotate
#' @importFrom dplyr filter mutate slice_sample
#' @importFrom grid unit
#' @importFrom utils data
#' @export
#'
#' @examples
#' \dontrun{
#' data("nwfsc_bio")
#' data("pbs_bio")
#' data("afsc_bio")
#' akgulf_bio <- afsc_bio |> filter(survey == "AK GULF")
#' akbsai_bio <- afsc_bio |> filter(survey == "AK BSAI")
#' all_data <- bind_rows(akgulf_bio, akbsai_bio, nwfsc_bio, pbs_bio)
#' 
#' length_weight(data = all_data, subregions = c("AK BSAI", "AK GULF", "PBS", "NWFSC"), species = "arrowtooth flounder")
#' length_weight(data = akgulf_bio, subregions ="AK GULF", species = "arrowtooth flounder")
#' }

length_weight <- function(data, subregions, species, subset = TRUE, facet_all = TRUE) { # subset default

#### DATA ####
  #load lw prediction dataset
  
  #subset data to species (spec.data), or give error message
  if (any(data$common_name == species | data$scientific_name == species)) {
    spec.data <- data  |> 
      filter(species == .data$common_name | species == .data$scientific_name, .data$survey %in% subregions) |>
      filter(!.data$sex == "U", !is.na(.data$length_cm), !is.na(.data$weight_kg)) #remove U sex, rows with NA lengths or widths
 } else (
    #stop(paste("Species name", "'", species,"'", "not found in this dataset.")))
    stop(return(ggplot() + theme_void() + ggtitle("No length-weight data available."))))
  
  # when subset = TRUE, subset into 10000 random points for plotting speed
  if(subset == TRUE){
    spec.data <- spec.data |> 
      group_by(.data$survey) |>
      dplyr::slice_sample(n = 10000) |>  # in case any region has < 10000
      ungroup()
    if(nrow(spec.data) < 10000) {message ("Note: Species data less than 10000 rows; plotting all data with no subset")}
    else {message (paste0("Note: Plotting a random n = 10000 subset of ", unique(spec.data$common_name), ". Model values not impacted."))}}
  
# filter predictions to species
  predict_all <- fishyplots::lw_predictions |> 
    filter(species == .data$common_name | species == .data$scientific_name, survey %in% unique(spec.data$survey)) 
  
#### ANNOTATIONS ####
# create dataset for parameter annotations
annotations <- fishyplots::lw_predictions |>
  filter(.data$common_name == unique(spec.data$common_name), .data$survey %in% (spec.data$survey)) |>
  filter(.data$sex %in% c("M", "F")) |> # removes Unsexed
  select(.data$survey, .data$sex, .data$a, .data$b) |>
  distinct() |> 
  mutate(sex.label = ifelse(.data$sex == "M", "Male", "Female")) |> # col to have M and F spelled out
  mutate(
    label = paste0(.data$sex.label, ": a = ", format(.data$a, digits = 3, scientific = TRUE),
                   "  b = ", format(round(.data$b, 2), nsmall = 2))) # add formatted parameter labels
  

#rename for labeling
annotations$survey <-  factor(annotations$survey, levels = c("AK BSAI", "AK GULF", "PBS", "NWFSC"),
                              labels = c("Aleutians/Bering Sea", "Gulf of Alaska", "Canada", "U.S. West Coast"))
spec.data$survey <- factor(spec.data$survey, levels = c("AK BSAI", "AK GULF", "PBS", "NWFSC"),
                      labels = c("Aleutians/Bering Sea", "Gulf of Alaska", "Canada", "U.S. West Coast"))
predict_all$survey <- factor(predict_all$survey, levels = c("AK BSAI", "AK GULF", "PBS", "NWFSC"),
                                labels = c("Aleutians/Bering Sea", "Gulf of Alaska", "Canada", "U.S. West Coast"))

#### PLOTTING #### 
#set M vs F color
sex.color <- c("M" = "#E69F00", "F" = "#009E73")

#plot scatter subset data and model prediction line
  plot <- ggplot(spec.data, aes(x = .data$length_cm, y = .data$weight_kg)) +
    theme_bw() + #remove grid
    geom_point(aes(color = .data$sex), alpha = 0.1) +
    geom_line(data = predict_all, 
              aes(x = .data$fit_length, y = .data$fit_weight, color = .data$sex), linewidth = 1) + # plot fit lines
    
  # toggle visual settings for line and points
    # scale_linetype_manual(
    #   values = c("M" = "dashed", "F" = "solid"),
    #   labels = c("M" = "Male", "F" = "Female")) + 
    # ##scale_shape_manual(
    #   values = c("M" = 24, "F" = 21),
    #   labels = c("M" = "Male", "F" = "Female")) +
    scale_color_manual(
      values = sex.color,
      labels = c("M" = "Male", "F" = "Female")) +
    scale_fill_manual(
      values = sex.color,
      labels = c("M" = "Male", "F" = "Female")) +
      
#### LEGEND AND LABELS ####
    #add legend for lines
    theme(legend.title = element_blank(),
          panel.grid = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.subtitle = element_text(color = "gray40", size = 10, face = "italic")) + # tilt years to reduce overlapping text, remove legend
    xlab("Length (cm)") +
    ylab("Weight (kg)")+ 
    ggtitle("Length-Weight") +
    #ggtitle(unique(spec.data$common_name)) +
    #guides(color = guide_legend(override.aes = list(alpha = 0.5)))+ # increase alpha of legend
    geom_label(
      data = dplyr::filter(annotations, .data$sex == "M"),
      aes(x = -Inf, y = Inf, color = .data$sex, fill = .data$sex, label = .data$label),
      hjust = -0.1, vjust = 3,
      alpha = 0.2, inherit.aes = FALSE, show.legend = FALSE, size = 3.3
    ) +
    geom_label(
      data = dplyr::filter(annotations, .data$sex == "F"),
      aes(x = -Inf, y = Inf, color = .data$sex, fill = .data$sex, label =.data$label),
      hjust = -0.1, vjust = 2,
      alpha = 0.2, inherit.aes = FALSE, show.legend = FALSE, size = 3.3
    ) 
    
    
  ### add ons: 
    if(subset == TRUE)( plot <-  plot + ggtitle("Length-Weight", subtitle = "plot max n = 10000"))
  
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
            data = data.frame(survey =factor(empty_surveys,
                                               levels = levels(spec.data$survey))),
            aes(x = mean(range(spec.data$length_cm)), y = mean(range(spec.data$weight_kg)), label = "No data"),
            inherit.aes = FALSE)
      }
    }
    else if (facet_all == FALSE) {
      plot <- plot + facet_wrap(~survey, ncol = 4)
    }}
  
  
  return(plot)
  }
