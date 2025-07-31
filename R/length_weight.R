#' Main function to plot length-weight relationships by sex
#'
#' @param data region bio data to be plotted. Preload from data file, see examples.
#' @param species common or scientific name of target species
#' @param subset default TRUE for a faster plotting subset of n = 10000. Set FALSE for all available data.
#' @return a plot of sexed data with log regression slope and intercept
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_linetype_manual theme_classic theme element_blank element_text xlab ylab annotate
#' @importFrom dplyr filter mutate
#' @importFrom grid unit
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
#' length_weight(all_data, "arrowtooth flounder")
#' length_weight(akgulf_bio, "atka mackerel", subset = FALSE)
#' }

length_weight <- function(data, species, subset = TRUE) { # subset default

#### DATA ####
  #load lw prediction dataset
  data("lw_predictions")
  
  
  #subset data to species (spec.data), or give error message
  if (any(data$common_name == species | data$scientific_name == species)) {
    spec.data <- data  %>% 
      filter(species == common_name | species == scientific_name) %>%
      filter(!sex == "U", !is.na(length_cm), !is.na(weight_kg)) #remove U sex, rows with NA lengths or widths
 } else (
    #stop(paste("Species name", "'", species,"'", "not found in this dataset.")))
    stop(return(ggplot() + theme_void() + ggtitle("No length-weight data available."))))
  
  # when subset = TRUE, subset into 10000 random points for plotting speed
  if(subset == TRUE){
    spec.data <- spec.data %>% 
      group_by(survey) %>%
      slice_sample(n = 10000) %>%  # in case any region has < 10000
      ungroup()
    if(nrow(spec.data) < 10000) {message ("Note: Species data less than 10000 rows; plotting all data with no subset")}
    else {message (paste0("Note: Plotting a random n = 10000 subset of ", unique(spec.data$common_name), ". Model values not impacted."))}}
  
# filter predictions to species
  predict_all <- lw_predictions %>% 
    filter(species == common_name | species == scientific_name, survey %in% unique(spec.data$survey)) 
  
#### ANNOTATIONS ####
# create dataset for parameter annotations
annotations <- lw_predictions %>%
  filter(common_name == unique(spec.data$common_name), survey %in% (spec.data$survey)) %>%
  filter(sex %in% c("M", "F")) %>%
  select(survey, sex, a, b) %>%
  distinct() %>% 
  mutate(sex.label = ifelse(sex == "M", "Male", "Female")) %>% 
  mutate(
    label = paste0(sex.label, ": a = ", format(a, digits = 3, scientific = TRUE),
                   "  b = ", format(round(b, 2), nsmall = 2)))
  

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
  plot <- ggplot(spec.data, aes(x = length_cm, y = weight_kg)) +
    theme_bw() + #remove grid
    geom_point(aes(color = sex), alpha = 0.1) +
    geom_line(data = predict_all, 
              aes(x = fit_length, y = fit_weight, color = sex), linewidth = 1) + # plot fit lines
    
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
      data = dplyr::filter(annotations, sex == "M"),
      aes(x = -Inf, y = Inf, color = sex, fill = sex, label = label),
      hjust = -0.1, vjust = 3,
      alpha = 0.2, inherit.aes = FALSE, show.legend = FALSE, size = 3.3
    ) +
    geom_label(
      data = dplyr::filter(annotations, sex == "F"),
      aes(x = -Inf, y = Inf, color = sex, fill = sex, label =label),
      hjust = -0.1, vjust = 2,
      alpha = 0.2, inherit.aes = FALSE, show.legend = FALSE, size = 3.3
    ) 
    
    # geom_text( data = annotations, # annotate with a and b values
    #   aes(x = -Inf, y = Inf, label = label, vjust = ifelse(sex == "F", 1.2, 2.5)),  hjust = -0.2, # placement
    #   size = 3.5, inherit.aes = FALSE)
    
    
  ### add ons: 
    if(subset == TRUE)( plot <-  plot + ggtitle("Length-Weight", subtitle = "plot max n = 10000"))
  
  if(length(unique(spec.data$survey)) > 1) {
    plot <- plot + facet_wrap( ~ survey, nrow = 1, drop = FALSE)
  }
  
  return(plot)
  }
