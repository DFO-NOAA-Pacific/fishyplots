#' Main function to plot length-weight relationships by sex
#'
#' @param data region bio data to be plotted. Preload from data file, see examples.
#' @param species common or scientific name of target species
#' @param subset default TRUE for a faster plotting subset of n = 2000. Set FALSE for all available data.
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
  #load lw fit dataset
  data("lw_predictions")
  
  
  #subset data to species (spec.data), or give error message
  if (any(data$common_name == species | data$scientific_name == species)) {
    spec.data <- data  %>% 
      filter(species == common_name | species == scientific_name) %>%
      filter(!sex == "U", !is.na(length_cm), !is.na(weight_kg)) #remove U sex, rows with NA lengths or widths
 } else (
    #stop(paste("Species name", "'", species,"'", "not found in this dataset.")))
    stop(return(ggplot() + theme_void() + ggtitle("No length-weight data available."))))
  
  # when subset = TRUE, subset into 2000 random points for plotting speed
  if(subset == TRUE){
    spec.data <- spec.data %>% 
      group_by(survey) %>%
      slice_sample(n = 2000) %>%  # in case any region has < 2000
      ungroup()
    if(nrow(spec.data) < 2000) {message ("Note: Species data less than 2000 rows; plotting all data with no subset")}
    else {message (paste0("Note: Plotting a random n = 2000 subset of ", unique(spec.data$common_name), ". Model values not impacted."))}}
  
  
  # create predictions for each survey grouping in data
  predict_all <- data.frame()
  for(survey_name in unique(spec.data$survey)) {
  # get regression fit for m and f of species in data
  F_pred <- lw_predictions %>% 
    filter(survey == survey_name, species == common | species == scientific, sex == "F")
  M_pred <- lw_predictions %>% 
    filter(survey == survey_name, species == common | species == scientific, sex == "M")

  # make plotting predictions to max length
  pred_df_F <- data.frame(
    x = seq(min(subset(spec.data, spec.data$sex == "F")$length_cm),
            max(subset(spec.data, spec.data$sex == "F")$length_cm), 
            length.out = 100),
    survey = survey_name
  ) %>%
    mutate(y = F_pred$a * x^F_pred$b)
  pred_df_M <- data.frame(
    x = seq(min(subset(spec.data, spec.data$sex == "M")$length_cm),
            max(subset(spec.data, spec.data$sex == "M")$length_cm),
            length.out = 100),
    survey = survey_name
  ) %>%
    mutate(y = M_pred$a * x^M_pred$b)
  
  # combine for legend plotting
  predict_all <- rbind(predict_all,
    data.frame(fit = pred_df_M, sex = "M"),
    data.frame(fit = pred_df_F, sex = "F")
  ) 
  }
  # put into one dataset: predict_all for prediction data plotting
predict_all <- predict_all%>% 
  rename(survey = fit.survey)

#### ANNOTATIONS ####
# create dataset for parameter annotations
annotations <- lw_predictions %>%
  filter(common == unique(spec.data$common_name), survey %in% (spec.data$survey)) %>%
  filter(sex %in% c("M", "F")) %>%
  select(survey, sex, a, b) %>%
  mutate(sex.label = ifelse(sex == "M", "Male", "Female")) %>% 
  mutate(
    label = paste0(sex.label, ": a = ", format(a, digits = 3, scientific = TRUE),
                   "  b = ", round(b, 2)))
  

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
    theme_classic() + #remove grid
    geom_point(aes(color = sex), alpha = 0.1) +
    geom_line(data = predict_all, 
              aes(x = fit.x, y = fit.y, color = sex), linewidth = 1) + # plot fit lines
    
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
    if(subset == TRUE)( plot <-  plot + ggtitle("Length-Weight", subtitle = "subset of n ≤ 2000"))
  
  if(length(unique(spec.data$survey)) > 1) {
    plot <- plot + facet_wrap( ~ survey, nrow = 1) + theme(strip.background = element_blank())
  }
  
  return(plot)
  }
