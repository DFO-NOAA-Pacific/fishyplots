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
#' all.data <- bind_rows(akgulf_bio, akbsai_bio, nwfsc_bio, pbs_bio)
#' 
#' length_weight(akgulf_bio, "atka mackerel")
#' }

length_weight <- function(data, species, subset = TRUE) { # subset default

  sex.color <- c("M" = "#E69F00", "F" = "#009E73")
  
  #load fit dataset
  data("lw_predictions")
  
  #subset data to species
  if (any(data$common_name == species | data$scientific_name == species)) {
    spec.data <- data  %>% 
      filter(species == common_name | species == scientific_name) %>%
      filter(!sex == "U") |>
      filter(!is.na(length_cm)) %>%
      filter(!is.na(weight_kg)) }
  else (stop(paste("Species name", "'", species,"'", "not found in this dataset.")))
  
  if(subset == TRUE){
    spec.data <- spec.data %>% slice_sample(n = 2000)
    if(nrow(spec.data) < 2000) {message ("Note: Species data less than 2000 rows; plotting all data with no subset")}
    else {message (paste0("Note: subset = TRUE ; plotting a random n = 2000 subset of ", unique(spec.data$common_name), ". Model values not impacted."))}
  }
  predict_all <- data.frame()
  for(survey_name in unique(spec.data$survey)) {
  # get regression fit for m and f of species in data
  F_pred <- lw_predictions %>% 
    filter(survey == survey_name, species == common | species == scientific, sex == "F")
  M_pred <- lw_predictions %>% 
    filter(survey == survey_name, species == common | species == scientific, sex == "M")
  
  
  pred_df_F <- data.frame(
    x = seq(min(spec.data$length_cm), max(spec.data$length_cm), length.out = 100),
    survey = survey_name
  ) %>%
    mutate(y = F_pred$a * x^F_pred$b)
  pred_df_M <- data.frame(
    x = seq(min(spec.data$length_cm), max(spec.data$length_cm), length.out = 100),
    survey = survey_name
  ) %>%
    mutate(y = M_pred$a * x^M_pred$b)
  
  # combine for legend plotting
  predict_all <- rbind(predict_all,
    data.frame(fit = pred_df_M, sex = "M"),
    data.frame(fit = pred_df_F, sex = "F")
  ) 
  }
predict_all <- predict_all%>% 
  rename(survey = fit.survey)
  
### annotations 
annotations <- lw_predictions %>%
  filter(common == unique(spec.data$common)) %>%
  filter(sex %in% c("M", "F")) %>%
  select(survey, sex, a, b) %>%
  mutate(
    label = paste0(sex, "\na = ", format(a, digits = 3, scientific = TRUE),
                   "\nb = ", round(b, 2)))


  
  plot <- ggplot(spec.data, aes(x = length_cm, y = weight_kg)) +
    theme_classic() + #removes grid
    geom_jitter(aes(color = sex, shape = sex, fill = sex), alpha = 0.1) +
    #DATA
    geom_line(data = predict_all, 
              aes(x = fit.x, y = fit.y, linetype = sex, color = sex), linewidth = 1) + # plot fit lines
    
    #set M vs F color, shape, lines, fill
    scale_linetype_manual(
      values = c("M" = "dashed", "F" = "solid"),
      labels = c("M" = "Male", "F" = "Female")) + 
    scale_shape_manual(
      values = c("M" = 24, "F" = 21),
      labels = c("M" = "Male", "F" = "Female")) +
    scale_color_manual(
      values = sex.color,
      labels = c("M" = "Male", "F" = "Female")) +
    scale_fill_manual(
      values = sex.color,
      labels = c("M" = "Male", "F" = "Female")) +
    
    facet_wrap( ~ survey, nrow = 1) +
    #LEGEND AND LABELS
    #add legend for lines
    theme(legend.title = element_blank(), legend.position.inside  = c(0.9, 0.1), legend.text=element_text(size=10), legend.key.width = unit(1, 'cm')) + # legend position
    xlab("Length (cm)") +# for the x axis label
    ylab("Weight (kg)")+
    #ggtitle(unique(spec.data$common_name)) +
    guides(color = guide_legend(override.aes = list(alpha = 0.5)))+ # increase alpha of legend
    geom_text(
      data = annotations,
      aes(x = -Inf, y = Inf, label = label,
          vjust = ifelse(sex == "F", 1.2, 2.5)),  # mapped inside aes()
      hjust = -0.2,
      size = 3.5,
      inherit.aes = FALSE
    )
  return(plot)
 
  }
