#' Main function to plot length-weight relationships by sex
#'
#' @param data a data frame of target species including Length_cm, Weight_kg, and Sex
#' @param color default FALSE for greyscale, TRUE gives red and blue
#' @return a plot of sexed data with log regression slope and intercept
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_linetype_manual theme_classic theme element_blank element_text xlab ylab annotate
#' @importFrom dplyr filter mutate
#' @importFrom stats lm predict
#' @importFrom grid unit
#' @export
#'
#' @examples
#' \dontrun{
#' length_weight(bio.data)
#' }

length_weight <- function(data, color = FALSE) #color BW default
{ 
  # transform and regression
  #remove unsexed and NAs
  log.data <- data %>% 
    filter(!Sex == "U") %>% 
    filter(!is.na(Length_cm)) %>% 
    filter(!is.na(Weight_kg)) %>% 
    mutate(loglength = log(Length_cm), logweight = log(Weight_kg))
  
  #subset male and female for ease
  male <- subset(log.data, log.data$Sex == "M")
  female <- subset(log.data, log.data$Sex == "F")
  
  #regression
  lw_mod_M <- lm(logweight ~ loglength, data = male)
  lw_mod_F <- lm(logweight ~ loglength, data = female)
  
  #create regression predictions
  log_predict_M <- data.frame(predict(lw_mod_M, interval = "confidence"))
  log_predict_F <- data.frame(predict(lw_mod_F, interval = "confidence"))
  
  real_predict_M <- apply(X = log_predict_M, MARGIN = 2, FUN = exp)
  real_predict_F <- apply(X = log_predict_F, MARGIN = 2, FUN = exp)
  
  # combine for legend plotting
  real_predict_all <- rbind(
    data.frame(Length_cm = male$Length_cm, fit = real_predict_M, Sex = "M"),
    data.frame(Length_cm = female$Length_cm, fit = real_predict_F, Sex = "F")
  )
  
  
  
  #plot with or without color
  if(color == TRUE){
  sex.color <- c("M" = "#005AB5", "F" = "#DC3220")
  }
  else{
  sex.color <- c("M" = "grey60", "F" = "black")
  }
  
  plot <- ggplot(log.data, aes(x = Length_cm, y = Weight_kg)) +
    theme_classic() + #removes grid
    geom_point(aes(color = Sex, shape = Sex), alpha = 0.15) +
    #DATA
    
    geom_line(data = real_predict_all, 
              aes(x = Length_cm, y = fit.fit, linetype = Sex, color = Sex), linewidth = 1) + # plot fit lines
    
    #set M vs F color, shape, lines
    scale_linetype_manual(
      values = c("M" = "dashed", "F" = "solid"),
      labels = c("M" = "Male", "F" = "Female")) + 
    scale_shape_manual(
      values = c("M" = 2, "F" = 1),
      labels = c("M" = "Male", "F" = "Female")) +
    scale_color_manual(
      values = sex.color,
      labels = c("M" = "Male", "F" = "Female")) +
    
    #LEGEND AND LABELS
    #add legend for lines
    theme(legend.title = element_blank(), legend.position = c(0.9, 0.1), legend.text=element_text(size=10), legend.key.width = unit(1, 'cm')) + # legend position
    xlab("Length (cm)") +# for the x axis label
    ylab("Weight (kg)")+
    guides(color = guide_legend(override.aes = list(alpha = 1)))+ # increase alpha of legend
    
    # ANNOTATIONS
    # Add text annotations for slope/intercept
    annotate("text", x = -Inf, y = Inf, 
             label = paste0("Female", 
                            "\n a = ", format(exp(coef(lw_mod_F)[1]), digits = 3, scientific = TRUE),
                            "\n   b = ", round(coef(lw_mod_F)[2],2)), 
             hjust = -0.5, vjust = 2, size = 4) +
    annotate("text", x = -Inf, y = Inf, 
             label = paste0("  Male", 
                            "\n a = ", format(exp(coef(lw_mod_M)[1]), digits = 3, scientific = TRUE),
                            "\n   b = ", round(coef(lw_mod_M)[2],2)), 
             hjust = -0.5, vjust = 3.5, size = 4)
  
  
  return(plot)
}
