#' Main function to plot length-weight relationships by sex
#'
#' @param data a data frame of target species including Length_cm, Weight_kg, and Sex
#' @return a plot of sexed data with log regression slope and intercept
#' @importFrom ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' TBD
#' }

lgth.wght.fn <- function(data) #add more arguments as needed, such as CI
{ 
  # transform and regression
  log.data <- data %>% 
    filter(!is.na(Length_cm)) %>% 
    filter(!is.na(Weight_kg)) %>% 
    mutate(loglength = log(Length_cm), logweight = log(Weight_kg))
  
  #filter into male and female, no unsexed
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
    data.frame(Length_cm = male$Length_cm, fit = real_predict_M, Sex = "Male"),
    data.frame(Length_cm = female$Length_cm, fit = real_predict_F, Sex = "Female")
  )
  
  
  
  
  #plot
  plot <- ggplot(log.data, aes(x = Length_cm, y = Weight_kg)) +
    theme_classic() + #removes grid
    
    #DATA
    geom_point(data = male, aes(x = Length_cm, y = Weight_kg), color = "darkgrey", alpha = 0.15, pch = 1) + #plots raw male data
    geom_point(data = female, aes(x = Length_cm, y = Weight_kg), color = "darkgrey", alpha = 0.15, pch = 1) + #plots raw female data
    # could also make M vs F be red and blue, but overlay makes one sex more visible
    geom_line(data = real_predict_all, aes(x = Length_cm, y = fit.fit, linetype = Sex), linewidth = 1) + # plot fit lines
    scale_linetype_manual(values = c("Male" = "dashed", "Female" = "solid")) + 
    
    
    #LEGEND AND LABELS
    #add legend for lines
    theme(legend.title = element_blank(), legend.position = c(0.9, 0.1), legend.text=element_text(size=10), legend.key.width = unit(1, 'cm')) + # legend position
    xlab("Length (cm)") +# for the x axis label
    ylab("Weight (kg)")+
    
    # ANNOTATIONS
    # Add text annotations for slope/intercept
    annotate("text", x = -Inf, y = Inf, 
             label = paste0("Female", 
                            "\nln(a) = ", round(coef(lw_mod_F)[1],2),
                            "\n   b = ", round(coef(lw_mod_F)[2],2)), 
             hjust = -0.5, vjust = 2, size = 4) +
    annotate("text", x = -Inf, y = Inf, 
             label = paste0("  Male", 
                            "\nln(a) = ", round(coef(lw_mod_M)[1],2),
                            "\n   b = ", round(coef(lw_mod_M)[2],2)), 
             hjust = -0.5, vjust = 3.5, size = 4)
  
  
  return(plot)
}
