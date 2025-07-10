#' Main function to display survey specimen counts
#'
#' @param data a data frame with year, estimated biomass, lower and upper SE
#' @return a ggplot object
#' @importFrom nwfscSurvey pull_catch CreateStrataDF.fn get_design_based plot_index
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point theme_bw scale_y_continuous xlab ylab
#' @export
#'
#' @examples
#' \dontrun{
#' plot_dbi(biomass.data)
#' }

#example data
catch = pull_catch(
  common_name = "Pacific ocean perch", 
  survey = "NWFSC.Combo")

strata <- CreateStrataDF.fn(
  names = c("shallow_s", "deep_s", "shallow_n", "deep_n"), 
  depths.shallow = c(  55,  183,  55, 183),
  depths.deep    = c( 183,  400, 183, 400),
  lats.south     = c(34.5, 34.5,  42,  42),
  lats.north     = c(  42,   42,  49,  49)
)

biomass = get_design_based(
  data = catch,  
  strata = strata)

plot_index(
  data = biomass,
  plot = 1)


# DBI plot func (adapted from shiny version):
plot_dbi <- function(data) {
    plot <- ggplot2::ggplot(data = data) +
      ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = lwr, ymax = upr), fill = "lightgray") +
      ggplot2::geom_line(ggplot2::aes(x = year, y = est)) +
      ggplot2::geom_point(ggplot2::aes(x = year, y = est)) +
      ggplot2::ylab("Biomass (mt)") +
      ggplot2::xlab("Year") +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(
        labels = function(x) format(x, big.mark = ",", scientific = FALSE))
    return(plot)
  }
