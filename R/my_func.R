#' Main function to summarize relationship between x and y
#'
#' @param data a data frame with columns x and y
#' @return a data frame of joined haul and catch data
#' @importFrom stats lm coef
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' df <- data.frame(x = 1:10, y = rnorm(10))
#' result <- my_func(df)
#' }
my_func <- function(data) {
  fit <- lm(y ~ x, data = data)
  return(coef(fit))
}