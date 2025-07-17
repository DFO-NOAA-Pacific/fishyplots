#' Function for modeled CPUE map based on prediction data
#'
#' @param data prediction data from fishfit script
#' @param common_name species common name 
#' @return a ggplot object
#' @importFrom scales trans_new
#' @importFrom rnaturalearth ne_countries
#' @importFrom sf st_crop st_transform
#' @importFrom ggplot2 ggplot geom_sf stat_summary_hex aes scale_fill_viridis_c theme_bw labs
#' @export
#'
#' @examples
#' \dontrun{
#' fishmap(afsc_predictions, "arrowtooth flounder")
#' }
fishmap <- function(data, common_name) {
  data <- data |> 
    filter(species == common_name)
  if (nrow(data) == 0) {
    stop(paste0("No data for ", common_name, ". Check species spelling."))
  }
  region <- unique(data$region)[1]
  
  fourth_root <- trans_new(
    name = "fourth_root",
    transform = function(x) x^(1/4),
    inverse = function(x) x^4
  )
  
  if (region == "nwfsc"){
    # US coast base map
    crs = 3157
    map_data <- ne_countries(scale = "medium", returnclass = "sf", country = "united states of america")
    US_map <- suppressWarnings(suppressMessages(
      st_crop(map_data, c(xmin = -128, xmax = -116, ymin = 32, ymax = 48))))
    proj <- st_transform(US_map, crs = crs)
    year <- "2023"
  }
  
  else if (region == "afsc"){
    # Alaska base map
    crs = 32602
    map_data <- ne_countries(scale = "medium", returnclass = "sf", country = "united states of america")
    AK_map <- suppressWarnings(suppressMessages(
      st_crop(map_data, c(xmin = -180, xmax = -129, ymin = 50, ymax = 72))))
    proj <- st_transform(AK_map, crs = crs)
    year <- "2023/2024"
  }
  
  else if (region == "pbs"){
    # Canada base map
    crs = 32610
    CAN_map_data <- ne_countries(scale = "medium", returnclass = "sf", country = "canada")
    CAN_map <- st_crop(CAN_map_data, c(xmin = -140, xmax = -120, ymin = 48, ymax = 55))
    proj <- st_transform(CAN_map, crs = crs)
    year <- "2022/2023"
  }
  
  map <- ggplot() +
    geom_sf(data = proj) +
    stat_summary_hex(data = data, aes(x = X*1000, y = Y*1000, z = prediction), bins = 50) +
    scale_fill_viridis_c(trans = fourth_root, option = "magma", name = "CPUE (kg/km\u00B2)") +
    theme_bw() +
    labs(x = "", y = "", title = paste0("Model Predicted CPUE ", year),
         caption = "Note: color scale is fourth-root transformed.")
  
  return(map)
}
