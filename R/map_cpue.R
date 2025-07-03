#' Function for CPUE map for US coast data
#'
#' @param data catch data from pull_catch(), containing cpue and long/lat coordinates
#' @param map_type choose either "static" or "dynamic" 
#' @return a ggplot object
#' @importFrom scales trans_new
#' @import rnaturalearthdata
#' @importFrom rnaturalearth ne_countries
#' @importFrom sf st_crop st_transform st_as_sf st_coordinates
#' @importFrom ggplot2 ggplot geom_sf stat_summary_hex aes scale_fill_viridis_c coord_sf theme_bw labs
#' @importFrom gganimate transition_states animate
#' @export
#'
#' @examples
#' \dontrun{
#' map_cpue(catch_data, map_type = "static")
#' }
map_cpue <- function(data, map_type) {
  # Fourth root transform for scale
  fourth_root <- trans_new(
    name = "fourth_root",
    transform = function(x) x^(1/4),
    inverse = function(x) x^4
  )
  
  # Getting map data
  map_data <- ne_countries(scale = "medium", returnclass = "sf", country = "united states of america")
  us_coast <- suppressWarnings(suppressMessages(st_crop(map_data, c(xmin = -128, xmax = -116, ymin = 32, ymax = 48))))
  us_coast <- st_transform(us_coast, crs = 3157)
  # Creating SF object for catch data, setting and transforming the CRS, saving CRS in columns of original data frame
  catch_sf <- st_as_sf(catch, coords = c("Longitude_dd", "Latitude_dd"), crs = 4326)
  catch_3157 <- st_transform(catch_sf, crs = 3157)
  coords <- st_coordinates(catch_3157)
  catch$X <- coords[,1]
  catch$Y <- coords[,2]
  
  
  # Plotting static/dynamic maps
  if (map_type == "static") {
    map <- ggplot() +
      geom_sf(data = us_coast, color = "black") +
      stat_summary_hex(data = catch, aes(x = X, y = Y, z = cpue_kg_km2), bins = 50, fun = "mean") +
      scale_fill_viridis_c(name = "CPUE (kg/km\u00B2)", trans = fourth_root, option = "magma") +
      coord_sf(crs = 3157) +
      theme_bw() +
      labs(title = "CPUE 2003-2024", x = "", y = "")
    return(map)
  } else if (map_type == "dynamic") {
    gif <- ggplot() +
      geom_sf(data = us_coast, color = "black") +
      stat_summary_hex(data = catch, aes(x = X, y = Y, z = cpue_kg_km2), bins = 50, fun = "mean") +
      scale_fill_viridis_c(name = "CPUE (kg/km\u00B2)", trans = fourth_root, option = "magma") +
      coord_sf(crs = 3157) +
      theme_bw() +
      transition_states(Year, transition_length = 0, state_length = 1) +
      labs(title = "CPUE {closest_state}", x = "", y = "")
    
    # 150 frames is 150/10 = 15 seconds, with 10 frames per second
    # Each state takes up 1 frame, with no gradual transitions
    animate(gif, nframes = 150, fps = 10)
  } else {
    message("Invalid map type.")
  }
  
}