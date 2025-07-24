#' Function for modeled CPUE map based on prediction data
#'
#' @param data prediction data from fishfit scripts
#' @param common_name species common name 
#' @return a ggplot object
#' @importFrom scales trans_new
#' @importFrom rnaturalearth ne_countries ne_states
#' @importFrom sf st_crop st_transform
#' @importFrom ggplot2 ggplot geom_sf stat_summary_hex aes scale_fill_viridis_c theme_bw labs
#' @export
#'
#' @examples
#' \dontrun{
#' data(predictions_afsc)
#' fishmap(predictions_afsc, "arrowtooth flounder")
#' }
fishmap <- function(data, common_name) {
  # Clean data
  data <- data |> 
    filter(species == common_name)
  if (nrow(data) == 0) {
    stop(paste0("No data for ", common_name, ". Check species spelling."))
  }
  region <- unique(data$region)[1]
  
  # Color scale transformation
  fourth_root <- trans_new(
    name = "fourth_root",
    transform = function(x) x^(1/4),
    inverse = function(x) x^4
  )
  
  # Base map construction
  if (region == "nwfsc"){
    # US coast base map
    crs = 3157
    states <- ne_states(country = "united states of america", returnclass = "sf")
    west_coast <- states |> filter(name %in% c("California", "Oregon", "Washington")) |>
      select(name, geometry)
    west_coast <- st_crop(west_coast, c(xmin = -117, xmax = -130, ymin = 30, ymax = 50))
    
    canada <- ne_countries(scale = "medium", returnclass = "sf", country = "canada") |> select(name = admin, geometry)
    canada <- st_crop(canada, c(xmin = -128, xmax = -117, ymin = 45, ymax = 50))
    
    combined <- rbind(west_coast, canada)
    proj <- st_transform(combined, crs = crs)
    year <- "2023"
  }
  
  else if (region == "afsc"){
    # Alaska base map
    crs = 32602
    alaska <- ne_countries(scale = "medium", returnclass = "sf", country = "united states of america")
    alaska <- suppressWarnings(suppressMessages(
      st_crop(alaska, c(xmin = -180, xmax = -129, ymin = 50, ymax = 72))))
    canada <- ne_countries(scale = "medium", returnclass = "sf", country = "canada")
    canada <- st_crop(canada, c(xmin = -150, xmax = -130, ymin = 51, ymax = 70))
    combined <- rbind(alaska, canada)
    proj <- st_transform(combined, crs = crs)
    year <- "2023/2024"
  }
  
  else if (region == "pbs"){
    # Canada base map
    crs = 32610
    canada <- ne_countries(scale = "medium", returnclass = "sf", country = "canada")
    canada <- st_crop(canada, c(xmin = -140, xmax = -120, ymin = 48, ymax = 55))
    proj <- st_transform(canada, crs = crs)
    year <- "2022/2023"
  }
  
  # Removing outliers
  .q <- quantile(data$prediction, probs = 0.998, na.rm = TRUE)[[1]]
  data$prediction[data$prediction > .q] <- .q
  
  # Mapping
  map <- ggplot() +
    stat_summary_hex(data = data, aes(x = X*1000, y = Y*1000, z = prediction), bins = 50) +
    scale_fill_viridis_c(trans = fourth_root, option = "magma", name = "CPUE (kg/km\u00B2)") +
    geom_sf(data = proj) +
    theme_bw() +
    labs(x = "", y = "", title = paste0("Predicted Density ", year),
         caption = "Note: color scale is fourth-root transformed.")
  
  return(map)
}