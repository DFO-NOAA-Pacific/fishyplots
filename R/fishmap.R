#' Function for modeled CPUE map based on prediction data
#'
#' @param data prediction data from fishfit scripts
#' @param region choose AFSC, PBS, NWFSC (choosing AK BSAI and AK GULF will default to AFSC)
#' @param common_name species common name 
#' @return a ggplot object
#' @importFrom scales trans_new
#' @importFrom rnaturalearth ne_countries ne_states
#' @importFrom sf st_crop st_transform
#' @importFrom ggplot2 ggplot geom_sf stat_summary_hex aes scale_fill_viridis_c theme_bw labs
#' @importFrom patchwork wrap_plots
#' @export
#'
#' @examples
#' \dontrun{
#' data(predictions_afsc)
#' data(predictions_pbs)
#' data(predictions_nwfsc)
#' data <- rbind(predictions_afsc, predictions_pbs, predictions_nwfsc)
#' 
#' fishmap(data, c("AFSC", "PBS", "NWFSC"), "arrowtooth flounder")
#' fishmap(data, c("AK BSAI", "NWFSC"), "sablefish")
#' fishmap(data, "PBS", "dover sole")
#' }
fishmap <- function(data, region = c("AFSC", "NWFSC", "PBS", "AK BSAI", "AK GULF"), common_name) {
  # Reformat survey to region
  region <- unique(ifelse(region %in% c("AK BSAI", "AK GULF"), "AFSC", region))
  
  # Clean data
  data <- data |> 
    filter(species == common_name) |>
    filter(sanity != FALSE) |>
    rename(r = region) |>
    filter(r %in% region)
  
  if (nrow(data) == 0) {
    stop(paste0("No data for ", common_name, " in ", region, "."))
  }
  
  # Color scale transformation
  fourth_root <- trans_new(
    name = "fourth_root",
    transform = function(x) x^(1/4),
    inverse = function(x) x^4
  )
  
  # Creating empty list to store map data
  plot_list <- list()
  
  # Loop for mapping
  for (i in region) {
    if (i == "NWFSC"){
      # US coast base map
      crs = 3157
      states <- ne_states(country = "united states of america", returnclass = "sf")
      west_coast <- states |> filter(name %in% c("California", "Oregon", "Washington")) |>
        select(name, geometry)
      west_coast <- st_crop(west_coast, c(xmin = -117, xmax = -130, ymin = 30, ymax = 50))
      canada <- ne_countries(scale = "medium", returnclass = "sf", country = "canada") |> select(name = admin, geometry)
      canada <- st_crop(canada, c(xmin = -128, xmax = -117, ymin = 45, ymax = 50))
      combined <- rbind(west_coast, canada)
      year <- "2023"
    }
    else if (i == "AFSC"){
      # Alaska base map
      crs = 32602
      alaska <- ne_countries(scale = "medium", returnclass = "sf", country = "united states of america")
      alaska <- suppressWarnings(suppressMessages(
        st_crop(alaska, c(xmin = -180, xmax = -129, ymin = 50, ymax = 72))))
      canada <- ne_countries(scale = "medium", returnclass = "sf", country = "canada")
      canada <- st_crop(canada, c(xmin = -150, xmax = -130, ymin = 51, ymax = 70))
      combined <- rbind(alaska, canada)
      year <- "2023/2024"
    } 
    else if (i == "PBS"){
      # Canada base map
      crs = 32610
      canada <- ne_countries(scale = "medium", returnclass = "sf", country = "canada")
      canada <- st_crop(canada, c(xmin = -140, xmax = -120, ymin = 48, ymax = 55))
      combined <- canada
      year <- "2022/2023"
    }
    
    proj <- st_transform(combined, crs = crs)
    subset <- data |> filter(r == i)
    
    # Removing outliers
    .q <- quantile(data$prediction, probs = 0.998, na.rm = TRUE)[[1]]
    data$prediction[data$prediction > .q] <- .q
    
    # Constructing map
    p <- ggplot() +
      stat_summary_hex(data = subset, aes(x = X*1000, y = Y*1000, z = prediction), bins = 50) +
      scale_fill_viridis_c(trans = fourth_root, option = "magma", name = "CPUE (kg/km\u00B2)") +
      geom_sf(data = proj) +
      theme_bw() +
      labs(x = "", y = "", title = paste0("Predicted Density ", year),
           caption = "Note: color scale is fourth-root transformed.")
    
    # Adding map to the list
    plot_list[[length(plot_list) + 1]] <- p
  }
  
  # Conditional for patchwork
  if (length(plot_list) == 1) {
    return(plot_list[[1]])
  } else {
    return(wrap_plots(plot_list, ncol = 1))
  }
}
