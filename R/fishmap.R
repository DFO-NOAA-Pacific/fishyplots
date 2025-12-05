#' Function for modeled CPUE map based on prediction data
#'
#' @param data prediction data from fishfit scripts
#' @param subregion choose AK BSAI, AK GULF, PBS, NWFSC
#' @param common_name species common name 
#' @return a ggplot object
#' @importFrom scales trans_new
#' @importFrom dplyr bind_rows
#' @importFrom rnaturalearth ne_countries ne_states
#' @importFrom sf st_crop st_transform
#' @importFrom ggplot2 ggplot geom_sf stat_summary_hex aes scale_fill_viridis_c theme_minimal labs geom_sf_text coord_sf
#' @importFrom patchwork wrap_plots
#' @importFrom stringr str_wrap
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' data(predictions_afsc)
#' data(predictions_pbs)
#' data(predictions_nwfsc)
#' data <- bind_rows(predictions_afsc, predictions_pbs, predictions_nwfsc)
#' 
#' fishmap(data, c("AK BSAI", "AK GULF", "PBS", "NWFSC"), "arrowtooth flounder")
#' fishmap(data, "PBS", "dover sole")
#' }
fishmap <- function(data, subregion = c("NWFSC", "PBS", "AK BSAI", "AK GULF"), common_name) {
  # 
  if(all(c("AK BSAI", "AK GULF") %in% subregion)) {
    subregion <- setdiff(subregion, c("AK BSAI", "AK GULF"))
    subregion <- unique(c("AFSC", subregion))
  }
  
  # Clean data
  rrr <- unique(ifelse(subregion %in% c("AK BSAI", "AK GULF"), "AFSC", subregion))
  data <- data |> 
    filter(.data$species == common_name) |>
    filter(.data$sanity != FALSE) |>
    filter(.data$region %in% rrr)
  
  if (nrow(data) == 0) {
    return(ggplot() + theme_void() + ggtitle(paste("No data for", common_name,"in this region.")))
    #stop(paste0("No data for ", common_name, " in this region."))
  }
  
  
  # Color scale transformation
  fourth_root <- trans_new(
    name = "fourth_root",
    transform = function(x) x^(1/4),
    inverse = function(x) x^4
  )
  
  # Creating empty list to store map data
  plot_list <- list()
  
  # Map data
  states <- ne_states(country = "united states of america", returnclass = "sf", scale = 50)
  canada <- ne_states(returnclass = "sf", country = "canada", scale = 50) |> select(.data$name, .data$geometry)
  mexico <- ne_countries(scale = "medium", returnclass = "sf", country = "mexico") |> select(name = .data$admin, .data$geometry)
  alaska <- states |> filter(.data$name == "Alaska")
  
  # Loop for mapping
  for (i in subregion) {
    if (i == "NWFSC"){ 
      combined <- bind_rows(states, canada, mexico)
      proj <- st_transform(combined, crs = 3157)
      proj <- suppressWarnings(st_crop(proj, c(xmin = -1000000, ymin = 3350000, xmax = 1200000, ymax = 5600000)))
      year <- "2023"
      name <- "US West Coast"
      caption <- "Survey year: 2024."
    }
    else if (i == "AK BSAI" | i == "AK GULF" | i == "AFSC"){
      combined <- bind_rows(alaska, canada, states)
      proj <- st_transform(combined, crs = 32602)
      proj <- suppressWarnings(st_crop(proj, c(xmin = -606409.9, ymin = 3190000, xmax = 3250000, ymax = 8000000)))
      
      if (i == "AFSC") {
        year <- "2023/2024"
        name <- "Alaska"
        caption <- str_wrap("Survey year: Gulf of Alaska (2023), northern Bering Sea (2023), eastern Bering Sea (2024), Aleutian Islands (2024).", 45)
      } else if (i == "AK BSAI") {
        year <- "2023/2024"
        name <- "Aleutians/Bering Sea"
        caption <- str_wrap("Survey year: northern Bering Sea (2023), eastern Bering Sea (2024), Aleutian Islands (2024).", 45)
      } else if (i == "AK GULF") {
        year <- "2023"
        name <- "Gulf of Alaska"
        caption <- "Survey year: 2023."
      }
      
    } 
    else if (i == "PBS"){ 
      alaska <- states |> filter(name %in% "Alaska")
      combined <- bind_rows(states, canada, alaska)
      proj <- st_transform(combined, crs = 32610)
      proj <- suppressWarnings(st_crop(proj, c(xmin = -5336622, ymin = 5150000, xmax = 850000, ymax = 6300000)))
      year <- "2022/2023"
      name <- "Canada"
      caption <- str_wrap("Survey year: Queen Charlotte Sound (2023), Hecate Strait (2023), Haida Gwaii (2022), Vancouver Island (2022).", 45)
    }
    
    
    if (i == "AK GULF") {
      subset <- data |> filter(.data$survey == "Gulf of Alaska Bottom Trawl Survey")
    } else if (i == "AK BSAI") {
      subset <- data |> filter(.data$survey != "Gulf of Alaska Bottom Trawl Survey")
    } else {
      subset <- data |> filter(.data$region == i)
    }
    
    
    # Removing outliers
    .q <- quantile(data$prediction, probs = 0.998, na.rm = TRUE)[[1]]
    data$prediction[data$prediction > .q] <- .q
    
    # Constructing map
    p <- ggplot() +
      stat_summary_hex(data = subset, aes(x = .data$X*1000, y = .data$Y*1000, z = .data$prediction), bins = 50) +
      scale_fill_viridis_c(trans = fourth_root, option = "magma", name = "CPUE (kg/km\u00B2)") +
      geom_sf(data = proj) +
      coord_sf(expand = FALSE) +
      geom_sf_text(data = proj, aes(label = .data$name), size = 2.7, fontface = "bold", check_overlap = TRUE) +
      theme_minimal() +
      labs(x = "", y = "", title = paste0("Predicted Density ", name),
           caption = paste0("Note: color scale is fourth-root transformed.\n ", caption))
    
    # Adding map to the list
    plot_list[[length(plot_list) + 1]] <- p
  }
  
  # Patchwork
  return(wrap_plots(plot_list, ncol = 1))
}
