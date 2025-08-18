
map_cpue <- function(data, type = "static") {
  if ("Year" %in% colnames(data)) {
    data <- data |> rename(year = Year, lon_start = Longitude_dd, lat_start = Latitude_dd)
    data$region <- "nwfsc"
  }
  if (!"cpue_kg_km2" %in% colnames(data)) {
    data <- data |> mutate(cpue_kg_km2 = catch_weight / (effort * 0.01))
  }
  
  region <- unique(data$region)[1]
  
  data <- data |>
    filter(!is.na(lat_start)) |>
    filter(!is.na(lon_start))
  
  min_year <- min(data$year)
  max_year <- max(data$year)
  
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
  }
  
  else if (region == "afsc"){
    # Alaska base map
    crs = 32602
    map_data <- ne_countries(scale = "medium", returnclass = "sf", country = "united states of america")
    AK_map <- suppressWarnings(suppressMessages(
      st_crop(map_data, c(xmin = -180, xmax = -129, ymin = 50, ymax = 72))))
    proj <- st_transform(AK_map, crs = crs)
  }
  
  else if (region == "pbs"){
    # Canada base map
    crs = 32610
    CAN_map_data <- ne_countries(scale = "medium", returnclass = "sf", country = "canada")
    CAN_map <- st_crop(CAN_map_data, c(xmin = -140, xmax = -120, ymin = 48, ymax = 55))
    proj <- st_transform(CAN_map, crs = crs)
  }
  
  data <- add_utm_columns(dat = data, ll_names = c("lon_start", "lat_start"), utm_names = c("X", "Y"), utm_crs = crs, units = "km")
  
  map <- ggplot() +
    geom_sf(data = proj) +
    stat_summary_hex(data = data, aes(x = X*1000, y = Y*1000, z = cpue_kg_km2), bins = 50) +
    scale_fill_viridis_c(trans = fourth_root, option = "magma", name = "CPUE (kg/km\u00B2)") +
    theme_bw() +
    labs(x = "", y = "", title = paste0("CPUE ", min_year, "-", max_year),
         caption = "Note: color scale is fourth-root transformed.")
  
  if (type == "static") {
    return(map)
  }
  else if (type == "dynamic") {
    gif <- map +
      transition_states(year, transition_length = 0, state_length = 1) +
      labs(x = "", y = "", title = "CPUE {closest_state}")
    
    return(animate(gif, nframes = 150, fps = 10))
  }
}

catch1 <- get_data(regions = "afsc", common = "sablefish")
map_cpue(catch1)

catch2 <- get_data(regions = "pbs", common = "arrowtooth flounder")
map_cpue(catch2, type = "dynamic")

catch3 <- pull_catch(survey = "NWFSC.Combo", common_name = "arrowtooth flounder")
map_cpue(catch3)
