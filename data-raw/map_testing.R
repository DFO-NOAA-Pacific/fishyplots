library(sdmTMB)
library(surveyjoin)
library(dplyr)
library(stringr)
library(arrow)
library(scales)
library(rnaturalearth)
library(sf)
library(ggplot2)
library(gganimate)
library(plotly)

# Read in species to run models for
spp_list <- read.csv("data-raw/nwfsc_joined.csv")
spp_list <- spp_list |> filter(common_name != "north pacific spiny dogfish")
spp_list$common_name[spp_list$common_name == "north pacific hake"] <- "pacific hake"

# Catch data
temp_file <- tempfile(fileext = ".rds")
# Download the .rds file from GitHub (raw)
download.file(
  "https://raw.githubusercontent.com/DFO-NOAA-Pacific/surveyjoin-data/main/nwfsc-catch-all.rds",
  destfile = temp_file,
  mode = "wb"  # Important: write as binary!
)
catch_data <- readRDS(temp_file)

# Download the .rds file from GitHub (raw)
download.file(
  "https://raw.githubusercontent.com/DFO-NOAA-Pacific/surveyjoin-data/main/nwfsc-haul.rds",
  destfile = temp_file,
  mode = "wb"  # Important: write as binary!
)
haul <- readRDS(temp_file)
haul$event_id <- as.numeric(haul$event_id)

# Clean catch data
spp_dictionary <- read.csv("data-raw/MasterSpeciesTableWithAphiaID.csv")
spp_dictionary <- dplyr::rename(spp_dictionary, itis = TSN) |>
  dplyr::group_by(itis) |>
  dplyr::summarize(scientific_name = raw_species[1],
                   common_name = raw_common[1])

catch_data <- dplyr::left_join(catch_data, spp_dictionary)
catch_data <- fishyplots:::clean_fishnames(catch_data)

x <- spp_list |> pull(common_name)
y <- catch_data |> pull(common_name)
missing_species <- setdiff(x,y)

# See which species are failing
no_data_species <- data.frame(i = integer(), r = integer(), species = character())

# Model loop
for(i in 1:nrow(spp_list)) {
  
  for (r in unique(haul$year)) {
    # Filter catch to species
    subset_catch <- dplyr::filter(catch_data, common_name==spp_list$common_name[i])
    #browser()
    # Skip iteration if there are no data
    if (nrow(subset_catch) == 0) {
      no_data_species <- rbind(no_data_species, data.frame(i = i, r = r, species = spp_list$common_name[i]))
      message(paste0("No data found for ", spp_list$common_name[i], r, " in NWFSC catch data."))
      next
    }
    
    # Join the haul data with the catch data for this species
    joined <- dplyr::left_join(haul, subset_catch)
    
    # Fill in the 0s
    joined$catch_wt[which(is.na(joined$catch_wt))] <- 0
    
    # Filter to most recent year
    joined <- dplyr::filter(joined, year %in% r)
    
    joined <- joined |> filter(!is.na(lat_start)) |> filter(!is.na(lon_start))
    
    if (nrow(joined) == 0) {
      no_data_species <- rbind(no_data_species, data.frame(i = i, r = r, species = spp_list$common_name[i]))
      message(paste0("No data found for ", spp_list$common_name[i], r, " in NWFSC catch data."))
      next
    }
    
    # Confirm CRS
    joined <- sdmTMB::add_utm_columns(joined, ll_names = c("lon_start", "lat_start"), utm_crs = 32610)
    
    # Create cpue column
    joined$cpue <- joined$catch_wt / joined$effort
    
    # Make mesh
    mesh <- sdmTMB::make_mesh(
      joined, xy_cols = c("X","Y"),
      cutoff = 20
    )
    
    tweedie <- FALSE
    fit <- tryCatch({sdmTMB(cpue ~ 1,
                            data = joined,
                            offset = log(joined$effort),
                            mesh = mesh,
                            family = delta_lognormal(),
                            spatiotemporal="off",
                            spatial="on")
    }, error = function(e) {
      message(paste0("Model failure for ", spp_list$common_name[i], r, ": ", e$message))
      return(NULL)
    })
    #if (is.null(fit)) next
    
    sanity_check <- sanity(fit, silent=TRUE)
    sanity_vec <- unlist(sanity_check)
    if (any(!sanity_vec)) {
      failed_checks <- names(sanity_vec)[!sanity_vec]
      tweedie <- TRUE
      message(paste0("Sanity check failure for ", spp_list$common_name[i], r, ": ", paste(failed_checks, collapse = ", "), ". Trying a Tweedie model instead..."))
    }
    
    if (tweedie == TRUE) {
      fit <- tryCatch({sdmTMB(cpue ~ 1,
                              data = joined,
                              #offset = log(joined$effort),
                              mesh = mesh,
                              family = tweedie(),
                              spatiotemporal="off",
                              spatial="on")
      }, error = function(e) {
        message(paste0("Tweedie model failure for ", i, r, ": ", e$message))
        return(NULL)
      })
      if (is.null(fit)) next
    }
    
    if (tweedie == TRUE) {
      sanity_check <- sanity(fit, silent=TRUE)
      sanity_vec <- unlist(sanity_check)
      if (any(!sanity_vec)) {
        failed_checks <- names(sanity_vec)[!sanity_vec]
        message(paste0(spp_list$common_name[i], r, "Tweedie sanity check failure: ", paste(failed_checks, collapse = ", ")))
      } else {
        message("Tweedie model successful!")
      }
    }
    
    # Now make the prediction to the data frame from surveyjoin
    grid <- surveyjoin::nwfsc_grid
    grid <- add_utm_columns(grid, ll_names = c("lon", "lat"), utm_crs = 32610)
    
    pred <- predict(fit, grid)
    
    if (tweedie == TRUE) {
      pred$prediction <- exp(pred$est) 
    } else {
      pred$prediction <- plogis(pred$est1) * exp(pred$est2)
    }
    
    # create a simplified data frame with just the columns we want
    pred <- dplyr::select(pred, lon, lat, X, Y, prediction)
    pred$species <- spp_list$common_name[i]
    pred$year <- r
    pred$sanity <- sanity_check$all_ok
    pred$region <- "NWFSC"
    pred$crs <- 32610
    
    if(i == 1) {
      pred_all <- pred
    } else {
      pred_all <- rbind(pred, pred_all)
    }
  }
}

# Check sanity column
table(pred_all$sanity)
# Check which species passed through loop
table(pred_all$species)
# Check species with no data
no_data_species
# Check which species have successful vs. unsuccessful models
pred_all |> filter(sanity == TRUE) |> distinct(species)
pred_all |> filter(sanity == FALSE) |> distinct(species)

# Write dataset
write_parquet(pred_all, "data-raw/predictions.parquet", compression = "snappy")
data <- open_dataset("data-raw/predictions.parquet")

# Mapping
states <- ne_states(country = "united states of america", returnclass = "sf")
canada <- ne_states(returnclass = "sf", country = "canada") |> select(name, geometry)
mexico <- ne_countries(scale = "medium", returnclass = "sf", country = "mexico") |> select(name = admin, geometry)
combined <- bind_rows(states, canada, mexico)
proj <- st_transform(combined, crs = 3157)
proj <- suppressWarnings(st_crop(proj, c(xmin = -1000000, ymin = 3350000, xmax = 1200000, ymax = 5600000)))
name <- "US West Coast"
fourth_root <- trans_new(
  name = "fourth_root",
  transform = function(x) x^(1/4),
  inverse = function(x) x^4
)

sablefish <- data |>
  filter(species == "sablefish") |>
  select(X, Y, prediction, year) |>
  collect()

sablefish$prediction_transformed <- sablefish$prediction^(1/4)

p <- ggplot() +
  stat_summary_hex(data = sablefish, aes(x = X*1000, y = Y*1000, z = prediction), bins = 50) +
  scale_fill_viridis_c(trans = fourth_root, option = "magma", name = "CPUE (kg/km\u00B2)") +
  geom_sf(data = proj) +
  coord_sf(expand = FALSE) +
  geom_sf_text(data = proj, aes(label = name), size = 2.7, fontface = "bold", check_overlap = TRUE) +
  theme_minimal() +
  transition_time(year) +
  labs(x = "", y = "", title = paste0("Predicted Density ", name, " {as.integer(frame_time)}"),
       caption = paste0("Note: color scale is fourth-root transformed."))

animate(p, fps = 3, width = 800, height = 600)

g <- plot_ly(data = sablefish, x = ~(X*1000), y = ~(Y*1000),
             frame = ~year, type = 'scatter', mode = 'markers',
             marker = list(
               size = 6,
               color = ~prediction_transformed,
               colorscale = 'Viridis',
               colorbar = list(title = "Prediction"),
               cmin = min(sablefish$prediction_transformed, na.rm = TRUE),
               cmax = max(sablefish$prediction_transformed, na.rm = TRUE)
             ))
g <- g |> layout(title = "Sablefish predictions",
                 showlegend = FALSE,
                 mapbox = list(
                   style = "light",
                   zoom = 4,
                   center = list(lat = mean(sablefish$lat), lon = mean(sablefish$lon))
                 ))
g <- g |> animation_opts(
  frame = 500, transition = 200, redraw = FALSE
)
g
