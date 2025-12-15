library(sdmTMB)
library(surveyjoin)
library(dplyr)

# Species list
spp_list <- read.csv("data-raw/pbs_joined.csv")
spp_list <- spp_list |> filter(common_name != "north pacific spiny dogfish")
spp_list <- spp_list |> filter(common_name != "north pacific hake")

# Get catch data
temp_file <- tempfile(fileext = ".rds")
# Download the .rds file from GitHub (raw)
download.file(
  "https://raw.githubusercontent.com/DFO-NOAA-Pacific/surveyjoin-data/main/pbs-catch-all.rds",
  destfile = temp_file,
  mode = "wb"  # Important: write as binary!
)
catch_data <- readRDS(temp_file)

# Download the .rds file from GitHub (raw)
download.file(
  "https://raw.githubusercontent.com/DFO-NOAA-Pacific/surveyjoin-data/main/pbs-haul.rds",
  destfile = temp_file,
  mode = "wb"  # Important: write as binary!
)
haul <- readRDS(temp_file)
haul$event_id <- as.numeric(haul$event_id)

# Rename to match the spp_list
catch_data$species_common_name[catch_data$species_common_name == "north pacific spiny dogfish"] <- "pacific spiny dogfish"

# See which species fail the loop (expected: nothern rock sole)
no_data_species <- data.frame(i = integer(), species = character())

# Model each species
for(i in 1:nrow(spp_list)) {
  # Filter catch to species
  subset_catch <- dplyr::filter(catch_data, species_common_name==spp_list$common_name[i])
  
  # If there are no catches for this species, skip to the next one
  if (nrow(subset_catch) == 0) {
    no_data_species <- rbind(no_data_species, data.frame(i = i, species = spp_list$common_name[i]))
    message(paste0("No data found for ", spp_list$common_name[i], " in PBS catch data."))
    next
  }
  
  # Join the haul data with the catch data for this species
  joined <- dplyr::left_join(haul, subset_catch, by = c("event_id"))
  
  # Fill in the 0s
  joined$catch_weight[which(is.na(joined$catch_weight))] <- 0
  
  # Filter to most recent year
  joined <- dplyr::filter(joined, year %in% c(2022,2023))
  
  if (nrow(joined) == 0) {
    message(paste0("No data for ", spp_list$common_name[i], " in 2022-2023."))
    next
  }
  
  # Confirm CRS
  #crs <- sdmTMB::get_crs(joined, ll_names = c("lon_start", "lat_start"))
  joined <- sdmTMB::add_utm_columns(joined, ll_names = c("lon_start", "lat_start"), utm_crs = 32610)
  
  # Create cpue column
  joined$cpue <- joined$catch_weight / joined$effort
  
  # Make mesh
  mesh <- sdmTMB::make_mesh(
    joined, xy_cols = c("X","Y"),
    cutoff = 20
  )
  message(paste0("Mesh: ", mesh$mesh$n))
  # mesh$mesh$n
  
  tweedie <- FALSE
  fit <- tryCatch({sdmTMB(catch_weight ~ 1,
                          data = joined,
                          offset = log(joined$effort),
                          mesh = mesh,
                          family = delta_lognormal(),
                          spatiotemporal="off",
                          spatial="on")
  }, error = function(e) {
    message(paste0("Model failure for ", spp_list$common_name[i], ": ", e$message))
    return(NULL)
  })
  #if (is.null(fit)) next
  
  sanity_check <- sanity(fit, silent=TRUE)
  sanity_vec <- unlist(sanity_check)
  if (any(!sanity_vec)) {
    failed_checks <- names(sanity_vec)[!sanity_vec]
    tweedie <- TRUE
    message(paste0("Sanity check failure for ", spp_list$common_name[i], ": ", paste(failed_checks, collapse = ", "), ". Trying a Tweedie model instead..."))
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
      message(paste0("Tweedie model failure for ", spp_list$common_name[i], ": ", e$message))
      return(NULL)
    })
    if (is.null(fit)) next
  }
  
  if (tweedie == TRUE) {
    sanity_check <- sanity(fit, silent=TRUE)
    sanity_vec <- unlist(sanity_check)
    if (any(!sanity_vec)) {
      failed_checks <- names(sanity_vec)[!sanity_vec]
      message(paste0(" Tweedie sanity check failure: ", spp_list$common_name[i], ": ", paste(failed_checks, collapse = ",")))
    } else {
      message("Tweedie model successful!")
    }
  }
  
  # Now make the prediction to the data frame from surveyjoin
  grid <- surveyjoin::dfo_synoptic_grid
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
  pred$sanity <- sanity_check$all_ok
  pred$region <- "PBS"
  pred$crs <- 32610
  
  if(i == 1) {
    pred_all <- pred
  } else {
    pred_all <- rbind(pred, pred_all)
  }
  
}

# Check sanity column
table(pred_all$sanity)
# Check which species passed through
table(pred_all$species)
# Check which species had no data
no_data_species
# Check which species have successful vs. unsuccessful fits
pred_all |> filter(sanity == TRUE) |> distinct(species)
pred_all |> filter(sanity == FALSE) |> distinct(species)
# Test map
#fishmap(pred_all, common_name = "dover sole")

predictions_pbs <- pred_all
usethis::use_data(predictions_pbs, overwrite = TRUE)
