library(sdmTMB)
library(surveyjoin)
library(dplyr)

# Read in species to run models for
spp_list <- read.csv("data-raw/nwfsc_joined.csv")
spp_list$common_name[spp_list$common_name == "pacific spiny dogfish"] <- "spiny dogfish"
spp_list$common_name[spp_list$common_name == "north pacific hake"] <- "Pacific hake"
spp_list$common_name[spp_list$common_name == "pacific cod"] <- "Pacific cod"
spp_list$common_name[spp_list$common_name == "pacific ocean perch"] <- "Pacific ocean perch"
spp_list$common_name[spp_list$common_name == "pacific halibut"] <- "Pacific halibut"
spp_list$common_name[spp_list$common_name == "pacific sanddab"] <- "Pacific sanddab"
spp_list$common_name[spp_list$common_name == "dover sole"] <- "Dover sole"
spp_list$common_name[spp_list$common_name == "english sole"] <- "english sole"

# Catch data
temp_file <- tempfile(fileext = ".rds")
# Download the .rds file from GitHub (raw)
download.file(
  "https://raw.githubusercontent.com/DFO-NOAA-Pacific/surveyjoin-data/main/nwfsc-catch.rds",
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

no_data_species <- data.frame(i = integer(), species = character())

# Model loop
for(i in 1:nrow(spp_list)) {
  # Filter catch to species
  subset_catch <- dplyr::filter(catch_data, common_name==spp_list$common_name[i])
  
  # Skip iteration if there are no data
  if (nrow(subset_catch) == 0) {
    no_data_species <- rbind(no_data_species, data.frame(i = i, species = spp_list$common_name[i]))
    message(paste0("No data found for ", spp_list$common_name[i], " in NWFSC catch data."))
    next
  }
  
  # Join the haul data with the catch data for this species
  joined <- dplyr::left_join(haul, subset_catch)
  
  # Fill in the 0s
  joined$catch_weight[which(is.na(joined$catch_weight))] <- 0
  
  # Filter to most recent year
  joined <- dplyr::filter(joined, year %in% c(2024))
  
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
  # mesh$mesh$n
  
  # model
  # fit <- tryCatch({sdmTMB(cpue ~ 1,
  #                        data = joined,
  #                        #offset = log(joined$effort),
  #                        mesh = mesh,
  #                        family = tweedie(),
  #                        spatiotemporal="off",
  #                        spatial="on")
  # }, error = function(e) {
  #   message(paste0("Model failure for ", i, ": ", e$message))
  #   return(NULL)
  # })
  # if (is.null(fit)) next
  tweedie <- FALSE
  fit <- tryCatch({sdmTMB(cpue ~ 1,
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
      message(paste0("Tweedie model failure for ", i, ": ", e$message))
      return(NULL)
    })
    if (is.null(fit)) next
  }
  
  if (tweedie == TRUE) {
    sanity_check <- sanity(fit, silent=TRUE)
    sanity_vec <- unlist(sanity_check)
    if (any(!sanity_vec)) {
      failed_checks <- names(sanity_vec)[!sanity_vec]
      message(paste0(spp_list$common_name[i], "Tweedie sanity check failure: ", paste(failed_checks, collapse = ", ")))
    } else {
      message("Tweedie model successful!")
    }
  }
  
  # Now make the prediction to the data frame from surveyjoin
  grid <- surveyjoin::nwfsc_grid
  grid <- add_utm_columns(grid, ll_names = c("lon", "lat"), utm_crs = crs)
  
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
  pred$region <- "nwfsc"
  
  if(i == 1) {
    pred_all <- pred
  } else {
    pred_all <- rbind(pred, pred_all)
  }
  
}

# Check sanity column
table(pred_all$sanity)
# Check which species passed through loop
table(pred_all$species)
# Check species with no data
no_data_species
# Check which species have successful vs. unsuccessful models
pred_all |> filter(sanity == TRUE) |> distinct(common_name)
pred_all |> filter(sanity == FALSE) |> distinct(common_name)