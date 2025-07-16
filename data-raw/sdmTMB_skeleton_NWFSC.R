library(sdmTMB)
library(surveyjoin)
library(dplyr)


# This example will be for the PBS data

# Read in species to run models for
spp_list <- read.csv("data-raw/nwfsc_joined.csv")

# read in PBS catch data -- this is all species, only positive observations
#catch_data <- readRDS(url("https://raw.githubusercontent.com/DFO-NOAA-Pacific/surveyjoin-data/3ad708fb208f58bb6bd19ec605a569ca93b54fd8/pbs-catch-all.rds"))
# filter out only species we're modeling
#catch_data <- dplyr::filter(catch_data, species_common_name %in% spp_list$common_name)

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

# grab spp dictionary
spp_dictionary <- read.csv("data-raw/MasterSpeciesTableWithAphiaID.csv")
spp_dictionary <- dplyr::rename(spp_dictionary, itis = TSN) |>
  dplyr::group_by(itis) |>
  dplyr::summarize(scientific_name = raw_species[1],
                   common_name = raw_common[1])

catch_data <- dplyr::left_join(catch_data, spp_dictionary)

# Now we want to cycle through the species and run the models
for(i in 1:nrow(spp_list)) {
  subset_catch <- dplyr::filter(catch_data, common_name==spp_list$common_name[i])
  # If there are no catches for this species, skip to the next one
  # Join the haul data with the catch data for this species
  joined <- dplyr::left_join(haul, subset_catch)
  
  # Fill in the 0s
  joined$catch_weight[which(is.na(joined$catch_weight))] <- 0
  
  # filter out the last 2 years -- this is specific to DFO sampling
  joined <- dplyr::filter(joined, year %in% c(2024))
  
  # now we get the CRS and add the UTM columns
  crs <- sdmTMB::get_crs(joined, ll_names = c("lon_start", "lat_start"))
  joined <- sdmTMB::add_utm_columns(joined, ll_names = c("lon_start", "lat_start"), utm_crs = crs)
  
  # now make the mesh
  mesh <- sdmTMB::make_mesh(
    joined, xy_cols = c("X","Y"),
    cutoff = 20
  )
  # knots looks reasonable ~ 176
  # mesh$mesh$n
  
  # now we can run the model
  fit <- sdmTMB(catch_weight ~ 1,
                data = joined,
                offset = log(joined$effort),
                mesh = mesh,
                family = delta_lognormal(),
                spatiotemporal="off",
                spatial="on") 
  
  # make sure model has converged:
  sanity_check <- sanity(fit, silent=TRUE)
  
  # Now make the prediction to the data frame from surveyjoin
  grid <- surveyjoin::nwfsc_grid
  grid <- add_utm_columns(grid, ll_names = c("lon", "lat"), utm_crs = crs)
  
  pred <- predict(fit, grid)
  
  # pred will have 2 columns with predictions
  # anything with a '1' is the presence absence model, 
  # anything with a '2' is the positive model
  
  # we can calculate the total
  pred$est <- plogis(pred$est1) * exp(pred$est2)
  
  # create a simplified data frame with just the columns we want
  pred <- dplyr::select(pred, lon, lat, X, Y, est)
  pred$common_name <- spp_list$common_name[i]
  pred$sanity <- sanity_check$all_ok
  
  if(i == 1) {
    pred_all <- pred
  } else {
    pred_all <- rbind(pred, pred_all)
  }
  
}

