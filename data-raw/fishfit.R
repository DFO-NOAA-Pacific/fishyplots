library(sdmTMB)
library(dplyr)
library(surveyjoin)

# Species lists
afsc_spp <- read.csv("data-raw/afsc_joined.csv")
nwfsc_spp <- read.csv("data-raw/nwfsc_joined.csv")
pbs_spp <- read.csv("data-raw/pbs_joined.csv")

afsc_spp$common_name[afsc_spp$common_name == "Kamchatka flounder"] <- "kamchatka flounder"
afsc_spp$common_name[afsc_spp$common_name == "Pacific cod"] <- "pacific cod"
afsc_spp$common_name[afsc_spp$common_name == "Pacific halibut"] <- "pacific halibut"
afsc_spp$common_name[afsc_spp$common_name == "Pacific herring"] <- "pacific herring"
afsc_spp$common_name[afsc_spp$common_name == "Pacific ocean perch"] <- "pacific ocean perch"
afsc_spp$common_name[afsc_spp$common_name == "Rex sole"] <- "rex sole"
nwfsc_spp$common_name[nwfsc_spp$common_name == "north pacific spiny dogfish"] <- "pacific spiny dogfish"


# Alaska catch data
#afsc_catch <- get_data(surveys = c("Aleutian Islands", "Gulf of Alaska", "eastern Bering Sea", "northern Bering Sea"), 
                       #common = afsc_spp$common_name)
afsc_data <- readRDS(url("https://raw.githubusercontent.com/DFO-NOAA-Pacific/surveyjoin-data/main/afsc-catch-all.rds"))


# NWFSC catch data
nwfsc_data <- readRDS(url("https://raw.githubusercontent.com/DFO-NOAA-Pacific/surveyjoin-data/3ad708fb208f58bb6bd19ec605a569ca93b54fd8/nwfsc-catch-all.rds"))
joined_list <- readRDS("data-raw/joined_list.rds")

nwfsc_top <- dplyr::left_join(nwfsc_data, spp_dictionary, by = "itis")
nwfsc_top$scientific_name[which(nwfsc_top$itis==166784)] <- "sebastolobus altivelis"
nwfsc_top$scientific_name[which(nwfsc_top$itis==166722)] <- "sebastes goodei"
nwfsc_top$scientific_name[which(nwfsc_top$itis==166741)] <- "sebastes saxicola"
nwfsc_top$scientific_name[which(nwfsc_top$itis==98675)] <- "cancer magister"
nwfsc_top$scientific_name[which(nwfsc_top$itis==158082)] <- "brisaster latifrons"
nwfsc_top$scientific_name[which(nwfsc_top$itis==165334)] <- "coryphaenoides acrolepis"
nwfsc_top$scientific_name[which(nwfsc_top$itis==166725)] <- "sebastes jordani"
nwfsc_top$scientific_name[which(nwfsc_top$itis==98431)] <- "chionoecetes tanneri"
nwfsc_top$scientific_name[which(nwfsc_top$itis==158079)] <- "brisaster"
nwfsc_top$scientific_name[which(nwfsc_top$itis==51483)] <- "scyphozoa"
nwfsc_top$common_name[which(nwfsc_top$scientific_name=="sebastolobus altivelis")] <- "longspine thornyhead"
nwfsc_top$common_name[which(nwfsc_top$scientific_name=="chionoecetes tanneri")] <- "tanner crab"
nwfsc_top$common_name[which(nwfsc_top$scientific_name=="brisaster")] <- NA
nwfsc_top$common_name[which(nwfsc_top$scientific_name=="brisaster latifrons")] <- NA
nwfsc_top$common_name[which(nwfsc_top$scientific_name=="sebastes goodei")] <- "chilipepper"
nwfsc_top$common_name[which(nwfsc_top$scientific_name=="sebastes saxicola")] <- "stripetail rockfish"
nwfsc_top$common_name[which(nwfsc_top$scientific_name=="sebastes jordani")] <- "shortbelly rockfish"
nwfsc_top$common_name[which(nwfsc_top$scientific_name=="coryphaenoides acrolepis")] <- "pacific grenadier"

download.file(
  "https://raw.githubusercontent.com/DFO-NOAA-Pacific/surveyjoin-data/3ad708fb208f58bb6bd19ec605a569ca93b54fd8/nwfsc-haul.rds",
  destfile = temp_file,
  mode = "wb"  # Important: write as binary!
)
haul <- readRDS(temp_file)

nwfsc_catch <- dplyr::filter(nwfsc_top, common_name %in% nwfsc_spp$common_name) 
nwfsc_catch$event_id <- as.character(nwfsc_catch$event_id)
haul$event_id <- as.character(haul$event_id)
#nwfsc_joined <- dplyr::left_join(haul, nwfsc_catch, by = "event_id")
#nwfsc_joined$catch_wt[which(is.na(nwfsc_joined$catch_wt))] <- 0

# Canada catch data
pbs_data <- readRDS(url("https://raw.githubusercontent.com/DFO-NOAA-Pacific/surveyjoin-data/3ad708fb208f58bb6bd19ec605a569ca93b54fd8/pbs-catch-all.rds"))
pbs_data <- dplyr::filter(pbs_data, species_common_name %in% pbs_spp$common_name)
temp_file <- tempfile(fileext = ".rds")
download.file(
  "https://raw.githubusercontent.com/DFO-NOAA-Pacific/surveyjoin-data/main/pbs-haul.rds",
  destfile = temp_file,
  mode = "wb" 
)
haul <- readRDS(temp_file)

# Create cpue_kg_km2
afsc_catch <- afsc_catch |> mutate(cpue_kg_km2 = catch_weight / (effort * 0.01))
#nwfsc_joined <- nwfsc_joined |> mutate(cpue_kg_km2 = catch_wt / (effort * 0.01))
#nwfsc_joined$cpue_kg_km2 <- format(nwfsc_joined$cpue_kg_km2, scientific = FALSE)
#pbs_catch <- pbs_catch |> mutate(cpue_kg_km2 = catch_weight / (effort * 0.01))

# Function
fishfit <- function(data, species, region, haul_data) {
  if (region == "nwfsc") {
    predictions <- list()
    for (i in species) {
      # Get US West coast prediction grid and confirm CRS
      grid <- nwfsc_grid |> filter(survey == "NWFSC.Combo")
      grid <- add_utm_columns(dat = grid, ll_names = c("lon", "lat"), utm_names = c("X", "Y"), utm_crs = 3157, units = "km")
      # Get catch data, filter by species and year, confirm CRS
      data <- dplyr::left_join(haul_data, data)
      
      clean_data <- data |> 
        filter(common_name == i) |> filter(year == 2023)
      if (nrow(clean_data) == 0) {
        message(paste0("No data found for ", i, " in US west coast catch data."))
        next}
      
      clean_data$catch_wt[which(is.na(clean_data$catch_wt))] <- 0
      clean_data <- clean_data |> mutate(cpue_kg_km2 = catch_wt / (effort * 0.01))
      
      clean_data <- add_utm_columns(dat = clean_data, ll_names = c("lon_start", "lat_start"), utm_names = c("X", "Y"), utm_crs = 3157, units = "km")
      cutoff <- 20
      # Make mesh and model
      mesh <- make_mesh(clean_data, c("X", "Y"), cutoff = cutoff)
      message(mesh$mesh$n)
      model <- sdmTMB(data = clean_data,
                      formula = cpue_kg_km2 ~ 1,
                      mesh = mesh,
                      family = delta_lognormal(),
                      spatial = "on",
                      anisotropy = FALSE)
   
      sanity_check <- sanity(model, silent = TRUE)
      sanity_vec <- unlist(sanity_check)
      if (any(!sanity_vec)) {
        failed_checks <- names(sanity_vec)[!sanity_vec]
        message(paste0(i, " sanity check failure: ", paste(failed_checks, collapse = ", ")))
      }
      # Make predictions for grid, back transform and save in the empty list
      raw_preds <- predict(model, newdata = grid)
      preds <- raw_preds |>
        mutate(prediction = plogis(est1) * exp(est2)) |>
        mutate(species = i) |>
        mutate(region = region) |>
        mutate(sanity = sanity_check$all_ok) |>
        select(X, Y, prediction, species, region, sanity)
      predictions[[i]] <- preds
    }
    results <- bind_rows(predictions)
    return(results)
  }
  if (region == "afsc") {
    predictions <- list()
    for (i in species) {
      # Get Alaska prediction grid and confirm CRS
      AK_grid <- surveyjoin::afsc_grid
      AK_grid <- add_utm_columns(dat = AK_grid, ll_names = c("lon", "lat"), 
                                 utm_names = c("X", "Y"), utm_crs = 32602, units = "km")
      # Get Alaska catch data, filter by species, confirm CRS
      AK_catch <- data |>
        filter(!is.na(lon_start)) |>
        filter(!is.na(lat_start)) |>
        filter(common_name == i)
      
      if (nrow(AK_catch) == 0) {
        message(paste0("No data found for ", i, " in Alaska catch data."))
        next
      }
      
      AK_catch <- add_utm_columns(dat = AK_catch, ll_names = c("lon_start", "lat_start"),
                                  utm_names = c("X", "Y"), utm_crs = 32602, units = "km")
      # Fetch the most recent years for four desired surveys 
      a <- AK_catch |> filter(survey_name == "Aleutian Islands" & year == 2024)
      b <- AK_catch |> filter(survey_name == "eastern Bering Sea" & year == 2024)
      c <- AK_catch |> filter(survey_name == "Gulf of Alaska" & year == 2023)
      d <- AK_catch |> filter(survey_name == "northern Bering Sea" & year == 2023)
      AK_all <- rbind(a,b,c,d)
      
      # Make mesh, model, and predictions
      mesh <- make_mesh(AK_all, c("X", "Y"), cutoff = 82)
      model <- sdmTMB(data = AK_all,
                      formula = cpue_kg_km2 ~ 1,
                      mesh = mesh,
                      family = delta_lognormal(),
                      spatial = "on",
                      anisotropy = FALSE)
      
      sanity_check <- sanity(model, silent = TRUE)
      sanity_vec <- unlist(sanity_check)
      if (any(!sanity_vec)) {
        failed_checks <- names(sanity_vec)[!sanity_vec]
        message(paste0(i, " sanity check failure: ", paste(failed_checks, collapse = ", ")))
      }
      
      raw_preds <- predict(model, newdata = AK_grid)
      preds <- raw_preds |>
        mutate(prediction = plogis(est1) * exp(est2)) |>
        mutate(species = i) |>
        mutate(region = region) |>
        mutate(sanity = sanity_check$all_ok) |>
        select(X, Y, prediction, species, region, sanity)
      predictions[[i]] <- preds
    }
    results <- bind_rows(predictions)
    return(results)
  }
  if (region == "pbs") {
     predictions <- list()
     for (i in species) {
       # Get Canada grid
       CAN_grid <- surveyjoin::dfo_synoptic_grid
       CAN_grid <- add_utm_columns(dat = CAN_grid, ll_names = c("lon", "lat"), 
                                   utm_names = c("X", "Y"), utm_crs = 32610, units = "km")
       # Get catch data
       subset_catch <- dplyr::filter(data, species_common_name == i)

       # Join the haul data with the catch data for this species
       joined <- dplyr::left_join(haul, subset_catch)
       # Fill in the 0s
       joined$catch_weight[which(is.na(joined$catch_weight))] <- 0
       joined <- dplyr::filter(joined, year %in% c(2022,2023))
       
       if (nrow(joined) == 0) {
         message(paste0("No data found for ", i, " in Canada catch data."))
         next
       }
       
       joined <- add_utm_columns(dat = joined, ll_names = c("lon_start", "lat_start"),
                                   utm_names = c("X", "Y"), utm_crs = 32610, units = "km")
       joined <- joined |> mutate(cpue_kg_km2 = catch_weight / (effort * 0.01))
       
       # Make mesh and model
       mesh <- make_mesh(joined, xy_cols = c("X", "Y"), cutoff = 20)
       model <- tryCatch({sdmTMB(catch_weight ~ 1,
                       data = joined,
                       offset = log(joined$effort),
                       mesh = mesh,
                       family = delta_lognormal(),
                       spatiotemporal = "off",
                       spatial = "on")
       }, error = function(e) {
         message(paste0("Model failure for ", i, ": ", e$message))
         return(NULL)
       })
       if (is.null(model)) next
       
       # model <- sdmTMB(cpue_kg_km2 ~ 1,
       #                 data = CAN_catch,
       #                 mesh = mesh,
       #                 family = delta_lognormal(),
       #                 spatial="on",
       #                 anisotropy = FALSE)
       
       sanity_check <- sanity(model, silent = TRUE)
       sanity_vec <- unlist(sanity_check)
       if (any(!sanity_vec)) {
         failed_checks <- names(sanity_vec)[!sanity_vec]
         message(paste0(i, " sanity check failure: ", paste(failed_checks, collapse = ", ")))
       }
       raw_preds <- predict(model, newdata = CAN_grid)
       preds <- raw_preds |>
         mutate(prediction = plogis(est1) * exp(est2)) |>
         mutate(species = i) |>
         mutate(region = region) |>
         mutate(sanity = sanity_check$all_ok) |>
         select(X, Y, prediction, species, region, sanity)
       predictions[[i]] <- preds
       }
     results <- bind_rows(predictions)
     return(results)
  }
}

# Call function on catch data
nwfsc_predictions <- fishfit(nwfsc_joined, nwfsc_spp$common_name, "nwfsc", haul)
afsc_predictions <- fishfit(afsc_catch, afsc_spp$common_name, "afsc")
pbs_predictions <- fishfit(pbs_data, pbs_spp$common_name, "pbs", haul_data = haul)

table(nwfsc_predictions$sanity) # No data for northern rock sole, all checks failed
table(afsc_predictions$sanity) # dover sole, grenadier, hake, sanddab, dogfish, rock sole, sharpchin, shortspine, splitnose, ratfish, rockfish
table(pbs_predictions$sanity) # english sole, sablefish (+3 species with model failure)

# Write dataframes
#usethis::use_data(nwfsc_predictions)
#usethis::use_data(afsc_predictions)
#usethis::use_data(pbs_predictions)

