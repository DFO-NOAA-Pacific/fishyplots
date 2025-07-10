library(taxize)
library(surveyjoin)
library(dplyr)
# read in PBS catch data
data <- readRDS(url("https://raw.githubusercontent.com/DFO-NOAA-Pacific/surveyjoin-data/3ad708fb208f58bb6bd19ec605a569ca93b54fd8/pbs-catch-all.rds"))

pbs_top <- dplyr::filter(data, !is.na(species_common_name), species_common_name != "all species") |>
dplyr::group_by(species_common_name) |>
  dplyr::summarize(tot_wt = sum(catch_weight),
                   species_science_name = species_science_name[1]) |>
  dplyr::arrange(-tot_wt) |>
  dplyr::slice(1:20) |>
  dplyr::rename(common_name = species_common_name,
                scientific_name = species_science_name)
pbs_top$region<-"PBS"
pbs_top$common_name <- tolower(pbs_top$common_name)
pbs_top$scientific_name <- tolower(pbs_top$scientific_name)



# Alaska data
temp_file <- tempfile(fileext = ".rds")

# Download the .rds file from GitHub (raw)
download.file(
  "https://raw.githubusercontent.com/DFO-NOAA-Pacific/surveyjoin-data/main/afsc-catch-all.rds",
  destfile = temp_file,
  mode = "wb"  # Important: write as binary!
)

# Read the RDS file
afsc_data <- readRDS(temp_file)

afsc_top <- dplyr::filter(afsc_data, !is.na(scientific_name), scientific_name != "all species") |>
  dplyr::group_by(scientific_name) |>
  dplyr::summarize(tot_wt = sum(catch_weight)) |>
  dplyr::arrange(-tot_wt) |>
  dplyr::slice(1:40)
afsc_top$common_name <- NA
for(i in 1:nrow(afsc_top)) {
  id <- taxize::sci2comm(afsc_top$scientific_name[i])[[1]]
  if(length(id) != 0) afsc_top$common_name[i] <- id
}
afsc_top <- dplyr::filter(afsc_top, !is.na(common_name))
afsc_top <- dplyr::filter(afsc_top, common_name %in% c("Northern Pacific seastar",
                                                       "snow crab",
                                                       "sea squirts",
                                                       "sponges",
                                                       "jellyfishes",
                                                       "sea anemones",
                                                       "red king crab")==FALSE)
afsc_top$region <- "AFSC"
afsc_top$common_name <- tolower(afsc_top$common_name)
afsc_top$scientific_name <- tolower(afsc_top$scientific_name)


# Alaska data
temp_file <- tempfile(fileext = ".rds")

# Download the .rds file from GitHub (raw)
temp_file <- tempfile(fileext = ".rds")
download.file(
  "https://raw.githubusercontent.com/DFO-NOAA-Pacific/surveyjoin-data/main/nwfsc-catch-all.rds",
  destfile = temp_file,
  mode = "wb"  # Important: write as binary!
)

nwfsc_data <- readRDS(temp_file)

nwfsc_top <- dplyr::filter(nwfsc_data, !is.na(itis), itis != "all species") |>
  dplyr::group_by(itis) |>
  dplyr::summarize(tot_wt = sum(catch_wt)) |>
  dplyr::arrange(-tot_wt) |>
  dplyr::slice(1:30)

joined_list <- readRDS("data-raw/joined_list.rds")
nwfsc_top <- dplyr::left_join(nwfsc_top, joined_list, by = "itis")
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
nwfsc_top$region <- "NWFSC"

nwfsc_top$common_name <- NA
for(i in 1:nrow(nwfsc_top)) {
  id <- taxize::sci2comm(nwfsc_top$scientific_name[i])[[1]]
  if(length(id) != 0) nwfsc_top$common_name[i] <- id
}

nwfsc_top$common_name[which(nwfsc_top$scientific_name=="sebastolobus altivelis")] <- "longspine thornyhead"
nwfsc_top$common_name[which(nwfsc_top$scientific_name=="sebastolobus alascanus")] <- "shortspine thornyhead"
nwfsc_top$common_name[which(nwfsc_top$scientific_name=="brisaster latifrons")] <- NA
nwfsc_top$common_name[which(nwfsc_top$scientific_name=="ophiodon elongatus")] <- "lingcod"
nwfsc_top$common_name[which(nwfsc_top$scientific_name=="chionoecetes tanneri")] <- "tanner crab"
nwfsc_top$common_name[which(nwfsc_top$scientific_name=="brisaster")] <- NA
nwfsc_top$common_name[which(nwfsc_top$scientific_name=="squalus suckleyi")] <- "north pacific spiny dogfish"
nwfsc_top$common_name[which(nwfsc_top$scientific_name=="sebastes flavidus")] <- "yellowtail rockfish"

nwfsc_top$common_name <- tolower(nwfsc_top$common_name)
nwfsc_top$scientific_name <- tolower(nwfsc_top$scientific_name)

nwfsc_top <- dplyr::filter(nwfsc_top, !is.na(common_name),
                           common_name %in% c("dungeness crab",
                                              "tanner crab",
                                              "jellyfishes",
                                              "pacific grenadier",
                                              "spotted ratfish",
                                              "longnose skate"
                                              )==FALSE)

saveRDS(pbs_top, file="data-raw/pbs_top.rds")
saveRDS(afsc_top, file="data-raw/afsc_top.rds")
saveRDS(nwfsc_top, file="data-raw/nwfsc_top.rds")


# finally look at the overlap from surveyjoin

d <- get_data()

overlap <- dplyr::group_by(d, common_name) |>
  dplyr::summarise(scientific_name = scientific_name[1],
                   tot_wt = sum(catch_weight, na.rm=T)) |>
  dplyr::arrange(-tot_wt) |>
  dplyr::slice(1:20) 
overlap$region <- "overlap"
saveRDS(overlap, file="data-raw/overlap_top.rds")

nwfsc_joined <- rbind(dplyr::select(nwfsc_top,-itis), overlap) |>
  dplyr::group_by(common_name) |>
  dplyr::summarize(scientific_name = scientific_name[1])
write.csv(nwfsc_joined, file="data-raw/nwfsc_joined.csv", row.names = FALSE)

afsc_joined <- rbind(afsc_top, overlap) |>
  dplyr::group_by(common_name) |>
  dplyr::summarize(scientific_name = scientific_name[1])
write.csv(afsc_joined, file="data-raw/afsc_joined.csv", row.names = FALSE)

pbs_joined <- rbind(pbs_top, overlap) |>
  dplyr::group_by(common_name) |>
  dplyr::summarize(scientific_name = scientific_name[1])
write.csv(pbs_joined, file="data-raw/pbs_joined.csv", row.names = FALSE)
