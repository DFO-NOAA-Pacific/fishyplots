library(surveyjoin)
library(lubridate)
library(dplyr)
library(sdmTMB)
library(stringr)
library(dbplyr)
url <- "https://raw.githubusercontent.com/pfmc-assessments/indexwc/main/data-raw/configuration.csv"
config_data <- read.csv(url, stringsAsFactors = FALSE)
config_data <- dplyr::filter(config_data,
                             source == "NWFSC.Combo")
# add model index
config_data$index_id <- seq_len(nrow(config_data))
# switch signs on the depth -- negative but pos in data
config_data$max_depth <- -config_data$max_depth
config_data$min_depth <- -config_data$min_depth
# drop out pass_scaled and put in yday instead
config_data$formula <- str_replace(config_data$formula,
                                   "pass_scaled",
                                   "zday + I(zday^2)")
# replace longnames in family
config_data$family <- str_replace(config_data$family, "sdmTMB::", "")
config_data$family <- str_replace(config_data$family, "\\(\\)", "")

dat <- surveyjoin::get_data()
dat <- dplyr::filter(dat,
                     common_name %in% tolower(config_data$species))

# for illustrative purposes, focus initially on WCGBTS
dat <- dplyr::filter(dat, survey_name == "NWFSC.Combo")

# convert date string to doy
dat$yday <- lubridate::yday(lubridate::ymd(dat$date))

dat <- dplyr::rename(dat, latitude_dd = lat_start,
                     longitude_dd = lon_start) |>
  dplyr::filter(!is.na(longitude_dd),
                !is.na(latitude_dd))
# filter fields for smaller file size
dat <- dplyr::select(dat,
                     #event_id,
                     common_name,
                     year,
                     yday,
                     depth_m,
                     effort,
                     catch_weight,
                     scientific_name,
                     longitude_dd,
                     latitude_dd
)

crs_out <- 32610

# Load data
#dat <- readRDS("data/wcgbts.rds")
# add X, Y
dat <- sdmTMB::add_utm_columns(dat,
                               ll_names = c("longitude_dd","latitude_dd"),
                               utm_crs = crs_out)
spp <- unique(dat$common_name)
spp <- spp[which(spp%in%config_data$species)]
config_data <- dplyr::filter(config_data, tolower(species) %in% spp)

clean_dat <- dat |> 
  dplyr::filter(common_name == "arrowtooth flounder")

get_parameters <- function(data, common_name) {
  data <- data |> filter(species == common_name)
  knots <- data |> select(knots) |> pull()
  formula <- as.formula(data |> select(formula) |> pull())
  spatiotemporal1 <- data |> select(spatiotemporal1) |> pull()
  spatiotemporal2 <- data |> select(spatiotemporal2) |> pull()
  anisotropy <- data |> select(anisotropy) |> pull()
  family <- data |> select(family) |> pull()
  #family <- gsub("sdmTMB::", "", family)
  share_range <- data |> select(share_range) |> pull()
  return(list(knots = knots, formula = formula, spatiotemporal1 = spatiotemporal1, spatiotemporal2 = spatiotemporal2, anisotropy = anisotropy, family = family, share_range = share_range))
}
params <- get_parameters(config_data, "arrowtooth flounder")

# 50 knots 
mesh <- sdmTMB::make_mesh(clean_dat, xy_cols = c("X", "Y"),
                          n_knots = params[["knots"]])
clean_dat$fyear <- as.factor(clean_dat$year)


fit <- NULL
fit <- try(sdmTMB(formula = catch_weight ~ 0 + fyear,
              time = "year",
              offset = log(clean_dat$effort),
              mesh = mesh,
              data = clean_dat,
              spatial = "on",
              spatiotemporal = list(params[["spatiotemporal1"]], params[["spatiotemporal2"]]),
              anisotropy = params[["anisotropy"]],
              family = delta_gamma(),
              share_range = params[["share_range"]], silent = TRUE))
san <- sanity(fit, silent = TRUE)
grid <- surveyjoin::nwfsc_grid
# grid <- dplyr::filter(grid,
#                              lat >= config_data$min_latitude[i],
#                              lat < config_data$max_latitude[i],
#                              depth_m >= config_data$min_depth[i],
#                              depth_m < config_data$max_depth[i],
#                              area > 0)
# Add calendar date -- predicting to jul 1
grid$zday <- (182 - mean(clean_dat$yday)) / sd(clean_dat$yday)
# add X-Y
grid <- sdmTMB::add_utm_columns(grid,
                                       ll_names = c("lon","lat"),
                                       utm_crs = crs_out)

# replicate grid
grid <- replicate_df(grid, time_name = "year",
                            time_values = unique(clean_dat$year))
grid$fyear <- as.factor(grid$year)

# Make predictions
# return_tmb_object in order to use other functions afterwards
pred_all <- predict(fit, grid, return_tmb_object = TRUE)

index <- get_index(pred_all)
ggplot(index, aes(year, est)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.5) +
  geom_line()

cog <- get_cog(pred_all)
