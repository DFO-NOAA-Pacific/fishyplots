library(dplyr)

spp <- readr::read_csv("data-raw/pbs_joined.csv")

# note these species issues:
spp$common_name[spp$common_name == "north pacific hake"] <- "pacific hake"
spp$common_name[spp$common_name == "pacific spiny dogfish"] <- "north pacific spiny dogfish"
spp <- filter(spp, common_name != "northern rock sole")

cache <- "../gfsynopsis-2024/report/data-cache-2025-03"
user <- Sys.info()[["user"]]
if (user != "seananderson") {
  error("This code will only work locally for Sean.")
}

ssids <- c(1, 3, 4, 16) # synoptic trawl surveys

dat <- purrr::map(seq_len(nrow(spp)), function(i) {
  s <- spp$scientific_name[i]
  f <- paste0(gsub(" ", "-", spp$common_name[i]), ".rds")
  f <- gsub("/", "-", f)
  cat(f, "\n")
  d <- readRDS(file.path(cache, f))
  if ("survey_series_id.x" %in% names(d$survey_sets)) { # FIXME
    d$survey_sets$survey_series_id <- d$survey_sets$survey_series_id.x
    d$survey_sets$survey_series_id.x <- NULL
    d$survey_sets$survey_series_id.y <- NULL
  }
  d$survey_sets <- filter(d$survey_sets, survey_series_id %in% ssids)
  d$survey_samples <- filter(d$survey_samples, survey_series_id %in% ssids)
  d$survey_index <- filter(d$survey_index, survey_series_id %in% ssids)
  out <- list(
    survey_sets = d$survey_sets,
    survey_index = d$survey_index,
    survey_samples = d$survey_samples
  )
  out
})

names(dat) <- spp$common_name
names(dat)

saveRDS(dat, file.path("data-raw", "pbs-gfdata.rds"))
