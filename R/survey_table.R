#silence build notes
utils::globalVariables(c("survey", "yr", "n_samples", "sample_type"))

#' Main function to display survey specimen counts
#'
#' @param data a data frame of bio data
#' @param species species of interest common or scientific name 
#' @param form choose 1 for tibble or 2 for ggplot
#' @return a tibble or ggplot object
#' @importFrom dplyr group_by summarize mutate arrange bind_rows ungroup left_join
#' @importFrom tidyr pivot_wider complete
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_distiller theme_minimal theme element_blank element_text ggtitle scale_alpha_manual coord_cartesian facet_wrap scale_x_continuous scale_fill_gradientn guides scale_y_discrete
#' @importFrom stats na.omit
#' @importFrom scales label_wrap
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' data("nwfsc_bio")
#' data("pbs_bio")
#' data("afsc_bio")
#' akgulf_bio <- afsc_bio |> filter(survey == "AK GULF")
#' akbsai_bio <- afsc_bio |> filter(survey == "AK BSAI")
#' all_data <- bind_rows(akgulf_bio, akbsai_bio, nwfsc_bio, pbs_bio)
#' 
#' 
#' survey_table(akgulf_bio, "atka mackerel", form = 1)
#' survey_table(nwfsc_bio, "sebastes zacentrus", form = 2)
#' survey_table(pbs_bio,"arrowtooth flounder", form = 2)
#' survey_table(all_data, "arrowtooth flounder")
#' }
survey_table <- function(data, species, form = 2) {
  
  #check if form = 1 or 2 (2 default) for table vs plot
  form <- match.arg(as.character(form), choices = c("1", "2"))
  form <- as.numeric(form) # convert back to numeric for later
  
  
  #filter data to species of interest (spec.data)
  if (any(data$common_name == species | data$scientific_name == species)) {
  spec.data <- filter(data, .data$common_name == species | .data$scientific_name == species)}
  else (stop(paste("Species name", "'", species,"'", "not found in ", "data.")))
  
#### COUNTS ####
  #make subsets with just the wanted data (l/w/a, year, unread counts)
  
## Lengths ##
  #number of lengths counted per year
  length_count <- data.frame(length = spec.data$length_cm, yr = spec.data$year, survey = spec.data$survey) |>
    na.omit() |> #omit rows with no data from being counted
    group_by(.data$yr, .data$survey) |>
    summarize(n_samples=n())|> #sum sample number over grouping (year)
    mutate(sample_type = "Lengths") #label as length count

  # use special dataset for AFSC lengths
  if(any(spec.data$region == "AFSC")){ #only called if dealing with AFSC data

    ak_length_count <-  fishyplots::ak_survey_lengths |> 
      filter(.data$common_name == species) |> 
      group_by(.data$year, .data$survey) |>
      summarize(n_samples=sum(.data$length.count))|>
      mutate(sample_type = "Lengths") |> 
      rename(yr = .data$year) |> 
      na.omit()
    
    #which afsc surveys are being called  (akgulf vs akbsai)
    which.asfc<- unique(spec.data$survey[spec.data$region == "AFSC"])
    
    # replace original AK lengths with special dataset
    length_count <- length_count |>
      filter(!(.data$survey %in% which.asfc)) |>
      bind_rows(ak_length_count |> filter(.data$survey %in% which.asfc))
  }

## Weights ##
  # nothing special, just count of weight data
  weight_count <- data.frame(weight = spec.data$weight_kg, yr = spec.data$year, survey = spec.data$survey) |>
    na.omit() |> 
    group_by(.data$yr, .data$survey) |>
    summarize(n_samples=n())|>
    mutate(sample_type = "Weights")

## Ages ##
  #count of read ages
  age_count <- data.frame(age = spec.data$age_years, yr = spec.data$year, survey = spec.data$survey) |>
    na.omit() |> 
    group_by(.data$yr, .data$survey) |>
    summarize(n_samples=n())|>
    mutate(sample_type = "Ages")
  # fill with 0 if no ages taken (ex nwfsc Sebastes zacentrus)
  if(nrow(age_count) == 0) {
    age_count <- data.frame(survey = unique(spec.data$survey), n_samples = 0, yr = weight_count$yr, sample_type = "Ages")}
  
## Unread Ages ##
  # for most data, n age structures = total specimen count (don't need to worry about multiple age str. per specimen)
  agestr_count <-  data.frame(age = spec.data$age_years, yr = spec.data$year, survey = spec.data$survey) |> 
    group_by(.data$yr, .data$survey) |>
    summarize(n_samples=n())|>
    mutate(sample_type = "Age Structures") 
  
  # for NWFSC, use otolith ID as n age structures
  # if nwfsc data present, rewrite agestr_count
  if ("otosag_id" %in% names(data)) { # Otosag ony present if given dataset contains nwfsc data
  nwfsc_count <- spec.data |>
    filter(.data$region == "NWFSC", !is.na(.data$otosag_id)) |>
    group_by(.data$survey, yr = .data$year) |>
    summarize(n_samples = n(), .groups = "drop") |>
    mutate(sample_type = "Age Structures (otosag_id)")

  # count other age structures normally
  other_count <-  data.frame(age = spec.data$age_years, yr = spec.data$year, survey = spec.data$survey) |> 
    group_by(.data$yr, .data$survey) |>
    summarize(n_samples=n())|>
    mutate(sample_type = "Age Structures") |> 
    filter(.data$survey != "NWFSC")

  # Combine both into new agestr_count
  agestr_count <- bind_rows(nwfsc_count, other_count)
}
 
  #subtract n ages counted (read structures) from n age structures to get unread age structures
  unread_count <- dplyr::left_join(agestr_count, age_count, by = c("yr", "survey")) |>
    mutate(n_ages = ifelse(is.na(.data$n_samples.y), 0, .data$n_samples.y), #change NA to 0 for calculation ease
           n_unread = .data$n_samples.x - .data$n_ages, #unread age strugtures = n age str - n ages read
           n_samples = ifelse(.data$n_unread == 0, NA, .data$n_unread)) |> # make 0 into NA
    select(.data$survey, .data$yr, .data$n_samples) |>
    mutate(sample_type = "Unread Ages") |> #unread structures only
    na.omit() #omit NAs where n age structures = n read ages

 ## trawl counts ##
  total_count <- fishyplots::all_catch |> 
    filter(.data$common_name == species | .data$scientific_name == species, survey %in% spec.data$survey) |> 
    ungroup() |> 
    rename(yr = .data$year, n_samples = .data$n_tows) |> 
    select(.data$yr, .data$survey, .data$n_samples) |> 
    mutate(sample_type = "Total Tows") |> 
    filter(!.data$n_samples == 0)
    
  pos_prop <- fishyplots::all_catch |> 
    filter(.data$common_name == species | .data$scientific_name == species, survey %in% spec.data$survey) |> 
    ungroup() |>
    rename(yr = .data$year, n_samples = .data$proportion_pos) |> 
    select(.data$yr, .data$survey, .data$n_samples) |> 
    mutate(sample_type = "% Positive Tows")|> 
    filter(!.data$n_samples == 0)
  
  # IF there is no catch data for a species, use other trawl data in the same region and years to fill
  if(nrow(total_count) == 0){
    total_count <- weight_count[1:2]
    pos_prop <- weight_count[1:3] |> mutate(sample_type = "% Positive Tows")
    
    #fill count with data from a complete species (same number of tows are conducted no matter the species collected, some species data is just not available)
    #arrowtooth is complete
    arrow.fill<-fishyplots::all_catch |> 
      filter(.data$common_name == "arrowtooth flounder", .data$survey %in% spec.data$survey) |> 
      ungroup() |> 
      rename(yr = .data$year, n_samples = .data$n_tows) |> 
      select(.data$yr, .data$survey, .data$n_samples) |> 
      filter(!.data$n_samples == 0)
    total_count <- dplyr::left_join(total_count, arrow.fill, by = c("yr", "survey")) |> 
      mutate(sample_type = "Total Tows")
    #fill % with NAs
    pos_prop[3] <- NA
    } 
  
   ## Combine ##
  # Combine data into one DF
  bio_data <-length_count |>
    bind_rows(weight_count,age_count, unread_count, total_count, pos_prop) |>
    arrange(.data$yr) |> 
    ungroup() |> 
    group_by(.data$survey) |> 
    tidyr::complete(yr = seq(min(.data$yr), max(.data$yr)), sample_type = unique(.data$sample_type), fill = list(n_samples = 0)) |> 
    ungroup() |> 
    mutate(common = unique(spec.data$common_name)) |> 
    mutate(sample_type=factor(sample_type, levels=c("% Positive Tows","Total Tows", "Unread Ages", "Ages", "Weights", "Lengths"))) #set order for plotting later
  
#### TABLE ####
if (form == 1) {
  # Return wide table
  table <- bio_data |>
    pivot_wider(
      names_from = sample_type,
      values_from = n_samples
    ) |> 
    select(.data$common, dplyr::everything())
  return(table)
}
  
#### PLOT ####
# output graph format if argument form = 2 (Default)
if (form == 2) {

# Convert count to label with "K" for thousands
  bio_data$label <-  ifelse(bio_data$n_samples < 1 & bio_data$n_samples > 0,
                            ifelse(round(bio_data$n_samples * 100, 0) == 0, "<1", #if a positive percent rounds to 0, label as <1
                                   as.character(round(bio_data$n_samples * 100, 0))), # formats decimals to whole number percents
                           ifelse(bio_data$n_samples < 1000,
                              as.character(bio_data$n_samples), # keep as number if under 1000
                                     paste0(round(bio_data$n_samples / 1000), "K") ))# if over 1000, round to nearest and label with k
  #give region descriptive labels
  bio_data$survey <- factor(bio_data$survey, levels = c("AK BSAI", "AK GULF", "PBS", "NWFSC"),
                        labels = c("Aleutians/Bering Sea", "Gulf of Alaska", "Canada", "U.S. West Coast"))
  
  #scale counts to 0-1 per region/sample type to project color
  bio_data <- bio_data |>
    group_by(.data$sample_type, .data$survey) |>
    mutate(n_scaled = (.data$n_samples - min(.data$n_samples, na.rm = TRUE)) / 
             (max(.data$n_samples, na.rm = TRUE) - min(.data$n_samples, na.rm = TRUE)))
  
#create plot
  plot <- ggplot(bio_data, aes(.data$yr, .data$sample_type)) +
    ggplot2::geom_tile(aes(fill = .data$n_scaled, alpha = .data$n_samples != 0), colour = "white", show.legend = FALSE) + # to preserve rows where all years have 0 data, color 0 tiles white
    scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0)) +
    scale_fill_gradientn( # color with white/blue gradient
      colours = c("#eff3ff", "#bdd7e7", "#6baed6", "#2171b5","#1a5a90" ), 
      na.value = "white"
    ) + 
    #theme_pbs() +  -> had issues using gfplot package as a remote, next few lines and theme are is most of the formatting for the theme_pbs function
    theme_light(base_size = 11, base_family = "") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(5.5 / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "grey20"),
      strip.text.y = element_text(colour = "grey20"),
      axis.text = element_text(colour = "grey20"),
      axis.title = element_text(colour = "grey20"),
      legend.title = element_text(colour = "grey20", size = rel(0.9)),
      panel.border = element_rect(fill = NA, colour = "grey70", linewidth = 1),
      legend.key.size = unit(0.9, "lines"),
      legend.text = element_text(size = rel(0.7), colour = "grey20"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA),
      plot.title = element_text(colour = "grey20", size = rel(1)),
      plot.subtitle = element_text(colour = "grey20", size = rel(.85)),
      # my own edits:
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(), 
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10), # tilt years to reduce overlapping text
      axis.text.y = element_text(size = 11)
    ) +
    ggplot2::guides(fill = "none") + xlab("") + ylab("") +
    geom_text(aes(label = ifelse(.data$n_samples > 0, .data$label, "")), #do not label 0s
              colour = "black", 
              size = 3.5, alpha = 1, fontface = "bold"
    ) +
    ggplot2::scale_y_discrete(position = "left", labels = scales::label_wrap(10))+
    scale_x_continuous(breaks = seq(
      if (min(bio_data$yr) %% 2 == 0) min(bio_data$yr) else min(bio_data$yr) + 1, # label even years 
      max(bio_data$yr),
      by = 2))+ #label every 2 years
    #ggplot2::ggtitle(paste("Survey Specimen Counts -", unique(spec.data$common_name))) +
    coord_cartesian(expand = FALSE)
  
  #check for no pos catch data: put note that pos catch data not available 
  no_catch <- bio_data |>
    filter(.data$sample_type == "% Positive Tows") |>
    group_by(.data$survey) |>
    summarize(all_zero = all(n_samples == 0), .groups = "drop") |>
    filter(.data$all_zero) |>
    pull(.data$survey)
  if(any(unique(bio_data$survey) %in% no_catch)) { #if there is no 
    plot <- plot +
      annotate(
        "text", 
        x = -Inf, y = -Inf, label = "No catch data available",
        hjust = -0.2, vjust = -1.5,
        size = 4, color = "black"
      )
  }
  
  #facet wrap if plotting multiple regions
  if(length(unique(bio_data$survey)) > 1) {
    plot <- plot + facet_wrap( ~ survey, ncol = 1, drop = FALSE) + theme(strip.background = element_blank(), strip.text = element_text(size = 13), strip.text.x = element_text(hjust = 0))
  }
  
  return(plot)
  }
  }
  
  
