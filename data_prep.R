base::Sys.setlocale(locale = "nb.utf8")

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/get_from_nvdb_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")


# TRP info ----
trp <-
  get_points() |>
  dplyr::distinct(
    trp_id,
    .keep_all = TRUE
  ) |>
  dplyr::select(
    trp_id,
    name,
    traffic_type,
    registration_frequency,
    operational_status,
    road_reference,
    municipality_name,
    municipality_no,
    lat,
    lon
  ) |>
  split_road_system_reference()

data_time_span <-
  get_trp_data_time_span()


# Mjøsbyen ----
mjosbyen_municipality_numbers <-
  c(
    3441, # Gausdal
    3443, # Vestre Toten
    3420, # Elverum
    3411, # Ringsaker
    3405, # Lillehammer
    3413, # Stange
    3403, # Hamar
    3407, # Gjøvik
    3440, # Øyer
    3442, # Østre Toten
    3412  # Løten
  )

## TRPs ----
trp_mjosbyen <-
  trp |>
  dplyr::filter(
    traffic_type == "VEHICLE",
    registration_frequency == "CONTINUOUS",
    operational_status != "RETIRED",
    municipality_no %in% mjosbyen_municipality_numbers
  ) |>
  dplyr::left_join(
    data_time_span,
    by = "trp_id"
  ) |>
  dplyr::filter(
    !is.na(first_data_with_quality_metrics)
  ) |>
  dplyr::select(
    trp_id,
    name,
    road_reference,
    road_category_and_number,
    operational_status,
    municipality_no,
    municipality_name,
    first_data_with_quality_metrics,
    latest_daily_traffic,
    lat, lon
  ) |>
  dplyr::mutate(
    latest_daily_traffic =
      dplyr::case_when(
        latest_daily_traffic == lubridate::today() ~ "I dag",
        TRUE ~ as.character(latest_daily_traffic)
      ),
    name = stringr::str_to_title(name, locale = "nb")
  ) |>
  sf::st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
  )


## Links ----
roads_with_trps <-
  base::unique(
    trp_mjosbyen$road_category_and_number
  )

aadt_links <-
  purrr::map_dfr(
    mjosbyen_municipality_numbers,
    ~ get_aadt_by_area(.)
  )

aadt_links_filtered <-
  aadt_links |>
  dplyr::distinct(
    nvdb_objekt_id,
    .keep_all = TRUE
  ) |>
  dplyr::filter(
    road_category_and_number %in% roads_with_trps
  ) |>
  sf::st_zm(drop = T, what = "ZM")


# Need to buffer for join to detect overlap
trp_mjosbyen_buffered <-
  trp_mjosbyen |>
  sf::st_buffer(
    dist = 5
  )


# trp_links <-
#   sf::st_join(
#     trp_mjosbyen,
#     aadt_links_filtered,
#     largest = TRUE
#   )

links_with_trp <-
  sf::st_join(
    aadt_links_filtered,
    trp_mjosbyen_buffered,
    left = FALSE,
    largest = TRUE
  ) |>
  dplyr::select(
    trp_id,
    nvdb_objekt_id,
    name,
    road_reference,
    road_category_and_number = road_category_and_number.y,
    intersection_part,
    municipality_no,
    municipality_name,
    first_data_with_quality_metrics,
    latest_daily_traffic,
    aadt_total,
    heavy_percentage,
    year
  ) |>
  dplyr::mutate(
    label_text =
      paste(
        name, "<br/>",
        "ÅDT: ", aadt_total, "<br/>"
      ),
    label_text =
      lapply(label_text, htmltools::HTML)
  ) |>
  dplyr::arrange(
    municipality_no,
    road_reference
  )

readr::write_rds(
  links_with_trp,
  file = "mjosbyen.rds"
)
