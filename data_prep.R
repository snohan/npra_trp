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


# Kristiansand ----
krs_municipality_numbers <-
  c(
    4204, # Kristiansand
    4223  # Vennesla
  )

city_index_trps <-
  get_published_pointindex(
    957,
    2022,
    1
  )[[2]] |>
  dplyr::filter(
    period == "month"
  )

## TRPs ----
trp_krs <-
  trp |>
  dplyr::filter(
    traffic_type == "VEHICLE",
    registration_frequency == "CONTINUOUS",
    operational_status != "RETIRED",
    municipality_no %in% krs_municipality_numbers
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
    name = stringr::str_to_title(name, locale = "nb"),
    city_index = trp_id %in% city_index_trps$trp_id,
    city_index = dplyr::case_when(
      city_index == FALSE ~ "Nei",
      TRUE ~ "Ja"
    )
  ) |>
  sf::st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
  )

## Links ----
roads_with_trps <-
  base::unique(
    trp_krs$road_category_and_number
  )

aadt_links <-
  purrr::map_dfr(
    krs_municipality_numbers,
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
trp_buffered <-
  trp_krs |>
  sf::st_buffer(
    dist = 5
  )

links_with_trp <-
  sf::st_join(
    aadt_links_filtered,
    trp_buffered,
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
    year,
    city_index
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
  ) |>
  dplyr::filter(
    nvdb_objekt_id != 1015032483
    # Skibåsen feilplassert, overlapper med to lenker
  )

trps_without_link <-
  trp_krs |>
  dplyr::filter(
    !(trp_id %in% links_with_trp$trp_id)
  )
# Hånes and Timenes are on the same link - the link should be split!
# Brennåsen øst and vest are on the same link -
# they should be merged to a virtual TRP

readr::write_rds(
  links_with_trp,
  file = "krs.rds"
)


# Oslo ----
osl_municipality_numbers <-
  c(
    301,  # Oslo
    3024, # Bærum
    3025, # Asker
    3020, # Nordre Follo
    3023, # Nesodden
    3022, # Frogn
    3021, # Ås
    3019, # Vesby
    3023, # Enebakk
    3029, # Lørenskog
    3027, # Rælingen
    3030, # Lillestrøm
    3031, # Nittedal
    3032, # Gjerdrum
    3033, # Ullensaker
    3036, # Nannestad
    3034, # Nes
    3026 # Aurskog-Høland
  )

city_index_trps <-
  get_published_pointindex_bike(
    6953,
    2022,
    8
  )[[2]] |>
  dplyr::filter(
    period == "month"
  )

## TRPs ----
trp_osl_bike <-
  trp |>
  dplyr::filter(
    traffic_type == "BICYCLE",
    registration_frequency == "CONTINUOUS",
    operational_status != "RETIRED",
    municipality_no %in% osl_municipality_numbers
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
    first_data_with_quality_metrics =
      as.character(first_data_with_quality_metrics),
    name = stringr::str_to_title(name, locale = "nb"),
    city_index = trp_id %in% city_index_trps$trp_id,
    city_index = dplyr::case_when(
      city_index == FALSE ~ "Nei",
      TRUE ~ "Ja"
    ),
    label_text =
      paste(
        name, "<br/>",
        "oppstart: ", first_data_with_quality_metrics
      ),
    label_text =
      lapply(label_text, htmltools::HTML)
  ) |>
  sf::st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
  )

readr::write_rds(
  trp_osl_bike,
  file = "oslo_bike_trp.rds"
)
