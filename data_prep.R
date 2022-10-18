base::Sys.setlocale(locale = "nb.utf8")

source("H:/Programmering/R/byindeks/get_from_trafficdata_api.R")
source("H:/Programmering/R/byindeks/split_road_system_reference.R")

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

trp_mjosbyen <-
  trp |>
  dplyr::filter(
    traffic_type == "VEHICLE",
    registration_frequency == "CONTINUOUS",
    operational_status != "RETIRED",
    municipality_no %in% c(
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
      )
  )



