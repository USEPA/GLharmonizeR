joinGLENDASeaBird <- function(glendaDf, seaBirdFiles) {
  seaBirdDf <- seaBirdFiles %>%
    purrr::map(seabird2df) %>%
    dplyr::bind_rows()

  dplyr::full_join(glendaDf, seaBirdDf, by = c("STATION_ID" = "Station", "sampleDate"))
}

