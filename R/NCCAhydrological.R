
# Hydrological data

.readNCCASecchi2015 <- function(filepath) {
  readr::read_csv(filepath) %>%
    dplyr::mutate(
      # Assume if not reported clear to bottom, it is not clear to bottom
      CLEAR_TO_BOTTOM = ifelse(is.na(CLEAR_TO_BOTTOM), "N", CLEAR_TO_BOTTOM),
      # Either it's clear to bottom, or they took measurements, so average them
      # Only mean or both dissappear and reappear exist at a time, so we can include all three in average without biasing
      Secchi = ifelse(CLEAR_TO_BOTTOM == "Y", STATION_DEPTH, rowMeans(select(., MEAN_SECCHI_DEPTH, DISAPPEARS, REAPPEARS), na.rm = TRUE))
      ) %>% 
      # Average over all reps
      dplyr::reframe(
        SITE_ID = toString(unique(SITE_ID)),
        DATE_COL = toString(unique(DATE_COL)), 
        ANALYTE = "Secchi",
        RESULT = mean(Secchi, na.rm= T),
        STATION_DEPTH_M = mean(STATION_DEPTH, na.rm=T),
        QA_COMMENT = toString(unique(SECCHI_COMMENT)), 
        .by = UID) %>%
      # Temporarily drop the date until we figureo out how to parse it
      dplyr::mutate(DATE_COL = lubridate::ymd("2015-01-01"))

}

.readNCCAhydro2010 <- function(filepaths) {
  filepaths %>%
    purrr::map_dfr(read_csv) %>%
    dplyr::select(UID, SITE_ID, DATE_COL, SDEPTH, PARAMETER_NAME, RESULT, UNITS, QA_CODE, QA_COMMENT) %>%
    dplyr::rename(
      SAMPLE_DEPTH_M = SDEPTH,
      ANALYTE = PARAMETER_NAME) %>%
    dplyr::mutate(DATE_COL = mdy(DATE_COL))
}


.readNCCAhydro2015 <- function(filepath) {
  readr::read_csv(filepath) %>%
    tidyr::pivot_longer(-c(1:12,14,15, 24), names_to = "ANALYTE", values_to = "RESULT") %>%
    dplyr::select(UID, SITE_ID, DATE_COL, STATION_DEPTH, ANALYTE, RESULT) %>%
    dplyr::rename(SAMPLE_DEPTH_M = STATION_DEPTH) %>%
    # Temporarily drop the date until we figureo out how to parse it
    dplyr::mutate(DATE_COL = lubridate::ymd("2015-01-01"))
}

.readNCCAhydro <- function(hydrofiles2010, hydrofile2015, secchifile2015) {
  dplyr::bind_rows(
    .readNCCAhydro2010(hydrofiles2010), 
    .readNCCAhydro2015(hydrofile2015), 
    .readNCCASecchi2015(secchifile2015))
}