#' Load and join secchi data for NCCA 2015 from csv files 
#'
#' @description
#' `.readNCCASecchi2015` returns a dataframe of all of the joined secchi data relating to NCCA 2015 
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the filepath of the data
#' @return dataframe of the fully joined secchi data from NCCA 2015
.readNCCASecchi2015 <- function(filepath) {
  readr::read_csv(filepath) %>%
    dplyr::mutate(
      # Assume if not reported clear to bottom, it is not clear to bottom
      CLEAR_TO_BOTTOM = ifelse(is.na(CLEAR_TO_BOTTOM), "N", CLEAR_TO_BOTTOM),
      # Either it's clear to bottom, or they took measurements, so average them
      # Only mean or both dissappear and reappear exist at a time, so we can include all three in average without biasing
      Secchi = ifelse(CLEAR_TO_BOTTOM == "Y", STATION_DEPTH, rowMeans(dplyr::select(., MEAN_SECCHI_DEPTH, DISAPPEARS, REAPPEARS), na.rm = TRUE))
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

#' Load and join secchi data for NCCA 2010 hydrographic data from csv files 
#'
#' @description
#' `.readNCCAhydro2010` returns a dataframe of all of the hydrographic data relating to NCCA 2010 
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the filepath of the data
#'  
#' @return dataframe
.readNCCAhydro2010 <- function(filepaths) {
  filepaths %>%
    purrr::map_dfr(readr::read_csv) %>%
    dplyr::select(UID, SITE_ID, DATE_COL, SDEPTH, PARAMETER_NAME, RESULT, UNITS, QA_CODE, QA_COMMENT) %>%
    dplyr::rename(
      SAMPLE_DEPTH_M = SDEPTH,
      ANALYTE = PARAMETER_NAME) %>%
    dplyr::mutate(DATE_COL = lubridate::mdy(DATE_COL))
}


#' Load and join secchi data for NCCA 2015 hydrographic data from csv files 
#'
#' @description
#' `.readNCCAhydro2015` returns a dataframe of all of the hydrographic data relating to NCCA 2010 
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the filepath of the data
#' @return dataframe
.readNCCAhydro2015 <- function(filepath) {
  readr::read_csv(filepath) %>%
    tidyr::pivot_longer(c(TRANS, CONDUCTIVITY:TEMPERATURE), names_to = "ANALYTE", values_to = "RESULT") %>%
    # combine measurements for similar depths across cast directions rounded to the nearest meter
    dplyr::mutate(DEPTH = round(DEPTH, 0)) %>%
    # I'm hesitant to use UID instead of DATE and SITE, just because I haven't verified that it is unique 
    dplyr::reframe(RESULT = mean(RESULT, na.rm = T),
            STATION_DEPTH = mean(STATION_DEPTH, na.rm = T),
            .by = c(UID, DATE_COL, SITE_ID, DEPTH, ANALYTE)) %>%
    # I decided not to select or rename columns until we are at the joining step
    # NOTE: I'm making up the Date so the join works for now but we
    # need to figure out how to actually parse it
    dplyr::mutate(DATE_COL = lubridate::ymd("2015-06-01"))
}

#' Load and join hydrographic and secchi data for NCCA 2010 and 2015 
#'
#' @description
#' `.readNCCAhydro` returns a dataframe of all of the hydrographic data relating to NCCA 2010 and 2015
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the filepath of the data
#' @return dataframe
.readNCCAhydro <- function(hydrofiles2010, hydrofile2015, secchifile2015) {
  dplyr::bind_rows(
    .readNCCAhydro2010(hydrofiles2010), 
    .readNCCAhydro2015(hydrofile2015), 
    .readNCCASecchi2015(secchifile2015))
}