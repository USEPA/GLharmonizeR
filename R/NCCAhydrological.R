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
      # Confirmed that the reference date with Hugh and by reformatting in Excel 
      dplyr::mutate(
        SECCHI_TIME = round(as.numeric(SECCHI_TIME) * 24),
        DATE_COL = as.Date(DATE_COL, origin = "1900-1-1"),
        Date = paste(DATE_COL, SECCHI_TIME, sep = "_"),
        Date = lubridate::ymd_h(Date),
        ) %>%
      dplyr::mutate(
        MEAN_SECCHI_DEPTH = ifelse((CLEAR_TO_BOTTOM == "Y") | (grepl("estimate", SECCHI_COMMENT, ignore.case =TRUE)),
          NA, MEAN_SECCHI_DEPTH),
        SECCHI_COMMENT = paste(SECCHI_COMMENT, "ClearToBottom", sep = ";"),
        .by = c(UID)
      ) %>%
      # Average over all reps
      dplyr::reframe(
        SITE_ID = toString(unique(SITE_ID)),
        ANALYTE = "Secchi",
        DATE_COL = unique(DATE_COL),

        #### Confirm that we should just be taking the mean column with Hugh
        RESULT = mean(MEAN_SECCHI_DEPTH, na.rm= T),
        STATION_DEPTH_M = mean(STATION_DEPTH, na.rm=T),
        QA_COMMENT = toString(unique(SECCHI_COMMENT)),
        .by = UID) %>%
      dplyr::mutate(
        STUDY = "NCCA_secchi_2015"
      )
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
      sampleDepth = SDEPTH,
      ANALYTE = PARAMETER_NAME) %>%
    dplyr::mutate(
      # This is unaffected by being grouped
      DATE_COL = lubridate::mdy(DATE_COL),
      # Calculate CPAR for each UID at each depth
      ambientPAR = mean(ifelse(ANALYTE == "Ambient PAR", RESULT, NA), na.rm = TRUE),
      underPAR = mean(ifelse(ANALYTE == "Underwater PAR", RESULT, NA), na.rm = TRUE),
      CPAR = underPAR / ambientPAR,
      RESULT = dplyr::case_when(
        ANALYTE == "Ambient PAR" ~ CPAR,
        .default = RESULT
      ),
      # Change the names to CPAR
      ANALYTE = dplyr::case_when(
        ANALYTE == "Ambient PAR" ~ "Corrected PAR",
        .default = ANALYTE
      ),
      .by = c(UID, sampleDepth)
    ) %>%
    # Don't need to drop Ambient PAR because we enter CPAR in its stead
    dplyr::filter(
      ANALYTE != "Underwater PAR"
    ) %>%
    dplyr::reframe(
      RESULT = mean(RESULT, na.rm = T),
      .by = c(UID, sampleDepth, ANALYTE) 
    ) %>%
    dplyr::mutate(
      sampleDepth = ifelse(sampleDepth == -9.0, NA, sampleDepth),
      STUDY = "NCCA_hydro_2010"
    ) %>%
    dply::select(
      -c(ambientPAR, underPAR)
    )
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
    dplyr::mutate(
      `Corrected PAR` = LIGHT_UW / LIGHT_AMB
    ) %>%
    dplyr::select(
      -c(LIGHT_AMB, LIGHT_UW)
    ) %>%
    tidyr::pivot_longer(c(TRANS, CONDUCTIVITY:TEMPERATURE, `Corrected PAR`), names_to = "ANALYTE", values_to = "RESULT") %>%
    # I'm hesitant to use UID instead of DATE and SITE, just because I haven't verified that it is unique 
    dplyr::reframe(RESULT = mean(RESULT, na.rm = T),
            STATION_DEPTH = mean(STATION_DEPTH, na.rm = T),
            .by = c(UID, DATE_COL, SITE_ID, DEPTH, ANALYTE)) %>%
    # I decided not to select or rename columns until we are at the joining step
    # NOTE: I'm making up the Date so the join works for now but we
    # need to figure out how to actually parse it
    dplyr::mutate(
      DATE_COL = lubridate::ymd("2015-06-01"),
      STUDY = "NCCA_hydro_2015")
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