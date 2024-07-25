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
.readNCCASecchi2015 <- function(NCCAsecchifile2015, n_max = Inf) {
  df <- readr::read_csv(NCCAsecchifile2015, n_max = n_max, show_col_types= FALSE) %>%
    dplyr::filter(
      # Kept estimated and based on trans
      # We will explicitly test if the measurements are greater than station depth
      !grepl("missing for this site", SECCHI_COMMENT, ignore.case = T),
      !grepl("unavailable for this site", SECCHI_COMMENT, ignore.case = T)
    ) %>%
    # Confirmed that the reference date with Hugh and by reformatting in Excel
    dplyr::mutate(
      SECCHI_TIME = round(as.numeric(SECCHI_TIME) * 24),
      DATE_COL = as.Date(DATE_COL, origin = "1900-1-1"),
      sampleDate = paste(DATE_COL, SECCHI_TIME, sep = "_"),
      sampleDateTime = lubridate::ymd_h(sampleDate),
    ) %>%
    # This may look like we are keeping MEAN_SECCHI_DEPTH to average with the others,
    # However, we filter it out in the mean call
    tidyr::pivot_longer(c(MEAN_SECCHI_DEPTH, DISAPPEARS, REAPPEARS), names_to = "SecchiType", values_to = "RESULT") %>%
    dplyr::reframe(
      SITE_ID = toString(unique(SITE_ID)),
      ANALYTE = "Secchi",
      DATE_COL = unique(DATE_COL),
      stationDepth = mean(STATION_DEPTH, na.rm = T),

      # Mean of everything but the estimated value (because we think this is estimated by Kd)
      RESULT = mean(ifelse(SecchiType != "MEAN_SECCHI_DEPTH", RESULT, NA), na.rm = T),
      # Compress all comments and note clear to bottom to be combined
      # [x] change CLEAR_TO_BOTTOM to actually checking if Disappear/Reappear >= to Depth
      # Check to see if this works
      CLEAR_TO_BOTTOM = RESULT >= stationDepth,
      QAcomment = toString(unique(SECCHI_COMMENT)),
      .by = c(UID)
    ) %>%
    dplyr::mutate(
      RESULT = dplyr::case_when(
        CLEAR_TO_BOTTOM == TRUE ~ NA,
        CLEAR_TO_BOTTOM == FALSE ~ RESULT,
        is.na(CLEAR_TO_BOTTOM) ~ RESULT
      ),
      QAcomment = dplyr::case_when(
        CLEAR_TO_BOTTOM == TRUE ~ paste("Clear to bottom", QAcomment, sep = ";"),
        CLEAR_TO_BOTTOM == FALSE ~ QAcomment,
        is.na(CLEAR_TO_BOTTOM) ~ QAcomment
      )
    ) %>%
    dplyr::mutate(
      Study = "NCCA_secchi_2015"
    ) %>%
    tidyr::drop_na(RESULT)
  return(df)
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
.readNCCAhydro2010 <- function(NCCAhydrofiles2010, NCCAwqQA, n_max = n_max) {
  # Read qa decisions
  QA <- openxlsx::read.xlsx(NCCAwqQA, sheet = "NCCAQAcounts2")

  df <- NCCAhydrofiles2010 %>%
    purrr::map_dfr(readr::read_csv, n_max = n_max, show_col_types= FALSE) %>%
    # filter to just downcast
    dplyr::filter(CAST == "DOWNCAST") %>%
    dplyr::left_join(QA, by = c("QA_CODE" = "QAcode")) %>%
    # [x] filter out based on QA decisions
    dplyr::filter(!grepl("remove", Decision, ignore.case = T)) %>%
    dplyr::filter(!((Decision == "Remove based on comment") & (grepl("suspect", QA_COMMENT)))) %>%
    dplyr::mutate(
      RESULT = dplyr::case_when(
        Decision == "Impute" ~ NA,
        Decision == "Keep" ~ RESULT,
        Decision == "CTB" ~ NA,
        .default = RESULT
      ),
      FLAG = dplyr::case_when(
        Decision == "Impute" ~ paste(QA_COMMENT, ";", "Value is appropriate to impute"),
        Decision == "CTB" ~ paste(QA_COMMENT, ";", "Clear to Bottom"),
        .default = QA_COMMENT
      ),
      ANALYTE = dplyr::coalesce(PARAMETER_NAME, ANALYTE)
    ) %>%
    dplyr::rename(
      sampleDepth = SDEPTH,
      stationDepth = `STATION_DEPTH(m)`
    ) %>%
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
      .by = c(UID, sampleDepth, stationDepth)
    ) %>%
    # [x] filter out where either ambientPAR or underPAR check if this does what we think
    dplyr::filter((!is.na(ambientPAR)) | (!is.na(underPAR))) %>%
    # Don't need to drop Ambient PAR because we enter CPAR in its stead
    dplyr::filter(
      ANALYTE != "Underwater PAR"
    ) %>%
    # [x] this don't need reframe since I filtered to downcast
    dplyr::mutate(
      sampleDepth = ifelse(sampleDepth == -9.0, NA, sampleDepth),
      Study = "NCCA_hydro_2010"
    ) %>%
    dplyr::select(DATE_COL)
}


#' Load and join secchi data for NCCA 2015 hydrographic data from csv files
#'
#' @description
#' `.readNCCAhydro2015` returns a dataframe of all of the hydrographic data relating to NCCA 2010
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param NCCAhydrofile2015 a string specifying the filepath of the data
#' @return dataframe
.readNCCAhydro2015 <- function(NCCAhydrofile2015, n_max = Inf) {
  readr::read_csv(NCCAhydrofile2015, n_max = n_max, show_col_types= FALSE) %>%
    # the only comments mention no measurment data or typo
    dplyr::filter(is.na(NARS_COMMENT)) %>%
    dplyr::filter(CAST == "DOWNCAST") %>%
    dplyr::mutate(
      `Corrected PAR` = LIGHT_UW / LIGHT_AMB,
      sampleDateTime = as.Date(DATE_COL, origin = "1900-1-1"),
      Study = "NCCA_hydro_2015"
    ) %>%
    dplyr::filter(!is.na(LIGHT_UW) | !is.na(LIGHT_AMB)) %>%
    dplyr::select(
      -c(LIGHT_AMB, LIGHT_UW)
    ) %>%
    dplyr::rename(sampleDepth = DEPTH, stationDepth = STATION_DEPTH) %>%
    tidyr::pivot_longer(c(TRANS, CONDUCTIVITY:TEMPERATURE, `Corrected PAR`), names_to = "ANALYTE", values_to = "RESULT") %>%
    dplyr::select(-DATE_COL)
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
.readNCCAhydro <- function(
    NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015,
    NCCAwqQA, n_max = Inf) {
  dplyr::bind_rows(
    .readNCCAhydro2010(NCCAhydrofiles2010, NCCAwqQA = NCCAwqQA, n_max = n_max),
    .readNCCAhydro2015(NCCAhydrofile2015, n_max = n_max),
    .readNCCASecchi2015(NCCAsecchifile2015, n_max = n_max)
  )
}
