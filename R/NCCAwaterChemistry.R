#' Read in all NCCA water quality from the early 2000s
#' 
#' @description
#' `.readNCCA2000s` returns water quality data measured during the NCCA study in the early 2000s
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the directory of the data
#' @return dataframe
.readNCCA2000s <- function(filepath, n_max = Inf) {
  # TODO move this function to an old functions folder
  readr::read_csv(filepath,
                  n_max = n_max,
                  col_types = readr::cols(
                    # Doesn't contain date nor time
                    .default = "-",
                    "SITE_ID" = "c",
                    "SAMPYEAR" = "d",
                    "Chla ug/L" = "d",
                    "DIN mg N/L" = "d",
                    "DIP mg P/L" = "d",
                    "DO mg/L" = "d",
                    "Light transmittance (%) @ 1 m" = "d",
                    "Chla Cond" = "-",
                    "DIN Cond" = "-",
                    "DIP Cond" = "-",
                    "DO Cond" = "-",
                    "Trans Cond" = "-",
                  )) %>%
    tidyr::pivot_longer(-c(SITE_ID, Year), names_to = "ANALYTE", values_to = "RESULT")  %>%
    dplyr::mutate(
      sampleDepth = 0.5,
      Study = "NCCA_WQ_2000s"
    )
    # Make a missing columns for depths to align with other data sources
    #mutate(DEPTH = NA)
}

#' Read in all NCCA water quality from 2010 
#' 
#' @description
#' `.readNCCA2010` returns water quality data measured during the NCCA study in 2010
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepaths a string specifying the directory of the data
#' @return dataframe
.readNCCA2010 <- function(filepaths, n_max = Inf) {
  filepaths %>%
    purrr::map_dfr(readr::read_csv,
            n_max = n_max,
            col_types = readr::cols(
              # TODO Load MDL, MRL, PQL
              "DATE_COL" = readr::col_date(format = "%m/%d/%Y"),
              "LAB_SAMPLE_ID" = "-",
              "SAMPLE_ID" = "-",
              "BATCH_ID" = "-",
              "DATE_ANALYZED" = "-",
              "HOLDING_TIME" = "-",
            )) %>%
    dplyr::rename(
      ANL_CODE = PARAMETER,
      ANALYTE = PARAMETER_NAME,
      sampleDate = DATE_COL) %>%
    dplyr::mutate(
      # Combine Nitrate  Nitrite
      Nitrite = mean(ifelse(ANALYTE == "Nitrite", RESULT, NA), na.rm = TRUE),
      Nitrate = mean(ifelse(ANALYTE == "Nitrate", RESULT, NA), na.rm = TRUE),
      `Nitrate/Nitrite` = Nitrate + Nitrite,
      # TODO does this create a problem (check if whenever Nitrate is missing Nitrite is missing)
      # TODO maybe easier to do pivot_wider first
      # TODO make sure Nitrate and Nitrite are already ug/L
      # DOCTHIS We assume sampling events that don't have certain analytes reported
      # DOCTHIS remove the observation when either one is missing  Also for Nitrate / Nitrite
      # TODO remove vlaues if one is missing
      RESULT = dplyr::case_when(
        ANALYTE == "Nitrate" ~ `Nitrate/Nitrite`,
        .default = RESULT
      ),
      ANALYTE = dplyr::case_when(
        ANALYTE == "Nitrate" ~ "Nitrate/Nitrite",
        .default = ANALYTE
      ),
      ANL_CODE = dplyr::case_when(
        ANALYTE == "Nitrate" ~ "Diss_NOx",
        .default = ANALYTE
      ),
      UNITS = dplyr::case_when(
        ANALYTE == "Nitrate" ~ "ugL",
        .default = UNITS 
      ),
      .by = c(UID, SITE_ID, sampleDate)
    ) %>%
    dplyr::select(
      # Remove the columns that we created called Nitrate, Nitrite and Nitrate/Nitrite
      -dplyr::contains("Nitr")
    ) %>%
    # Don't need to drop Ambient PAR because we enter CPAR in its stead
    dplyr::filter(
      ANALYTE != "Nitrite"
    ) %>%
    # All NCCA WQ samples at 0.5m
    dplyr::mutate(
      sampleDepth = 0.5
    ) %>%
    dplyr::mutate(
      # TODO add this to Analytes3
      Study = "NCCA_WChem_2010"
    ) %>%
    dplyr::mutate(
      QACODE = paste(QACODE, ifelse(STATE == "WI", "WSLH", ""), sep = ";"))
}

#' Read in all NCCA water quality from 2015
#'
#' @description
#' `.readNCCA2015` returns water quality data measured during the NCCA study in 2015
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the directory of the data
#' @return dataframe
.readNCCA2015 <- function(filepath, n_max = Inf) {
  readr::read_csv(filepath,
           n_max = n_max,
           col_types = readr::cols(
             "UID" = "d",
             "SITE_ID" = "c",
             "DATE_COL"= "c",
             "ANALYTE" = "c",
             "LRL" = "d",
             "MDL" = "d",
             "METHOD" = "c",
             "NARS_FLAG" = "c",
             "NARS_COMMENT" = "c",
             "RESULT" = "d",
             "RESULT_UNITS" = "c"
           )) %>%
    dplyr::rename(
      sampleDate = DATE_COL,
      QAcode= NARS_FLAG,
      QAcomment = NARS_COMMENT,
      UNITS = RESULT_UNITS,
      ANL_CODE = ANALYTE
    ) %>%
    dplyr::mutate(
      sampleDate = lubridate::dmy(sampleDate), 
      # Combine Nitrate adn Nitrite
      # TODO does this create a problem (check if whenever Nitrate is missing Nitrite is missing)
      # TODO maybe easier to do pivot_wider first
      # TODO make sure Nitrate and Nitrite are already ug/L
      # TODO make 2 separate mutates so the .by goes more obvsiously with the mean (if not pivoted wider)
      Nitrite = mean(ifelse(ANL_CODE == "NITRITE_N", RESULT, NA), na.rm = TRUE),
      Nitrate = mean(ifelse(ANL_CODE == "NITRATE_N", RESULT, NA), na.rm = TRUE),
      `Nitrate/Nitrite` = Nitrate + Nitrite,
      RESULT = dplyr::case_when(
        ANL_CODE == "NITRATE_N" ~ `Nitrate/Nitrite`,
        .default = RESULT
      ),
      # Change the names 
      ANL_CODE = dplyr::case_when(
        ANL_CODE == "NITRATE_N" ~ "Diss_NOx",
        .default = ANL_CODE 
      ),
      UNITS = dplyr::case_when(
        # Replacing units that had NA's added to their end to make things simpler
        (ANL_CODE == "AMMONIA_N") & (lubridate::year(sampleDate) == 2015) ~ "mgL",
        (ANL_CODE == "Diss_NOx") & (lubridate::year(sampleDate) == 2015) ~ "mgL",
        (ANL_CODE == "SRP") & (lubridate::year(sampleDate) == 2015) ~ "mgL",
        (ANL_CODE == "PTL") & (lubridate::year(sampleDate) == 2015) ~ "mgL",
        .default = UNITS
      ),
      .by = c(UID, SITE_ID, sampleDate)
    ) %>%
    dplyr::select(
      -dplyr::contains("NITR")
    ) %>%
    # Don't need to drop Nitrate because we enter Nitrate/Nitrite in its stead
    dplyr::filter(
      ANL_CODE != "NITRITE_N"
    ) %>%
    # All NCCA WQ samples at 0.5m
    dplyr::mutate(
      sampleDepth = 0.5,
      Study = "NCCA_WChem_2015"
    ) %>%
    dplyr::mutate(
      QAcode = paste(QAcode, ifelse(LAB == "WSLH", "WSLH", ""), sep = ";"),
      QAcomment = paste(QAcode, ifelse(LAB == "WSLH", "WSLH used large filters for Chla-A", ""), sep = ";")
      )
}

# 2020/2021
# SITE_ID, Date_COL,  LAT/LON_DD (not 83), STATION_DEPTH


#' Read in all NCCA from 2000s, 2010, and 2015
#' 
#' @description
#' `.readNCCAchemistry` returns water quality data along with spatial data from the 
#'  site information measured through NCCA study in the early 2000s as well as in 2010, and 2015
#' 
#' @details
#' The spatial information for sites is read in using the .readSites helper functions, this is then
#' joined to the water quality and ultimately output as a data table.
#' @param filepath a string specifying the directory of the data
#' 
#' @return dataframe
.readNCCAchemistry <- function(tenFiles=NULL, fifteenFiles=NULL, nccaWQqaFile = NULL, n_max = n_max){
  dfs <- list()
  if (!is.null(tenFiles)) dfs[[1]] <- .readNCCA2010(tenFiles, n_max = n_max) else print("2010 WQ filepath not specified or trouble finding")
  if (!is.null(fifteenFiles)) dfs[[2]] <- .readNCCA2015(fifteenFiles, n_max = n_max) else print("2015 WQ filepath not specified or trouble finding")

  # TODO check if bind_rows breaks with one file
  dfs <- dplyr::bind_rows(dfs) %>%
    # QC filters
    #filter(! QACODE %in% c("J01", "Q08", "ND", "Q", "H", "L")) 
    dplyr::mutate(SAMPYEAR = lubridate::year(sampleDate))

  # TODO add a try catch if the filepath isn't included 
  QA <- readxl::read_xlsx(nccaWQqaFile, sheet = "NCCAQAcounts2", .name_repair = "unique_quiet") 

  dfs %>%
    dplyr::left_join(QA, by = c("SAMPYEAR", "QAcode", "ANALYTE", "ANL_CODE")) %>%
    dplyr::mutate(
      RESULT = dplyr::case_when(
        Decision == "Keep" ~ RESULT,
        Decision == "Impute" ~ NA,
        Decision == "Estimate" ~ RESULT,
        Decision == "Remove" ~ NA,
        .default = RESULT),
      FLAG = dplyr::case_when(
        Decision == "Keep" ~ NA,
        Decision == "Impute" ~ "Impute value using one or more detect limits (see QA comment)",
        Decision == "Estimate" ~ "Value estimate",
        Decision == "Remove" ~ NA,
        .default = NA)
    ) %>%
    dplyr::filter(Decision != "Remove")
}
