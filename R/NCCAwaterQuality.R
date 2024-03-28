#' Parse NCCA data files stored in a common directory 
#'
#' @description
#' `.parseNCCAData` returns filepaths of data NCCA stored files
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the directory of the data
#' @return dataframe
.parseNCCAData <- function(directory) {
  data.frame("file" = dir(directory)) %>%
    dplyr::mutate(
      year = as.integer(readr::parse_number(file)), 
      chem = grepl(file, pattern = "chem", ignore.case = TRUE),
      site = grepl(file, pattern = "site", ignore.case = TRUE),
      meta = grepl(file, pattern = "metadata", ignore.case = TRUE),
      qacodes = grepl(file, pattern = "QA_Codes", ignore.case = TRUE)
    ) %>%
    dplyr::arrange(year)
}

#' Read in NCCA site data 
#'
#' @description
#' `.readNCCASite2010` returns spatial data relating to study sites
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the directory of the data
#' @return dataframe
.readNCCASite2010 <- function(filepath) {
# all depths are reported in meters
  readr::read_csv(filepath, show_col_types=FALSE) %>%
    # cutdown number of lats and longs
    dplyr::rename(
      # No missingness, so no need to coalesce
      Latitude = ALAT_DD,
      Longitude = ALON_DD,
      stationDepth = STATION_DEPTH,
      NCCRreg = NCCR_REG
    ) %>%
    dplyr::select(SITE_ID, Latitude, Longitude, stationDepth, WTBDY_NM, NCCRreg) %>%

    # file 3 has a bunch of empty rows at the end
    # file 2 has missing lat/lons for some reason
    tidyr::drop_na()
}

#' Read in NCCA site data 
#'
#' @description
#' `.readNCCASite2015` returns spatial data relating to study sites
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the directory of the data
#' @return dataframe
.readNCCASite2015 <- function(filepath) {
# all depths are reported in meters
  readr::read_csv(filepath, show_col_types=FALSE) %>%
  # This column is all NAs
  dplyr::select(-WTBDY_NM) %>%
    # cutdown number of lats and longs
    dplyr::rename(
      # No missingness, so no need to coalesce
      Latitude = LAT_DD,
      Longitude = LON_DD,
      stationDepth = STATION_DEPTH,
      NCCRreg = NCCA_REG,
      WTBDY_NM = GREAT_LAKE
    ) %>%
    dplyr::select(SITE_ID, Latitude, Longitude, stationDepth, WTBDY_NM, NCCRreg) %>%

    # file 3 has a bunch of empty rows at the end
    # file 2 has missing lat/lons for some reason
    tidyr::drop_na()
}


#' Read in all NCCA site data
#' 
#' @description
#' `.readNCCASites` returns spatial data relating to study sites
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the directory of the data
#' @return dataframe
.readNCCASites <- function(ncca2010sites, ncca2015sites) {
  df <- .readNCCASite2010(ncca2010sites)
  df2 <- .readNCCASite2015(ncca2015sites) 

  return(dplyr::bind_rows(df, df2) %>% dplyr::distinct())

}

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
.readNCCA2000s <- function(filepath) {
  readr::read_csv(filepath,
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
    tidyr::pivot_longer(-c(SITE_ID, SAMPYEAR), names_to = "ANALYTE", values_to = "RESULT")  %>%
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
#' @param filepath a string specifying the directory of the data
#' @return dataframe
.readNCCA2010 <- function(filepaths) {
  filepaths %>%
    purrr::map_dfr(readr::read_csv,
            col_types = readr::cols(
              # Doesn't contain time
              "DATE_COL" = readr::col_date(format = "%m/%d/%Y"),
              "LAB_SAMPLE_ID" = "-",
              "SAMPLE_ID" = "-",
              "STATE" = "-",
              "BATCH_ID" = "-",
              "DATE_ANALYZED" = "-",
              "HOLDING_TIME" = "-",
            )) %>%
    dplyr::rename(
      ANL_CODE = PARAMETER,
      ANALYTE = PARAMETER_NAME,
      Date = DATE_COL) %>%
    dplyr::mutate(
      # Combine Nitrate adn Nitrite
      Nitrite = mean(ifelse(ANALYTE == "Nitrite", RESULT, NA), na.rm = TRUE),
      Nitrate = mean(ifelse(ANALYTE == "Nitrate", RESULT, NA), na.rm = TRUE),
      `Nitrate/Nitrite` = Nitrate + Nitrite,
      RESULT = dplyr::case_when(
        ANALYTE == "Nitrate" ~ `Nitrate/Nitrite`,
        .default = RESULT
      ),
      # Change the names to CPAR
      ANALYTE = dplyr::case_when(
        ANALYTE == "Nitrate" ~ "Nitrate/Nitrite",
        .default = ANALYTE
      ),
      ANL_CODE = dplyr::case_when(
        ANALYTE == "Nitrate" ~ "NOx",
        .default = ANALYTE
      ),

      .by = c(UID, SITE_ID, Date)
    ) %>%
    dplyr::select(
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
      Study = "NCCA_WQ_2010"
    )
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
.readNCCA2015 <- function(filepath) {
  readr::read_csv(filepath,
           col_types = readr::cols(
             "UID" = "d",
             "SITE_ID" = "c",
             # doesn't contain time
             "DATE_COL"= "c",
             "ANALYTE" = "c",
             "LRL" = "d",
             "MDL" = "d",
             "METHOD" = "c",
             "NARS_FLAG" = "c",
             "NARS_COMMENT" = "c",
             "RESULT" = "d",
             "RESULT_UNITS" = "c",
             .default = "-"
           )) %>%
    dplyr::rename(Date = DATE_COL,
           QAcode= NARS_FLAG,
           QAcomment = NARS_COMMENT,
           UNITS = RESULT_UNITS,
           ANL_CODE = ANALYTE
          ) %>%
    dplyr::mutate(
      Date = lubridate::dmy(Date), 
      # Combine Nitrate adn Nitrite
      Nitrite = mean(ifelse(ANL_CODE == "NITRITE_N", RESULT, NA), na.rm = TRUE),
      Nitrate = mean(ifelse(ANL_CODE == "NITRATE_N", RESULT, NA), na.rm = TRUE),
      `Nitrate/Nitrite` = Nitrate + Nitrite,
      RESULT = dplyr::case_when(
        ANL_CODE == "NITRATE_N" ~ `Nitrate/Nitrite`,
        .default = RESULT
      ),
      # Change the names 
      ANL_CODE = dplyr::case_when(
        ANL_CODE == "NITRATE_N" ~ "NOx",
        .default = ANL_CODE 
      ),
      .by = c(UID, SITE_ID, Date)
    ) %>%
    dplyr::select(
      -dplyr::contains("NITR")
    ) %>%
    # Don't need to drop Ambient PAR because we enter CPAR in its stead
    dplyr::filter(
      ANL_CODE != "NITRITE_N"
    ) %>%
    # All NCCA WQ samples at 0.5m
    dplyr::mutate(
      sampleDepth = 0.5,
      Study = "NCCA_WQ_2015"
    )
}

# 2020/2021
# SITE_ID, Date_COL,  LAT/LON_DD (not 83), STATION_DEPTH


#' Read in all NCCA from 2000s, 2010, and 2015
#' 
#' @description
#' `readNCCA` returns water quality data along with spatial data from the 
#'  site information measured through NCCA study in the early 2000s as well as in 2010, and 2015
#' 
#' @details
#' The spatial information for sites is read in using the .readSites helper functions, this is then
#' joined to the water quality and hydrographic data and ultimately output as a data table.
#' @param filepath a string specifying the directory of the data
#' 
#' @return dataframe
.readNCCA <- function(tenFiles=NULL, fifteenFiles=NULL){
  dfs <- list()
  if (!is.null(tenFiles)) dfs[[1]] <- .readNCCA2010(tenFiles) else print("2010 WQ filepath not specified or trouble finding")
  if (!is.null(fifteenFiles)) dfs[[2]] <- .readNCCA2015(fifteenFiles) else print("2015 WQ filepath not specified or trouble finding")
  dplyr::bind_rows(dfs)
    # QC filters
    #filter(! QACODE %in% c("J01", "Q08", "ND", "Q", "H", "L")) 
}

