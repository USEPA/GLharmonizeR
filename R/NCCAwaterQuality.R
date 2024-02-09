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
#' `.readSite` returns spatial data relating to study sites
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the directory of the data
#' @return dataframe
.readSite <- function(filepath) {
# pre 2000's doesn't report depth
# 2010 reports depth units, need to check if they're same
# all depths are reported in meters
  templateTable <- dplyr::tibble(
    SITE_ID = character(),
    LATITUDE = numeric(),
    LONGITUDE = numeric(),
    DEPTH = numeric())
  readr::read_csv(filepath, show_col_types=FALSE) %>%
    dplyr::select(SITE_ID, dplyr::contains(c("LAT_DD", "LON_DD", "DEPTH"))) %>%
    dplyr::select(-dplyr::contains(c("TLAT_DD", "TLON_DD", "83", "UNITS")))  %>%
    dplyr::rename(SITE_ID = 1, LATITUDE = 2, LONGITUDE = 3) %>%
    dplyr::rename_with(~ dplyr::case_when(
      . == "STATION_DEPTH" ~ "DEPTH",
      TRUE ~ .
    )) %>%
    # bind to template table to get all of the columns (even if empty)
    dplyr::bind_rows(templateTable) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::one_of('DEPTH')), as.numeric) %>%
    # file 3 has a bunch of empty rows at the end
    # file 2 has missing lat/lons for some reason
    drop_na()
}

#' Read in all NCCA site data
#' 
#' @description
#' `.readNCCAsites` returns spatial data relating to study sites
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the directory of the data
#' @return dataframe
.readNCCASites <- function(directory) {
  dir(path = directory, pattern = "site.*.csv", all.files =T, full.names=T, ignore.case = T) %>%
    purrr::map_dfr(.readSite)
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
    tidyr::pivot_longer(-c(SITE_ID, SAMPYEAR), names_to = "ANALYTE", values_to = "RESULT") 
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
      Date = DATE_COL)
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
           QACODE = NARS_FLAG,
           QAComment = NARS_COMMENT,
           UNITS = RESULT_UNITS,
           ANL_CODE = ANALYTE
          ) %>%
    dplyr::mutate(Date = lubridate::dmy(Date)) 
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
#' @return dataframe
#' @export
readNCCA <- function(siteFiles, preFiles=NULL, tenFiles=NULL, fifteenFiles=NULL){
  sites <- .readNCCASites(siteFiles) %>%
    dplyr::distinct(SITE_ID, .keep_all =T) 
  dfs <- list()
  if (!is.null(preFiles)) dfs[[1]] <- .readNCCA2000s(preFiles) else print("No early data specified")
  if (!is.null(tenFiles)) dfs[[2]] <- .readNCCA2010(tenFiles) else print("2010 files not specified")
  if (!is.null(fifteenFiles)) dfs[[3]] <- .readNCCA2015(fifteenFiles) else print("2015 files not specified")
  dplyr::bind_rows(dfs) %>%
    dplyr::left_join(sites, by = "SITE_ID")
    # QC filters
    #filter(! QACODE %in% c("J01", "Q08", "ND", "Q", "H", "L")) 
}

