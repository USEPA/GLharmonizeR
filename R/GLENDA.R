#' readPivotGLENDA
#'
#' @description 
#' A function to read the full GLENDA csv file and convert it to a more user friendly long format.
#' 
#' @details
#' `.readPivotGLENDA` This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset. This function contains some 
#' of the filtering functions in order to easily be compatible with the testing schema because 
#' when we filtered later on, we lost too many sample ids that would be included in the test data.
#' 
#'  
#' @param filepath a filepath to the GLENDA csv
#' @param n_max Number of rows to read in from the raw GLENDA data (this is really just for testing purposes)
#' @param sampleIDs a list of sampleIDs to keep in the final dataset
#'
#' @return a dataframe
.readPivotGLENDA <- function(glendaData, n_max = Inf, sampleIDs = NULL) {
  df <- glendaData %>%
    { if (grepl(tools::file_ext(glendaData), ".csv", ignore.case = TRUE)) {
      readr::read_csv(.,
             col_types = readr::cols(
              YEAR = "i",
              STN_DEPTH_M = "d",
              LATITUDE = "d",
              LONGITUDE = "d",
              SAMPLE_DEPTH_M = "d",
              SAMPLING_DATE = col_datetime(format = "%Y/%m/%d %H:%M"),
              # Skip useless or redundant columns
              Row = "-",
              .default = "c"),
            n_max = n_max) 
      } else . } %>%
    { if (grepl(tools::file_ext(glendaData), ".Rds", ignore.case = TRUE)) {
      readRDS(.) %>%
        # This is so that everything can be pivoted, we change 
        # to final datatypes later once it's in long format
        dplyr::mutate(across(everything(), as.character)) %>%
        dplyr::mutate(Year = as.integer(YEAR),
              STN_DEPTH_M = as.double(STN_DEPTH_M),
              Latitude = as.double(LATITUDE),
              Longitude = as.double(LONGITUDE),
              SAMPLE_DEPTH_M = as.double(SAMPLE_DEPTH_M),
              SAMPLING_DATE = lubridate::ymd_hm(SAMPLING_DATE)) %>%
              dplyr::select(-Row)
    } else .} %>%
    # this if statement is only for saving test data
    { if (!is.null(sampleIDs)) {
      dplyr::filter(.,
        SAMPLE_ID %in% sampleIDs
      )}
      else . } %>%
    tidyr::pivot_longer(cols = -c(1:18),
                 names_to = c(".value", "Number"),
                 names_pattern = "(.*)_(\\d*)$") %>%
    # there are a lot of empty columns because of the data storage method
    # so once they are pivoted they are appear as missing
    tidyr::drop_na(ANALYTE) %>%
    # Select samples that haven't been combined
    dplyr::filter(
      SAMPLE_TYPE %in% c("Individual", "INSITU_MEAS"),
      QC_TYPE == "routine field sample",
      # If value and remarks are missing, we assume sample was never taken
      !is.na(VALUE) | !is.na(RESULT_REMARK),
      # The only QA Codes worth removing "Invalid" and "Known Contamination".
      # The rest already passed an initial QA screening before being entered
      !grepl("Invalid", RESULT_REMARK, ignore.case= T),
      RESULT_REMARK != "Known Contamination"
    )
  return(df)
}


#' cleanGLENDA
#'
#' @description 
#' A function to perform automated QC on the GLENDA data
#' 
#' @details
#' `.cleanGLENDA` This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' 
#' @param df GLENDA dataframe in long format
#' @param flagsPath (optional) filepath to the Result remarks descriptions. Default is NULL.
#' @param imputeCoordinates (optional) Boolean specifying whether to impute missing station coordinates, 
#' This shouldn't need to be used, since after joining sites missingness is removed
#' @param siteCoords (optional) , deprecated, filepath to list of site coordinates to impute missing lats/lons
#' @param namingFile (optional) filepath to a file containing remappings for analyte names 
#'
#' @return a dataframe
.cleanGLENDA <- function(df, namingFile, flagsPath= NULL, imputeCoordinates = FALSE, siteCoords = NULL, limitsPath = NULL) {

  renamingTable <- readxl::read_xlsx(namingFile, sheet= "GLENDA_Map", na = c("", "NA"),
    .name_repair = "unique_quiet") 
  key <- readxl::read_xlsx(namingFile, sheet = "Key", .name_repair = "unique_quiet") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- readxl::read_xlsx(namingFile, sheet = "UnitConversions", .name_repair = "unique_quiet") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))

  df <- df %>%
    # Convert daylight saving TZs into standard time TZs
    dplyr::mutate(
      # [ ] Check if remove RESULTstart
      # RESULTstart = VALUE,
      TIME_ZONE= dplyr::case_when(
        TIME_ZONE == "EDT" ~ "America/Puerto_Rico",
        TIME_ZONE == "CDT" ~ "EST",
        .default = TIME_ZONE
      ),
      SAMPLING_DATE = stringr::str_remove(SAMPLING_DATE, " UTC$"),
      # Some have missing times, so impute 12 noon where necessary
      SAMPLING_DATE = ifelse(
        stringr::str_length(SAMPLING_DATE) < 14,
        paste(SAMPLING_DATE, "12:00:00"),
        SAMPLING_DATE
      ) 
      # [ ] ADD A FLAG whereever we assumed it was noon
    ) %>%
    tidyr::unite(sampleDate, SAMPLING_DATE, TIME_ZONE) %>%
    dplyr::mutate(sampleDate = readr::parse_datetime(sampleDate, format = "%Y-%m-%d %H:%M:%S_%Z")) %>%


    # Drop analyte number since it doesn't mean anything now
    # These columns are redundant with the "Analyte" columns
    dplyr::select(-Number) %>%
    # Adding verbose remark descriptions is purely optional
    {if (!is.null(flagsPath)) {
      dplyr::left_join(., readxl::read_xlsx(flagsPath), by = c("RESULT_REMARK" = "NAME"), .name_repair = "unique_quiet")
      } else .
    } %>%
    # Haven't pursued because only has limits prior to 2012
    # So downgraded it's priority
    # [ ] Add in the limits
    # [ ] Extract the < values for all datas, join it back to the pdf extracted RL's
    # [ ] Join them all together then join back to the data
    # [ ] Give priority to the pdf source
    # [ ] Code the flags MDL and DL are the same
    #{if (!is.null(limitsPath)) {
    #  dplyr::left_join(., readRDS(limitsPath))
    #} else .
    #} %>%
    { if (!is.null(siteCoords)) {
      # grab the missing sites from that file
      # [ ] 
      dplyr::left_join(., readRDS(siteCoords), by = c("STATION_ID" = "Station"), suffix = c("", ".x")) %>%
        dplyr::mutate(
          LATITUDE = dplyr::coalesce(Latitude, LATITUDE),
          LONGITUDE = dplyr::coalesce(Longitude, LONGITUDE)
        ) %>%
        dplyr::select(-c(Latitude, Longitude))
    } else .
    } %>%
    # Impute site coordinates as the mean of that Site's recorded coordinates
    # Not removing this, just deprecating this argument
    { if (imputeCoordinates) {
      dplyr::mutate(.,
        LATITUDE = as.numeric(LATITUDE),
        LONGITUDE = as.numeric(LONGITUDE),
        LATITUDE = ifelse(is.na(LATITUDE), mean(LATITUDE, na.rm = T), LATITUDE),
        LONGITUDE = ifelse(is.na(LONGITUDE), mean(LONGITUDE, na.rm = T), LONGITUDE),
        STN_DEPTH_M = ifelse(is.na(STN_DEPTH_M), mean(STN_DEPTH_M, na.rm = T), STN_DEPTH_M),
        .by = STATION_ID
      ) 
    } else .
    } %>%
    dplyr::mutate(
      Study = "GLENDA",
      VALUE = dplyr::case_when(
        # Secchi estimates are treated differently (removed) than WChem estimates (not removed)
        # [ ] Could we impute with station depth here?
        # [ ] Is the secchi estimamte ever greater than station depth?
          # If so, then it's a clear to bottom issue, if not, then filter these all out
        # If so, make sure to add to the result_remark
        (grepl("secchi", ANALYTE, ignore.case = TRUE)) & (grepl("estimate", RESULT_REMARK, ignore.case = TRUE)) ~ NA,
        .default = VALUE
      
      RESULT_REMARK= dplyr::case_when(
        grepl("estimate", RESULT_REMARK, ignore.case =TRUE) ~ paste(RESULT_REMARK, NA, sep=";"),
        .default = RESULT_REMARK
      )
    ) %>%
    dplyr::mutate(RESULT = as.numeric(VALUE), Latitude = as.numeric(LATITUDE), Longitude = as.numeric(LONGITUDE)) %>%
    # These columns are all renamed and so no longer needed
    dplyr::select(-c(VALUE, LONGITUDE, LATITUDE)) %>%
    dplyr::rename(UID = SAMPLE_ID, sampleDepth = SAMPLE_DEPTH_M, stationDepth = STN_DEPTH_M, QAcomment = RESULT_REMARK) %>%
    dplyr::mutate(UID = as.character(UID), RESULT = as.numeric(RESULT)) %>%

    # Standardize analyte names
    dplyr::left_join(renamingTable, by = c("Study", "MEDIUM", "ANALYTE", "FRACTION", "METHOD"= "Methods")) %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    dplyr::mutate(
      ReportedUnits = tolower(ReportedUnits),
      ReportedUnits = stringr::str_remove_all(ReportedUnits, "/")
    ) %>%
    dplyr::left_join(key, by = "CodeName") %>% 
    # [ ] Check if we need to impute units
    # If so, we will assume on a given year analytes have same units
    dplyr::mutate(TargetUnits = tolower(TargetUnits)) %>%
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::filter(!grepl("remove", CodeName, ignore.case = T)) %>%
    dplyr::mutate(RESULT = dplyr::case_when(
      # convert from silicon to silica which has more mass
      ANALYTE == "Silicon, Elemental" ~ RESULT * 2.13918214,
      ANALYTE == "Silica, Dissolved as Si" ~ RESULT * 2.13918214,
      ReportedUnits == TargetUnits ~ RESULT,
      ReportedUnits != TargetUnits ~ RESULT * ConversionFactor,
      .default = RESULT
    ))

  return (df)
}

