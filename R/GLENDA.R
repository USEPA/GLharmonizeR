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
    # this line is only for saving test data
    { if (!is.null(sampleIDs)) {
      dplyr::filter(.,
        SAMPLE_ID %in% sampleIDs
      )}
      else . } %>%
    tidyr::pivot_longer(cols = -c(1:18),
                 names_to = c(".value", "Number"),
                 names_pattern = "(.*)_(\\d*)$") %>%
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
      RESULT_REMARK != "Known Contamination",

      # We aren't including air measurements (though we don't remove all
      # marked as air measurements, since some are secretly surface water
      # measurements
      !((MEDIUM == "air: ambient") & (ANALYTE == "Temperature"))
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
#' @param imputeCoordinages (optional) Boolean specifying whether to impute missing station coordinates 
#' @param siteCoords (optional) filepath to list of site coordinates to impute missing lats/lons
#' @param namingFile (optional) filepath to a file containing remappings for analyte names 
#'
#' @return a dataframe
.cleanGLENDA <- function(df, namingFile, flagsPath= NULL, imputeCoordinates = FALSE, siteCoords = NULL) {

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
    { if (!is.null(siteCoords)) {
      # impute the missing sites from that file
      dplyr::left_join(., readxl::read_xlsx(sitecoords, sheet =1 ), by = c("STATION_ID" = "STATION"), suffix = c("", ".x"),
                        .name_repair = "unique_quiet") %>%
        dplyr::mutate(
          Latitude = dplyr::coalesce(Latitude, LATITUDE),
          Longitude = dplyr::coalesce(Longitude, LONGITUDE),
          stationDepth = dplyr::coalesce(STN_DEPTH_M, `AVG_DEPTH, m`)
        ) %>%
        dplyr::select(-c(LATITUDE, LONGITUDE, `AVG_DEPTH, m`, STN_DEPTH_M))
    } else .
    } %>%
    # Impute site coordinates as the mean of that Site's recorded coordinates
    { if (imputeCoordinates) {
      dplyr::mutate(.,
        Latitude = ifelse(is.na(Latitude), mean(Latitude, na.rm = T), Latitude),
        Longitude = ifelse(is.na(Longitude), mean(Longitude, na.rm = T), Longitude),
        stationDepth = ifelse(is.na(stationDepth), mean(stationDepth, na.rm = T), stationDepth),
        .by = STATION_ID
      ) 
    } else .
    } %>%
    dplyr::mutate(
      Study = "GLENDA",
      VALUE = dplyr::case_when(
        # MAKE SURE THIS IS EXPLICITLY secchi
        (grepl("secchi", ANALYTE, ignore.case =TRUE)) & (grepl("estimate", RESULT_REMARK, ignore.case =TRUE)) ~ NA,
        .default = VALUE
      ),
      RESULT_REMARK= dplyr::case_when(
        grepl("estimate", RESULT_REMARK, ignore.case =TRUE) ~ paste(RESULT_REMARK, NA, sep=";"),
        .default = VALUE
      )
    ) %>%
    dplyr::mutate(RESULT = as.numeric(VALUE), Latitude = as.numeric(LATITUDE), Longitude = as.numeric(LONGITUDE)) %>%
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
    dplyr::mutate(TargetUnits = tolower(TargetUnits)) %>%
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::filter(!grepl("remove", CodeName, ignore.case = T)) %>%
    dplyr::mutate(RESULT = ifelse(
      ANALYTE == "Silicon, Elemental", RESULT / 2.13918214, RESULT
    ))

  return (df)
}
