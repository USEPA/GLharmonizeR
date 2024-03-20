#' readPivotGLENDA
#'
#' @description 
#' A function to read the full GLENDA csv file and convert it to a more user friendly long format.
#' 
#' @details
#' `.readPivotGLENDA` This is a hidden function, this should be used for development purposes only, users will only call
<<<<<<< HEAD
#' this function implicitly when assembling their full water quality dataset
#' 
#'  
#' @param filepath a filepath to the GLENDA csv
#'
#' @return a dataframe
.readPivotGLENDA <- function(filepath) {
  readr::read_csv(filepath,
           col_types = readr::cols(YEAR = "i",
                            STN_DEPTH_M = "d",
                            LATITUDE = "d",
                            LONGITUDE = "d",
                            SAMPLE_DEPTH_M = "d",
                            # Skip useless or redundant columns
                            Row = "-",
                            .default = "c")) %>%
    # Convert daylight saving TZs into standard time TZs
    dplyr::mutate(TIME_ZONE= dplyr::case_when(
      TIME_ZONE == "EDT" ~ "America/Puerto_Rico",
      TIME_ZONE == "CDT" ~ "EST",
      .default = TIME_ZONE
    )) %>%
    tidyr::unite(sampleDate, SAMPLING_DATE, TIME_ZONE) %>%
    dplyr::mutate(sampleDate = readr::parse_datetime(sampleDate, format = "%Y/%m/%d %H:%M_%Z")) %>%
    tidyr::pivot_longer(cols = -c(1:18),
                 names_to = c(".value", "Number"),
                 names_pattern = "(.*)_(\\d*)$") %>%
    tidyr::drop_na(ANALYTE)
=======
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
.readPivotGLENDA <- function(filepath, n_max = Inf, sampleIDs = NULL) {
  filepath %>%
    { if (grepl(tools::file_ext(filepath), ".csv", ignore.case = TRUE)) {
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
    { if (grepl(tools::file_ext(filepath), ".Rds", ignore.case = TRUE)) {
      readRDS(.) %>%
        # This is so that everything can be pivoted, we change 
        # to final datatypes later once it's in long format
        dplyr::mutate(across(everything(), as.character)) %>%
        dplyr::mutate(YEAR = as.integer(YEAR),
              STN_DEPTH_M = as.double(STN_DEPTH_M),
              LATITUDE = as.double(LATITUDE),
              LONGITUDE = as.double(LONGITUDE),
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
>>>>>>> 38-tests-for-data-quality
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
#' @param nameMap (optional) filepath to a file containing remappings for analyte names 
#'
#' @return a dataframe
.cleanGLENDA <- function(df, flagsPath= NULL, imputeCoordinates = FALSE, siteCoords = NULL, nameMap= NULL) {

  df %>%
<<<<<<< HEAD
    # Select samples that haven't been combined
    dplyr::filter(SAMPLE_TYPE %in% c("Individual", "INSITU_MEAS"),
           QC_TYPE == "routine field sample",
           ) %>%
    # Drop analyte number since it doesn't mean anything now
    # These columns are redundant with the "Analyte" columns
    dplyr::select(-Number) %>%
    tidyr::unite(ANL_CODE2, ANL_CODE, FRACTION, sep = "_", remove = F) %>%
    # If value and remarks are missing, we assume sample was never taken
    dplyr::filter(
      !is.na(VALUE) | !is.na(RESULT_REMARK),
      # The only QA Codes worth removing "Invalid" and "Known Contamination".
      # The rest already passed an initial QA screening before being entered
      !grepl("Invalid", RESULT_REMARK, ignore.case= T),
      RESULT_REMARK != "Known Contamination",

      # We aren't including air measurements (though we don't remove all
      # marked as air measurements, since some are secretly surface water
      # measurements
      !((MEDIUM == "air: ambient") & (ANALYTE == "Temperature"))
    ) %>%
=======
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
>>>>>>> 38-tests-for-data-quality
    # Adding verbose remark descriptions is purely optional
    {if (!is.null(flagsPath)) {
      dplyr::left_join(., readxl::read_xlsx(flagsPath), by = c("RESULT_REMARK" = "NAME"))
      } else .
    } %>%
    { if (!is.null(siteCoords)) {
      # impute the missing sites from that file
      dplyr::left_join(., readxl::read_xlsx(sitecoords, sheet =1 ), by = c("STATION_ID" = "STATION"), suffix = c("", ".x")) %>%
        dplyr::mutate(
          LATITUDE = dplyr::coalesce(LATITUDE, LATITUDE.x),
          LONGITUDE = dplyr::coalesce(LONGITUDE, LONGITUDE.x),
          STN_DEPTH_M = dplyr::coalesce(STN_DEPTH_M, `AVG_DEPTH, m`)
        ) %>%
        dplyr::select(-c(LATITUDE.x, LONGITUDE.x, `AVG_DEPTH, m`))
    } else .
    } %>%
    # Impute site coordinates as the mean of that Site's recorded coordinates
    { if (imputeCoordinates) {
      dplyr::mutate(.,
        LATITUDE = ifelse(is.na(LATITUDE), mean(LATITUDE, na.rm = T), LATITUDE),
        LONGITUDE = ifelse(is.na(LONGITUDE), mean(LONGITUDE, na.rm = T), LONGITUDE),
        STN_DEPTH_M = ifelse(is.na(STN_DEPTH_M), mean(STN_DEPTH_M, na.rm = T), STN_DEPTH_M),
        .by = STATION_ID
      ) 
    } else .
    } %>%
    { if (!is.null(nameMap))  {
      # Assume name map will always be in the GLENDA_MAP sheet
      dplyr::left_join(., readxl::read_xlsx(nameMap, sheet = "GLENDA_Map"), by = "ANALYTE")
<<<<<<< HEAD
    } else . 
    }
=======
    } else .
    } %>%
    mutate(
      STUDY = "GLENDA",
      VALUE = dplyr::case_when(
        # MAKE SURE THIS IS EXPLICITLY secchi
        (grepl("secchi", ANALYTE, ignore.case =TRUE)) & (grepl("estimate", RESULT_REMARK, ignore.case =TRUE)) ~ NA,
        .default = VALUE
      ),
      RESULT_REMARK= dplyr::case_when(
        grepl("estimate", RESULT_REMARK, ignore.case =TRUE) ~ paste(RESULT_REMARK, NA, sep=";"),
        .default = VALUE
      )
    )
>>>>>>> 38-tests-for-data-quality
}


#' readCleanGLENDA 
#'
#' A function to read and clean the full GLENDA csv file. This is generally the
#' function users will interact with, the comprising read and clean functions 
#' are moreso for development purposes.
#' 
#' @param filepath a filepath to the GLENDA csv
#'
#' @return a dataframe
#' @export
<<<<<<< HEAD
readCleanGLENDA <- function(filepath, flagsPath = NULL, siteCoords = NULL, imputeCoordinates= FALSE, nameMap = NULL) {
  .cleanGLENDA(.readPivotGLENDA(filepath), flagsPath = flagsPath, imputeCoordinates = imputeCoordinates, nameMap = nameMap) 
=======
readCleanGLENDA <- function(filepath, flagsPath = NULL, siteCoords = NULL, imputeCoordinates= FALSE, nameMap = NULL, n_max = Inf, sampleIDs = NULL) {
  .cleanGLENDA(.readPivotGLENDA(filepath, n_max = n_max, sampleIDs = sampleIDs), flagsPath = flagsPath, imputeCoordinates = imputeCoordinates, nameMap = nameMap) 
>>>>>>> 38-tests-for-data-quality
}
# Useful links
# Water chemistry descriptions
# https://www.epa.gov/great-lakes-monitoring/great-lakes-water-quality-monitoring-program-0#chemistry
