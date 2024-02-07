library(tidyverse)
library(janitor)

# Useful links
# Water chemistry descriptions
# https://www.epa.gov/great-lakes-monitoring/great-lakes-water-quality-monitoring-program-0#chemistry


#' readPivotGLENDA
#'
#' A function to read the full GLENDA csv file and convert it to a more user friendly long format.
#'  
#' @param filepath a filepath to the GLENDA csv
#'
#' @return a dataframe
readPivotGLENDA <- function(filepath) {
  read_csv(filepath,
           col_types = cols(YEAR = "i",
                            STN_DEPTH_M = "d",
                            LATITUDE = "d",
                            LONGITUDE = "d",
                            SAMPLE_DEPTH_M = "d",
                            # Skip useless or redundant columns
                            Row = "-",
                            .default = "c")) %>%
    # Convert daylight saving TZs into standard time TZs
    mutate(TIME_ZONE= case_when(
      TIME_ZONE == "EDT" ~ "America/Puerto_Rico",
      TIME_ZONE == "CDT" ~ "EST",
      .default = TIME_ZONE
    )) %>%
    unite(sampleDate, SAMPLING_DATE, TIME_ZONE) %>%
    mutate(sampleDate = parse_datetime(sampleDate, format = "%Y/%m/%d %H:%M_%Z")) %>%
    pivot_longer(cols = -c(1:18),
                 names_to = c(".value", "Number"),
                 names_pattern = "(.*)_(\\d*)$") %>%
    drop_na(ANALYTE)
}


#' cleanGLENDA
#'
#' @param df GLENDA dataframe in long format
#' @param flagsPath (optional) filepath to the Result remarks descriptions. Default is NULL.
#' @param imputeCoordinages (optional) Boolean specifying whether to impute missing station coordinates 
#' @param siteCoords (optional) filepath to list of site coordinates to impute missing lats/lons
#' @param nameMap (optional) filepath to a file containing remappings for analyte names 
#'
#' @return a dataframe
cleanGLENDA <- function(df, flagsPath= NULL, imputeCoordinates = FALSE, siteCoords = NULL, nameMap= NULL) {

  df %>%
    # Select samples that haven't been combined
    filter(SAMPLE_TYPE %in% c("Individual", "INSITU_MEAS"),
           QC_TYPE == "routine field sample",
           ) %>%
    # Drop analyte number since it doesn't mean anything now
    # These columns are redundant with the "Analyte" columns
    select(, -Number) %>%
    unite(ANL_CODE2, ANL_CODE, FRACTION, sep = "_", remove = F) %>%
    # If value and remarks are missing, we assume sample was never taken
    filter(
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
    # Adding verbose remark descriptions is purely optional
    {if (!is.null(flagsPath)) {
      left_join(., readxl::read_xlsx(flagsPath), by = c("RESULT_REMARK" = "NAME"))
      } else .
    } %>%
    { if (!is.null(siteCoords)) {
      # impute the missing sites from that file
      left_join(., readxl::read_xlsx(sitecoords, sheet =1 ), by = c("STATION_ID" = "STATION"), suffix = c("", ".x")) %>%
        mutate(
          LATITUDE = coalesce(LATITUDE, LATITUDE.x),
          LONGITUDE = coalesce(LONGITUDE, LONGITUDE.x),
          STN_DEPTH_M = coalesce(STN_DEPTH_M, `AVG_DEPTH, m`)
        ) %>%
        select(-c(LATITUDE.x, LONGITUDE.x, `AVG_DEPTH, m`))
    } else .
    } %>%
    # Impute site coordinates as the mean of that Site's recorded coordinates
    { if (imputeCoordinates) {
      mutate(.,
        LATITUDE = ifelse(is.na(LATITUDE), mean(LATITUDE, na.rm = T), LATITUDE),
        LONGITUDE = ifelse(is.na(LONGITUDE), mean(LONGITUDE, na.rm = T), LONGITUDE),
        STN_DEPTH_M = ifelse(is.na(STN_DEPTH_M), mean(STN_DEPTH_M, na.rm = T), STN_DEPTH_M),
        .by = STATION_ID
      ) 
    } else .
    } %>%
    { if (!is.null(nameMap))  {
      # Assume name map will always be in the GLENDA_MAP sheet
      left_join(., readxl::read_xlsx(nameMap, sheet = "GLENDA_Map"), by = "ANALYTE")
    } else . 
    }
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
#'
#' @examples
readCleanGLENDA <- function(filepath, flagsPath = NULL, siteCoords = NULL, imputeCoordinates= FALSE, nameMap = NULL) {
  cleanGLENDA(readPivotGLENDA(filepath), flagsPath = flagsPath, imputeCoordinates = imputeCoordinates, nameMap = nameMap) 
}
