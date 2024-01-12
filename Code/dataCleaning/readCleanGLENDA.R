library(tidyverse)
library(janitor)

# Useful links
# Water chemistry descriptions
# https://www.epa.gov/great-lakes-monitoring/great-lakes-water-quality-monitoring-program-0#chemistry

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
    # Matching column names to LAGOS
}
# "","SITE_ID","LAT_DD","LON_DD","STATE","CNTYNAME","valueid","obs_id","lagoslakeid","sampledate","lagos_variableid","lagos_variablename","datavalue","datavalue_unit","detectionlimit_value","datavalue_conversion","detectionlimitvalue_conversion","lagos_comments","lagos_sampledepth","lagos_sampleposition","lagos_sampletype","organization_id","organization_name","source_activityid","source_comments","source_detectionlimit_value","source_labmethoddescription","source_labmethodid","source_labmethodname","source_parameter","source_sampledepth","source_sampleposition","source_samplesiteid","source_sampletype","source_unit","source_value","source_methodqualifier"



#' Title
#'
#' @param dataframe GLENDA dataframe in long format
#'
#' @return a dataframe
#' @export
#'
#' @examples
cleanGLENDA <- function(df) {
  df %>%
    # Select samples that haven't been combined
    # This is where we could add option to select for
    # composite if wanted
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
    )
}


readCleanGLENDA <- function(filepath) {
  cleanGLENDA(readPivotGLENDA(filepath)) 
}
