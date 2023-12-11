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
}



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
    filter(!is.na(VALUE) | !is.na(RESULT_REMARK)) %>%
    # Labeling REMARK RISK
    mutate(REMARK_RISK = case_when(
      # replace low first, becuase in cases where multiple strings are matched, we want to give precedent to
      # the higher risk flags
      grepl("limit", RESULT_REMARK, ignore.case= T) ~ "LOW",
      grepl("Correction", RESULT_REMARK, ignore.case= T) ~ "LOW",
      grepl("Estimated", RESULT_REMARK, ignore.case= T) ~ "LOW",
      grepl("incomplete", RESULT_REMARK, ignore.case= T) ~ "MEDIUM",
      grepl("Holding time", RESULT_REMARK, ignore.case= T) ~ "MEDIUM",
      grepl("outlier", RESULT_REMARK, ignore.case= T) ~ "MEDIUM",
      grepl("fail", RESULT_REMARK, ignore.case= T) ~ "HIGH",
      grepl("No result", RESULT_REMARK, ignore.case= T) ~ "HIGH",
      grepl("Anomaly", RESULT_REMARK, ignore.case= T) ~ "HIGH",
      grepl("Contamination", RESULT_REMARK, ignore.case= T) ~ "HIGH",
      grepl("Inconsistent", RESULT_REMARK, ignore.case= T) ~ "HIGH",
      grepl("Invalid", RESULT_REMARK, ignore.case= T) ~ "HIGH"
    )) %>%

    # Matching units
    mutate(VALUE = as.numeric(VALUE),
      VALUE2 = case_when((UNITS == "ug/l" & ANALYTE == "Magnesium") ~ VALUE / 1000,
              (UNITS == "ug/l" & ANALYTE == "Sodium") ~ VALUE / 1000,
                             .default = VALUE),
           UNITS2 = case_when(ANALYTE == "Magnesium" ~ "mg/l",
                             ANALYTE == "Sodium" ~ "mg/l",
                             ANALYTE =="Alkalinity, Total as CaCO3" ~ "mg/l",
                             ANALYTE == "Conductivity" ~ "umho/cm",
                             ANALYTE == "Manganese"  ~ "ug/l",
                             ANALYTE == "Secchi Disc Transparency" ~ "m",
                             # FTU and NTU are equivalent
                             ANALYTE == "Turbidity" ~ "FTU",
                             .default = UNITS) )
}


readCleanGLENDA <- function(filepath) {
  cleanGLENDA(readPivotGLENDA(filepath)) 
}
