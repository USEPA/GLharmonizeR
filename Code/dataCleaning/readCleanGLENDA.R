library(tidyverse)
library(janitor)

# Useful links
# Water chemistry descriptions
# https://www.epa.gov/great-lakes-monitoring/great-lakes-water-quality-monitoring-program-0#chemistry

# Other filter ideas
# FIX TIME ZONES

# head(sort(table(df$VALUE_1), decreasing=T), 200)
# This is where I determined the NA values
#Clear_NAs <- df$VALUE_9[!is.na(df$VALUE_9)]
#unique(Clear_NAs[is.na(as.numeric(Clear_NAs))])
#df %>%
#  select(contains("VALUE")) %>%
#  glimpse()
# df %>%
#   select(YEAR, STN_DEPTH_M, LATITUDE, LONGITUDE, SAMPLE_DEPTH_M, contains("VALUE_")) %>%
#   glimpse()


readPivotGLENDA <- function(filepath) {
  read_csv(filepath,
           col_types = cols(YEAR = "i",
                            STN_DEPTH_M = "d",
                            LATITUDE = "d",
                            LONGITUDE = "d",
                            SAMPLING_DATE = col_date(format = "%Y/%m/%d %H:%M"),
                            SAMPLE_DEPTH_M = "d",
                            # Skip useless or redundant columns
                            Row = "-",
                            .default = "c")) %>%
    pivot_longer(cols = -c(1:18),
                 names_to = c(".value", "Number"),
                 names_pattern = "(.*)_(\\d*)$") %>%
    drop_na(ANALYTE)
}


#' Title
#'
#' @param filepath GLENDA dataframe in long format
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
    mutate(FRACTION = ifelse(FRACTION == "Not applicable", "", FRACTION)) %>%
    unite(ANL_CODE, ANL_CODE, FRACTION, sep = "_", remove = T) %>%
    # If value and remarks are missing, we assume sample was never taken
    filter(!is.na(VALUE) | !is.na(RESULT_REMARK)) %>%
    #mutate(VALUE = as.numeric((str_replace(VALUE, "[a-zA-Z]", "NA")))) %>%
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
      VALUE = case_when((UNITS == "ug/l" & ANALYTE == "Magnesium") ~ VALUE / 1000,
              (UNITS == "ug/l" & ANALYTE == "Sodium") ~ VALUE / 1000,
                             .default = VALUE),
           UNITS = case_when(ANALYTE == "Magnesium" ~ "mg/l",
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
