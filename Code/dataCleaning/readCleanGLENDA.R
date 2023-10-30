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



#' Title
#'
#' @param filepath string specifying path to file
#'
#' @return a dataframe
#' @export
#'
#' @examples
readCleanGLENDA <- function(filepath) {
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
    # Select samples that haven't been combined
    filter(QC_TYPE == "routine field sample",
           SAMPLE_TYPE != "Composite") %>%
    # These columns are redundant with the "Analyte" columns
    select(-contains("ANL_CODE_")) %>%
    pivot_longer(cols = -c(1:18),
                 names_to = c(".value", "Number"),
                 names_pattern = "(.*)_(\\d*)$") %>%
    ##### NEED TO TAKE CARE OF
    #        "T", "NRR",  "INV", "W", "INVALID 89", "INVALID 88", "INVALID 88.5", "LAC", "INVALID 115",  "INVALID 8.4",
    #        "INVALID 11.645993", "INVALID 680", "INVALID 17.179", "INVALID 26.8039", "INVALID 4.1", "INVALID 42.9",
    #        "INVALID 6.5",
    #
    #        "<0.12", # Total/Bulk Chloride E300.0
    #        "<0.1", # Total/Bulk Calcium LG213
    #        "<500", # Filtrate Potassium E200.8
    #        "<2", # Total/Bulk Arsenic LG213
    #        "<1", # Total/Bulk Manganese LG213
    #        "<0.05" # Total/Bulk Potassium LG213
    ### TAKEN Care of by forcing strings to NA
    # na = c("", "NA", "no result reported", "No result reported.", "No result recorded.", "No Result Reported",
    #        "No Result Reported.", "NO RESULT", "no result", "No result reported",
    # convert no result reported strings into NAs
    mutate(VALUE = as.numeric((str_replace(VALUE, "[a-zA-Z]", "NA")))) %>%
    # Drop analyte number since it doesn't mean anything now
    select(-Number)
}
