library(tidyverse)
library(janitor)
# Other filter ideas
# FIX TIME ZONES
# Depth Code 'thermocline


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
           na = c("", "NA", "no result reported", "No result reported.", "No result recorded.", "No Result Reported",
                  "No Result Reported.", "NO RESULT", "no result", "No result reported",

                  "T", "NRR",  "INV", "W", "INVALID 89", "INVALID 88", "INVALID 88.5", "LAC", "INVALID 115",  "INVALID 8.4",
                  "INVALID 11.645993", "INVALID 680", "INVALID 17.179", "INVALID 26.8039", "INVALID 4.1", "INVALID 42.9",
                  "INVALID 6.5",

                  "<0.12", "<0.1", "<500", "<2", "<1", "<0.05")) %>%
    # These columns are redundant with the "Analyte" columns
    select(-contains("ANL_CODE_"), -Row) %>%
    # Drop columns with 100% missingness
    #select_if(~mean(is.na(.)) < 0.9) %>%
    pivot_longer(cols = -c(1:18),
                 names_to = c(".value", "Number"),
                 names_pattern = "(.*)_(\\d*)$") %>%
    filter(!is.na(ANALYTE),
           QC_TYPE == "routine field sample",
           SAMPLE_TYPE != "Composite") %>%
    mutate(SAMPLING_DATE = parse_datetime(SAMPLING_DATE, "%Y/%m/%d %H:%M"))

}
