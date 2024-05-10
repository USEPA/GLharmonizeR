# This folder is convenient for old files since R will not build from subdirectories.

# DONE Move this old function to a different location
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
.readNCCA2000s <- function(filepath, n_max = Inf) {
  readr::read_csv(filepath,
                  n_max = n_max,
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
    tidyr::pivot_longer(-c(SITE_ID, Year), names_to = "ANALYTE", values_to = "RESULT")  %>%
    dplyr::mutate(
      sampleDepth = 0.5,
      Study = "NCCA_WQ_2000s"
    )
    # Make a missing columns for depths to align with other data sources
    #mutate(DEPTH = NA)
}