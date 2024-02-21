#' Load and join CSMI water quality data from 2010, 2015, and 2021.
#'
#' @description
#' `LoadCSMI` returns a dataframe of all of the joined water quality data relating to CSMI years 
#' 2010, 2015, and 2021.
#'
#' @details
#' This is the main functions users should use to load and assemble CSMI data
#' using this package. This function is also called in over arching functions 
#' to assemble data across multiple data sources.
#' @param directory2010 a string specifying the directory containing CSMI 2010 data 
#' @param database2015 a string specifying the filepath of the CSMI2015 access database
#' @param directory2021 a string specifying the directory containing the CSMI 2021 data 
#' @return dataframe of the fully joined water quality data from CSMI years 2010, 2015, 2021 
#' @export
LoadCSMI <- function(directory2010, database2015, database2021) {
  CSMI <- dplyr::bind_rows(
    .LoadCSMI2010(directory2010),
    .LoadCSMI2015(database2015),
    .LoadCSMI2021(database2021)
  ) %>%
    # Didn't see any useful information in these
    dplyr::select(-c(Notes, QA_CODE)) %>%
    dplyr::mutate(FRACTION = dplyr::case_when(
      FRACTION == "F" ~ "Filtrate",
      FRACTION == "U" ~ "Total/Bulk",
      FRACTION == "A" ~ "Filtrate",
      FRACTION == "M" ~ "Filtrate",
      FRACTION == "D" ~ "Filtrate",
      FRACTION == "V" ~ "Total/Bulk",
      FRACTION == "PCN" ~ "Residue",
      FRACTION == "Not applicable" ~ NA,
      .default = FRACTION
    )) %>%
    mutate(
      STUDY = "CSMI",
      ANALYTE = str_remove_all(ANALYTE, "\\+"),
      ANALYTE = str_remove_all(ANALYTE, "-"),
      ANALYTE = str_remove_all(ANALYTE, "="),
      Date = lubridate::date(Date)
      )
# CSMI fraction labels
# From "L:\Priv\Great lakes Coastal\2010 MED Lake Michigan\2010\LMich10forms.xls"
# Sheet "flow_charts"
}