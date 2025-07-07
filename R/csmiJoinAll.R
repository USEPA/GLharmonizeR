#' Load and join CSMI water quality data from 2015 and 2021.
#'
#' @description
#' `LoadCSMI` returns a dataframe of all of the joined water quality data relating to CSMI years
#' 2015 and 2021.
#'
#' @details
#' This is the main functions users should use to load and assemble CSMI data
#' using this package. This function is also called in over arching functions
#' to assemble data across multiple data sources.
#' @param csmi2015 a string specifying the URL for CSMI 2015 zipped database
#' @param csmi2021 a string specifying the directory containing the CSMI 2021 data
#' @param namingFile filepath to a file containing mappings for analyte names#'
#' @param n_max Number of rows to read in from the data file (this is just for testing purposes)
#' @return dataframe of the fully joined water quality data from CSMI years 2015 and 2021
.loadCSMI <- function(csmi2015, csmi2021, namingFile, n_max = Inf) {
  # Water chemistry  copied from
  # L:\Priv\Great lakes Coastal\2021 CSMI Lake Michigan\Data\Water chem
  # Contact was Annie Fosso
  CSMI <- dplyr::bind_rows(
      # We aren't including 2010 at this point
      # .readCleanCSMI2010(csmi2010, n_max = n_max),
      .readCleanCSMI2015(csmi2015, namingFile),
      .readCleanCSMI2021(csmi2021, namingFile)
    ) %>%
    dplyr::mutate(
      UID = as.character(UID),
      STIS = as.character(STIS),
      `STIS#` = as.character(`STIS#`),
      UID = dplyr::coalesce(UID, STIS, `STIS#`),
      UID = paste0("CSMI-", UID),
      Years = as.character(Years)
    ) %>%
    dplyr::select(-ANL_CODE, -SiteMaxCTDdepth, -STIS, -`STIS#`, -targetDepth, -targetLat, -targetLon, -SampleEventKey)

    return(CSMI)
}


