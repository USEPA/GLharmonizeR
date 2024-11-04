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
#' @param csmi2010 a string specifying the directory containing CSMI 2010 data
#' @param csmi2021 a string specifying the directory containing the CSMI 2021 data
#' @return dataframe of the fully joined water quality data from CSMI years 2010, 2015, 2021
.loadCSMI <- function(csmi2010, csmi2015, csmi2021, namingFile, n_max = Inf) {
  # Water chemistry  copied from
  # L:\Priv\Great lakes Coastal\2021 CSMI Lake Michigan\Data\Water chem
  # Contact is Annie Fosso
  CSMI <- dplyr::bind_rows(
      # We aren't including 2010 at this point
      # .readCleanCSMI2010(csmi2010, n_max = n_max),
      .readCleanCSMI2015(csmi2015, namingFile),
      .readCleanCSMI2021(csmi2021, namingFile)
    ) %>%
    dplyr::filter(CodeName != "Remove") %>%
    # dplyr::mutate(
    #   QAcomment = ifelse(RESULT < mdl, "MDL", NA), # mdls have already been converted to correct units
    #   QAcode = ifelse(RESULT < mdl, "MDL", NA), # mdls have already been converted to correct units
    #   RESULT = ifelse(RESULT < mdl, NA, RESULT) # mdls have already been converted to correct units
    # ) %>%
    dplyr::mutate(Units = TargetUnits) %>%
    dplyr::mutate(
      UID = as.character(UID),
      STIS = as.character(STIS),
      `STIS#` = as.character(`STIS#`),
      UID = dplyr::coalesce(UID, STIS, `STIS#`),
      UID = paste0("CSMI-", UID)
    ) %>%
    dplyr::select(
      UID,
      Study, sampleDepth, SITE_ID, Longitude, Latitude, stationDepth, sampleDateTime, Lake,
      CodeName, LongName, Explicit_Units, mdl, QAcomment, Units, RESULT) %>%
    dplyr::filter(!grepl("remove", CodeName, ignore.case=T))
  return(CSMI)
}
# Turn into test
# test %>%
#   filter(! TargetUnits == ReportedUnits) %>%
#   filter(is.na(ConversionFactor)) %>%
#   count(ReportedUnits, TargetUnits, ConversionFactor)

# CSMI 2010 fraction labels
# From "L:\Priv\Great lakes Coastal\2010 MED Lake Michigan\2010\LMich10forms.xls"
# Sheet "flow_charts"
