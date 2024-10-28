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
.LoadCSMI <- function(csmi2010, csmi2015, csmi2021, namingFile, n_max = Inf) {
  # Load file to map analyte names to standard names
  dbi <- RODBC::odbcConnectAccess2007("CSMI2015_newQuery/CSMI2015_newQuery.accdb")
  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "CSMI_Map", na.strings = c("", "NA"))
  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))
  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  mdls2015 <- RODBC::sqlFetch(dbi, "Metadata_WQanalytes") %>%
    dplyr::select(ANALYTE = WQParam, ReportedUnits = WQUnits, mdl = DetectLimit, LAB = Analyst) %>%
    dplyr::mutate(
      Study = "CSMI_2015",
      ANALYTE = stringr::str_remove(ANALYTE, "_.*$"),
      ANALYTE = stringr::str_remove_all(ANALYTE, "[+-=]")
      ) %>%
    dplyr::left_join(renamingTable, by = c("Study", "ANALYTE")) %>%
    dplyr::filter(CodeName != "Remove") %>%
    dplyr::left_join(key, by = "CodeName") %>%
    dplyr::left_join(conversions) %>%
    dplyr::mutate(
      mdl = as.numeric(mdl),
      mdl = ifelse(!is.na(ConversionFactor), mdl*ConversionFactor, mdl),
      ) %>%
    dplyr::select(ANALYTE, CodeName, mdl) %>%
    dplyr::mutate(Study = "CSMI_2015")

  # Water chemistry  copied from
  # L:\Priv\Great lakes Coastal\2021 CSMI Lake Michigan\Data\Water chem
  # Contact is Annie Fosso
  mdls2021 <- file.path(csmi2021, "Chem2021_detection%20limits.xlsx") %>%
    # The detection limit file contains MDLs and the values used to impute results <MDL.
    openxlsx::read.xlsx(sheet = "detection limits", rows = 1:3) %>%
    dplyr::select(15:28) %>%
    dplyr::slice(1) %>%
    tidyr::pivot_longer(dplyr::everything(), values_to = "mdl", names_to = "ANALYTE") %>%
    dplyr::mutate(
      ANALYTE = stringr::str_extract(ANALYTE, "^[:alnum:]*"),
      ANALYTE = ifelse(ANALYTE == "chl", "Chla", ANALYTE)
      ) %>%
    dplyr::left_join(renamingTable) %>%
    dplyr::left_join(key) %>%
    dplyr::rename(ReportedUnits = Units) %>%
    dplyr::left_join(conversions) %>%
    dplyr::mutate(mdl = ifelse(!is.na(ConversionFactor), mdl * ConversionFactor, mdl)) %>%
    dplyr::select(ANALYTE, UNITS = TargetUnits, mdl) %>%
    distinct()



  CSMI <- dplyr::bind_rows(
    # We aren't including 2010 at this point
    # .LoadCSMI2010(csmi2010, n_max = n_max),
    # [x] 2015 has a lot of missing in VALUE column
    # This is just becuase the way the original data is stored
    .LoadCSMI2015(csmi2015, namingFile) %>% 
      dplyr::rename(ReportedUnits = UNITS) %>%
      dplyr::left_join(key) %>%
      dplyr::left_join(conversions) %>%
      dplyr::mutate(RESULT = ifelse(!is.na(ConversionFactor), RESULT * ConversionFactor, RESULT)) %>%
      dplyr::left_join(mdls2015),
    .LoadCSMI2021(csmi2021, namingFile) %>% 
      dplyr::rename(ReportedUnits = UNITS) %>%
      dplyr::left_join(key) %>%
      dplyr::left_join(conversions) %>%
      dplyr::mutate(RESULT = ifelse(!is.na(ConversionFactor), RESULT * ConversionFactor, RESULT)) %>%
      dplyr::left_join(mdls2021)
    ) %>%
    dplyr::filter(CodeName != "Remove") %>%
    dplyr::mutate(
      QAcomment = ifelse(RESULT < mdl, "MDL", NA), # mdls have already been converted to correct units
      QAcode = ifelse(RESULT < mdl, "MDL", NA), # mdls have already been converted to correct units
      RESULT = ifelse(RESULT < mdl, NA, RESULT) # mdls have already been converted to correct units
    ) %>%
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
      CodeName, LongName, Explicit_Units, mdl, QAcomment, QAcode, Units)
  file.remove("CSMI2015_newQuery/CSMI2015_newQuery.accdb")
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
