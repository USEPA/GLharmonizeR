#' Read in all NCCA from 2000s, 2010, and 2015 including hydrogrpahic data
#' 
#' @description
#' `LoadNCCAfull` returns water quality data along with spatial data from the 
#'  site information measured through NCCA study in the early 2000s as well as in 2010, and 2015
#' 
#' @details
#' The spatial information for sites is read in using the .readSites helper functions, this is then
#' joined to the water quality and hydrographic data and ultimately output as a data table.
#' @param siteFiles filepath to site files
#' @param tenFiles filepath to 2010's data
#' @param fifteenFiles filepaht to 2015 data
#' @param NCCAhydrofiles2010 filepath to hydrogrpahic 2010 data
#' @param NCCAhydrofile2015 filepath to hydrogarphic 2015 data
#' @param NCCAjsecchifile2015  filepaht to secchi 2015 data
#' @return dataframe
#' @export
LoadNCCAfull <- function(ncca2010sites, ncca2015sites, tenFiles, tenQAfile, fifteenFiles, 
                         NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015,
                         greatLakes=TRUE, Lakes=c("Lake Michigan"), namingFile) {

  sites <- .readNCCASites(ncca2010sites, ncca2015sites) %>%
    dplyr::distinct(SITE_ID, .keep_all = T)

  NCCAhydro <- .readNCCAhydro(NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015) %>%
    dplyr::mutate(UID = as.character(UID))

  # Read NCCA WQ files 
  nccaWQ <- .readNCCA(tenFiles, fifteenFiles) %>%
    dplyr::mutate(UID = as.character(UID)) %>%
    dplyr::mutate(Year = lubridate::year(sampleDate)) #%>%

  QA <- readr::read_csv(tenQAfile) %>%
    dplyr::select(-`...3`) %>%
    dplyr::rename(QAconsiderations = Considerations,
                  QAcode = `Unique Qualifier Code`) %>%
    dplyr::distinct(QAcode, .keep_all = TRUE)
    
  final <- dplyr::bind_rows(NCCAhydro, nccaWQ) %>%
    dplyr::left_join(., sites, by = "SITE_ID") %>%
    dplyr::mutate(QAcode = dplyr::coalesce(QAcode, QACODE)) %>%
    dplyr::select(-QACODE) %>%
    dplyr::left_join(., QA, by = "QAcode") %>%
    dplyr::mutate(
      stationDepth = dplyr::coalesce(stationDepth.y, stationDepth.x, Depth),
      sampleDate = dplyr::coalesce(sampleDate, DATE_COL),
      Year = lubridate::year(sampleDate)
      ) %>%
    dplyr::select(-c(
      stationDepth.x, stationDepth.y, Depth,
      DATE_COL #QAconsiderations,
    )) %>%
    # Great lakes get's priority over spcifying each lake
    {if (greatLakes) {
      dplyr::filter(., NCCRreg == "Great Lakes")
    } else if (!is.null(Lakes)) {
      dplyr::filter(., waterName %in% Lakes)
    } else .} %>%
    dplyr::select(-Units)

  renamingTable <- readxl::read_xlsx(namingFile, sheet= "NCCA_Map", na = c("", "NA")) 
  key <- readxl::read_xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- readxl::read_xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))

  final <- final %>%
  # rename
    dplyr::left_join(renamingTable, by = c("Study", "ANALYTE", "ANL_CODE", "METHOD" = "Methods"), na_matches="na") %>%
    dplyr::rename(ReportedUnits = Units) %>%
    # unit conversions
    dplyr::left_join(key, by = "CodeName") %>%
    # standardize units
    dplyr::mutate(
      ReportedUnits = tolower(ReportedUnits),
      ReportedUnits = stringr::str_remove(ReportedUnits, "/"),
      ReportedUnits = stringr::str_remove(ReportedUnits, "\\\\"),
    ) %>%
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::mutate(RESULT = ifelse(ReportedUnits == TargetUnits, RESULT,
                    RESULT  * ConversionFactor)) %>%
    dplyr::rename(Units = TargetUnits)

  # Turn into test
  # test %>%
  #   filter(! TargetUnits == ReportedUnits) %>%
  #   filter(is.na(ConversionFactor)) %>%
  #   count(ReportedUnits, TargetUnits, ConversionFactor)
  # test %>% 
  #   filter(is.na(CodeName)) %>%
  #   count(Study, ANALYTE, ANL_CODE, METHOD)
  return(final)
}
