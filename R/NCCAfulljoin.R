#' Read in all NCCA from 2000s, 2010, and 2015 including hydrogrpahic data
#' 
#' @description
#' `.LoadNCCAfull` returns water quality data along with spatial data from the 
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
#' @param Lakes List of Lakes to output
#' @return dataframe
#' @export
.LoadNCCAfull <- function(ncca2010sites, ncca2015sites, tenFiles, tenQAfile, fifteenFiles, 
                         NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015,
                         Lakes=c("Lake Michigan"), namingFile, nccaWQqaFile = nccaWQqaFile, n_max = Inf) {
# TODO Did we QC all of the great lakes already???
  sites <- .readNCCASites(ncca2010sites, ncca2015sites) %>%
    dplyr::distinct(SITE_ID, .keep_all = T)

  NCCAhydro <- .readNCCAhydro(NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015, n_max = n_max) %>%
    dplyr::mutate(UID = paste0("NCCA-hydro", "-", as.character(UID)))

  # Read NCCA Water chemistry files 
  nccaWQ <- .readNCCAchemistry(tenFiles, fifteenFiles, nccaWQqaFile = nccaWQqaFile, n_max = n_max) %>%
    dplyr::mutate(UID = as.character(UID)) %>%
    dplyr::mutate(Year = lubridate::year(sampleDate))

  
  renamingTable <- readxl::read_xlsx(namingFile, sheet= "NCCA_Map", na = c("", "NA"), .name_repair = "unique_quiet") 
  key <- readxl::read_xlsx(namingFile, sheet = "Key", .name_repair = "unique_quiet") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- readxl::read_xlsx(namingFile, sheet = "UnitConversions", .name_repair = "unique_quiet") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))
    
  final <- dplyr::bind_rows(NCCAhydro, nccaWQ) %>%
    dplyr::left_join(., sites, by = "SITE_ID") %>%
    # Cleaning up a column naming mistake
    # TODO Make sure these still work after the upstream changes
    dplyr::mutate(QAcode = dplyr::coalesce(QAcode, QACODE)) %>%
    dplyr::select(-QACODE) %>%
    # Add QA codes
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
    dplyr::filter(NCCRreg == "Great Lakes") %>%
    {if (!is.null(Lakes)) {
      dplyr::filter(., waterName %in% Lakes)
    } else .} %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    # rename
    dplyr::left_join(renamingTable, by = c("Study", "ANALYTE", "ANL_CODE", "METHOD" = "Methods"), na_matches="na") %>%
    # unit conversions
    dplyr::left_join(key, by = "CodeName") %>%
    # standardize units
    dplyr::mutate(
      ReportedUnits = tolower(ReportedUnits),
      ReportedUnits = stringr::str_remove(ReportedUnits, "/"),
      ReportedUnits = stringr::str_remove(ReportedUnits, "\\\\"),
      ReportedUnits = dplyr::case_when(
        # TODO can we make this more year specific Move this to 2015 hydro These were take from hdyro 2015 metadata file
        CodeName == "DO" ~ "mgl",
        CodeName == "Secchi" ~ "m",
        CodeName == "Temp" ~ "c",
        CodeName == "Cond" ~ "uscm",
        CodeName == "CPAR" ~ "%"
      )
    ) %>%
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::mutate(RESULT = dplyr::case_when(
      ReportedUnits == TargetUnits ~ RESULT,
      is.na(ReportedUnits) ~ RESULT,
      .default = RESULT  * ConversionFactor)) %>%
    dplyr::select(-Units) %>%
    dplyr::rename(Units = TargetUnits) %>%
    dplyr::filter(CodeName != "Remove")

  # Turn into test
  # final %>%
  #   filter(Units != ReportedUnits) %>%
  #   filter(is.na(ConversionFactor)) %>%
  #   count(ReportedUnits, Units, ConversionFactor)
  # test %>% 
  #   filter(is.na(CodeName)) %>%
  #   count(Study, ANALYTE, ANL_CODE, METHOD)
  return(final)
}


# list of measures without units
# - Secchi - assumed m
# - Temp  - assumed c
# - Cond  - assumed uscm
# - DO - assume  mgl
# - pH - no units is fine
# - CPAR - always %
