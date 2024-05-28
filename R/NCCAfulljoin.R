#' Read in all NCCA from 2000s, 2010, and 2015 including hydrogrpahic data
#' 
#' @description
#' `.LoadNCCAfull` returns water quality data along with spatial data from the 
#'  site information measured through NCCA study in the early 2000s as well as in 2010, and 2015
#' 
#' @details
#' The spatial information for sites is read in using the .readSites helper functions, this is then
#' joined to the water quality and hydrographic data and ultimately output as a data table.
#' @param NCCAsites2010 filepath to site files
#' @param NCCAsites2015 filepath to site files
#' @param NCCAwq2010 filepath to 2010's data
#' @param NCCAwq2015 filepaht to 2015 data
#' @param NCCAhydrofiles2010 filepath to hydrogrpahic 2010 data
#' @param NCCAhydrofile2015 filepath to hydrogarphic 2015 data
#' @param NCCAjsecchifile2015  filepaht to secchi 2015 data
#' @param Lakes List of Lakes to output
#' @param namingFile filepath to Analytes3.xlsx which conatains names and conversions
#' @param n_max integer specifying how many lines to read of each file to save time for testing
#' @return dataframe
#' @export
.LoadNCCAfull <- function(NCCAsites2010, NCCAsites2015, NCCAwq2010, NCCAwq2015,
                         NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015,
                         Lakes=c("Lake Michigan"), namingFile, NCCAwqQA, n_max = Inf) {
  # [ ] Did we QC all of the great lakes already???
  sites <- .readNCCASites(NCCAsites2010, NCCAsites2015) %>%
    dplyr::distinct(SITE_ID, .keep_all = T)

  NCCAhydro <- .readNCCAhydro(NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015, 
    NCCAwqQA = NCCAwqQA, n_max = n_max) %>%
    dplyr::mutate(UID = paste0("NCCA_hydro", "-", as.character(UID)))

  # Read NCCA Water chemistry files 
  nccaWQ <- .readNCCAchemistry(NCCAwq2010, NCCAwq2015, nccaWQqaFile = NCCAwqQA, n_max = n_max) %>%
  # XXX This might break when NCCA updates their data with new UID's
    dplyr::mutate(UID = paste0(Study, "-", as.character(UID))) %>%
    dplyr::mutate(Year = lubridate::year(sampleDateTime))


  renamingTable <- readxl::read_xlsx(namingFile, sheet= "NCCA_Map", na = c("", "NA"), .name_repair = "unique_quiet") 
  key <- readxl::read_xlsx(namingFile, sheet = "Key", .name_repair = "unique_quiet") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- readxl::read_xlsx(namingFile, sheet = "UnitConversions", .name_repair = "unique_quiet") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))
    
  final <- dplyr::bind_rows(NCCAhydro, nccaWQ) %>%
    dplyr::left_join(., sites, by = "SITE_ID") %>%
    # Cleaning up a column naming mistake
    # [ ] Make sure these still work after the upstream changes
    dplyr::mutate(QAcode = dplyr::coalesce(QAcode, QACODE)) %>%
    dplyr::select(-QACODE) %>%
    dplyr::mutate(
      stationDepth = dplyr::coalesce(stationDepth.y, stationDepth.x),
      sampleDateTime = dplyr::coalesce(sampleDateTime, DATE_COL),
      Year = lubridate::year(sampleDateTime),
      # [x] store the originally reported Units
      OriginalUnits = UNITS
      ) %>%
    dplyr::select(-c(
      stationDepth.x, stationDepth.y,
      DATE_COL #QAconsiderations,
    )) %>%
    # Great lakes get's priority over spcifying each lake
    dplyr::filter(NCCRreg == "Great Lakes") %>%
    {if (!is.null(Lakes)) {
      dplyr::filter(., WTBDY_NM %in% Lakes)
    } else .} %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    # [x] fill missing units as the most common non-missing units within a given study-year
    # Group by study-year, analytes then 
    dplyr::mutate(
      ReportedUnits = ifelse(
        is.na(ReportedUnits),
        # impute with hte mode
        (data.frame(table(ReportedUnits)) %>% 
          tidyr::drop_na() %>% 
          dplyr::arrange(dplyr::desc(Freq)))[[1]],
        ReportedUnits
      ),
      QAcode = ifelse(is.na(ReportedUnits),
        paste(QAcode, ";", "No reported units, so assumed same as most common units for this given analyte on this year"),
        QAcode),
      .by = c(Study, Year, ANALYTE)
    ) %>%
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
        # [x] can we make this more year specific
        # These were take from hdyro 2015 metadata file
        (Year == 2015) & (CodeName == "DO") ~ "mgl",
        (Year == 2015) & (CodeName == "Secchi") ~ "m",
        (Year == 2015) & (CodeName == "Temp") ~ "c",
        (Year == 2015) & (CodeName == "Cond") ~ "uscm",
        (Year == 2015) & (CodeName == "CPAR") ~ "%"
      )
    ) %>%
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::mutate(RESULT = dplyr::case_when(
      ReportedUnits == TargetUnits ~ RESULT,
      is.na(ReportedUnits) ~ RESULT,
      .default = RESULT  * ConversionFactor),
    ) %>%
    dplyr::select(-Units) %>%
    dplyr::rename(Units = TargetUnits) %>%
    dplyr::filter(CodeName != "Remove")
  # Turn into test
  # final %>%
  #   filter(TargetUnits != ReportedUnits) %>%
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
