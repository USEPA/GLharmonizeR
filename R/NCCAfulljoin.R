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
.LoadNCCAfull <- function(NCCAsites2010, NCCAsites2015, NCCAwq2010, NCCAwq2015,
                          NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015,
                          Lakes = c("Lake Michigan"), n_max = Inf) {
  # [ ] Did we QC all of the great lakes already???
  sites <- .readNCCASites(NCCAsites2010, NCCAsites2015) %>%
    dplyr::distinct(SITE_ID, .keep_all = T)

  NCCAhydro <- .readNCCAhydro(NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015,
    n_max = n_max
  ) %>%
    dplyr::mutate(UID = paste0("NCCA_hydro", "-", as.character(UID)))

  # Read NCCA Water chemistry files
  # [x] Make the wqQA argument name consistent over all levels
  nccaWQ <- dplyr::bind_rows(
    .readNCCA2010(NCCAwq2010, n_max = n_max),
    .readNCCA2015(NCCAwq2015, n_max = n_max)
  ) %>%
    # QC filters
    # filter(! QACODE %in% c("J01", "Q08", "ND", "Q", "H", "L"))
    dplyr::mutate(
      SAMPYEAR = lubridate::year(sampleDateTime) # XXX This might break when NCCA updates their data with new UID's
    ) %>%
    dplyr::mutate(UID = paste0(Study, "-", as.character(UID))) %>%
    dplyr::mutate(Year = lubridate::year(sampleDateTime))



  final <- dplyr::bind_rows(NCCAhydro, nccaWQ) %>%
    dplyr::left_join(., sites, by = "SITE_ID") %>%
    # Cleaning up a column naming mistake
    # [x] Make sure these still work after the upstream changes
    dplyr::mutate(
      stationDepth = dplyr::coalesce(stationDepth.y, stationDepth.x),
      sampleDateTime = dplyr::coalesce(sampleDateTime, DATE_COL),
      Year = lubridate::year(sampleDateTime),
      # [x] store the originally reported Units
      OriginalUnits = UNITS
    ) %>%
    dplyr::select(-c(
      stationDepth.x, stationDepth.y,
      DATE_COL # QAconsiderations,
    )) %>%
    # [ ] Fix filter for region and lake
    # Great lakes get's priority over spcifying each lake
    dplyr::filter(NCCRreg == "Great Lakes") %>%
    {
     if (!is.null(Lakes)) {
       dplyr::filter(., WTBDY_NM %in% Lakes)
     } else {
       .
     }
    } %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    # [x] fill missing units as the most common non-missing units within a given study-year
    # Group by study-year, analytes
    dplyr::mutate(
      ReportedUnits = dplyr::case_when(
        ANALYTE == "pH" ~ "unitless",
        ANALYTE == "Corrected PAR" ~ "percent",
        .default = ReportedUnits
    )) %>%
    dplyr::mutate(
      ReportedUnits = ifelse(
        # impute with the mode
        is.na(ReportedUnits) & mean(is.na(ReportedUnits)) !=0,
          as.character(names(sort(table(ReportedUnits), decreasing = T, na.last = T))[1]),
          as.character(ReportedUnits)
      ),
      .by = c(Study, Year, ANALYTE)) %>%
    dplyr::mutate(
      QAcode = dplyr::case_when(
        is.na(ReportedUnits) & !grepl("hydro", Study, ignore.case = T) ~ paste0(QAcode, "; U"),
        .default = QAcode
      ),
      QAcomment = dplyr::case_when(
        is.na(ReportedUnits) & !grepl("hydro", Study, ignore.case = T) ~ paste0(QAcomment, "; No reported units, so assumed most common units for this given analyte-year"),
        .default = QAcomment
      )
    )
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
