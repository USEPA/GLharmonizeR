#' Read in all NCCA from 2000s, 2010, and 2015 including hydrogrpahic data
#'
#' @description
#' `.loadNCCA` returns water quality data along with spatial data from the
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
.loadNCCA <- function(NCCAsites2010, NCCAsites2015, NCCAwq2010, NCCAwq2015,
                          NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015,
                          namingFile, 
                          Lakes = NULL, n_max = Inf) {
  NCCAhydro <- .loadNCCAhydro(
    NCCAhydrofiles2010, NCCAsites2010,
    NCCAhydrofile2015, NCCAsites2015,
    NCCAsecchifile2015, namingFile,
    n_max = n_max) %>%
    dplyr::mutate(UID = paste0("NCCA_hydro", "-", as.character(UID)))

  # Read NCCA Water chemistry files
  # [x] Make the wqQA argument name consistent over all levels
  nccaWQ <- dplyr::bind_rows(
    .loadNCCAwq2010(NCCAwq2010, NCCAsites2010, namingFile, n_max = n_max),
    .loadNCCAwq2015(NCCAwq2015, NCCAsites2015, namingFile, n_max = n_max)
  ) %>%
    # QC filters
    # filter(! QACODE %in% c("J01", "Q08", "ND", "Q", "H", "L"))
    dplyr::mutate(UID = paste0(Study, "-", as.character(UID)))




  final <- dplyr::bind_rows(NCCAhydro, nccaWQ) %>%
    # [x] Fix filter for region and lake
    # Great lakes get's priority over spcifying each lake
    #dplyr::filter(NCCA_REG == "Great Lakes") %>%
    {
     if (!is.null(Lakes)) {
       dplyr::filter(., WTBDY_NM %in% Lakes)
     } else {
       .
     }
    } %>%
    dplyr::mutate(
      QAcode = dplyr::case_when(
        is.na(ReportedUnits) & !grepl("hydro|secchi", Study, ignore.case = T) ~ paste0(QAcode, "; U"),
        .default = QAcode
      ),
      QAcomment = dplyr::case_when(
        is.na(ReportedUnits) & !grepl("hydro|secchi", Study, ignore.case = T) ~ paste0(QAcomment, "; No reported units, so assumed most common units for this given analyte-year"),
        .default = QAcomment
        # [ ] KV: Not sure why this code isn't applied to hydro or secchi study types?
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
