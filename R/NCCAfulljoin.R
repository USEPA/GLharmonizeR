#' Read in NCCA 2010 and 2015 water chemistry and hydrographic data
#'
#' @description
#' `.loadNCCA` returns water quality data along with spatial data from the
#'  site information measured through NCCA study in 2010 and 2015
#'
#' @details
#' Join the water quality and hydrographic data and filter to specified lakes.
#' @param NCCAsites2010 a string specifying the URL for the 2010 site data
#' @param NCCAsites2015 a string specifying the URL for the 2015 site data
#' @param NCCAwq2010 a string specifying the URL for the 2010 water chemistry data
#' @param NCCAwq2015 a string specifying the URL for the 2015 water chemistry data
#' @param NCCAhydrofiles2010 a string specifying the URL for the 2010 hydrographic data
#' @param NCCAhydrofile2015 a string specifying the URL for the 2015 hydrographic data
#' @param NCCAsecchifile2015 a string specifying the URL for the 2015 Secchi data
#' @param namingFile a string specifying the URL for the analyte naming file
#' @param Lakes List of Great Lakes to output
#' @param n_max integer specifying how many lines to read of each file to save time for testing
#' @return dataframe
.loadNCCA <- function(NCCAsites2010, NCCAsites2015, NCCAwq2010, NCCAwq2015,
                          NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015,
                          namingFile,
                          Lakes = NULL, n_max = Inf) {

  # Note that all GL are included and have been cleaned

  NCCAhydro <- .loadNCCAhydro(
    NCCAhydrofiles2010, NCCAsites2010,
    NCCAhydrofile2015, NCCAsites2015,
    NCCAsecchifile2015, namingFile,
    n_max = n_max) %>%
    dplyr::mutate(UID = paste0("NCCA_hydro", "-", as.character(UID)))

  # [ ] KV: Need to fill in NCCAwq2010 site info using NCCAhydrofiles2010 data because the stationDepths were checked for accuracy in the 2010 hydro data  (compared to CTD depths) and want station info to match

  # Pull out 2010 hydro site info here and join it below to wq data below
  # lat, long, stationDepth, WTBDY_NM

  # Read NCCA Water chemistry files
  # [x] Make the wqQA argument name consistent over all levels
  nccaWQ <- dplyr::bind_rows(
    .loadNCCAwq2010(NCCAwq2010, NCCAsites2010, namingFile, n_max = n_max),
    .loadNCCAwq2015(NCCAwq2015, NCCAsites2015, namingFile, n_max = n_max)
  ) %>%
    # QC filters
    # filter(! QACODE %in% c("J01", "Q08", "ND", "Q", "H", "L"))
    dplyr::mutate(
      UID = paste0(Study, "-", as.character(UID))
      )




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
        is.na(ReportedUnits) ~ paste0(QAcode, "; U"),
        .default = QAcode
      ),
      QAcomment = dplyr::case_when(
        is.na(ReportedUnits) ~ paste0(QAcomment, "; No reported units, so assumed most common units for this given analyte-year"),
        .default = QAcomment
        # [x] KV: Not sure why this code isn't applied to hydro or secchi study types?
        # - this was before I added the unts within the loading files, so no longer necessary
      )
    ) %>%
  dplyr::mutate(
    Finalized = as.character(Finalized),
    )
  # [x] KV: Move this final mutate function to within .loadNCCA() function if still needed


  return(final)
}
