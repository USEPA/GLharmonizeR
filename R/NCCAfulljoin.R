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
    dplyr::select(-STUDY, -PUBLICATION_DATE, -YEAR, -INDEX_NCCA15, -SAMPLE_TYPE, -CLEAR_TO_BOTTOM, -NCCA_REG, -CAST)


  # Fill in NCCAwq2010 site info using NCCAhydrofiles2010 data because the stationDepths were checked for accuracy in the 2010 hydro data  (compared to CTD depths) and want station info to match

  # Pull out 2010 hydro site info here and join it below to wq data below
  hydro2010_sitedat <- NCCAhydro %>% dplyr::filter(Study == "NCCA_hydro_2010") %>%
    dplyr::select(SITE_ID, UID, Latitude, Longitude, stationDepth, WTBDY_NM) %>%
    dplyr::distinct()


  # Read NCCA Water chemistry files
  wq2010 <- .loadNCCAwq2010(NCCAwq2010, NCCAsites2010, namingFile, n_max = n_max) %>%
    dplyr::left_join(hydro2010_sitedat) # Join in

  wq2015 <- .loadNCCAwq2015(NCCAwq2015, NCCAsites2015, namingFile, n_max = n_max)

  nccaWQ <- dplyr::bind_rows(
      wq2010,
      wq2015
  )


  final <- dplyr::bind_rows(NCCAhydro, nccaWQ) %>%
    {
     if (!is.null(Lakes)) {
       dplyr::filter(., WTBDY_NM %in% Lakes)
     } else {
       .
     }
    } %>%
  dplyr::mutate(
    UID = paste0("NCCA-", UID),
    Finalized = as.character(Finalized) # Not sure if this is really necessary anymore?
    )

  return(final)
}
