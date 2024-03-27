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
                         greatLakes=TRUE, Lakes=c("Lake Michigan")) {

  sites <- .readNCCASites(ncca2010sites, ncca2015sites)

  NCCAhydro <- .readNCCAhydro(NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015) %>%
    dplyr::mutate(UID = as.character(UID))

  # Read NCCA WQ files 
  nccaWQ <- .readNCCA(tenFiles, fifteenFiles) %>%
    dplyr::mutate(UID = as.character(UID)) %>%
    dplyr::mutate(SAMPYEAR = lubridate::year(Date)) #%>%

  QA <- readr::read_csv(tenQAfile) %>%
    dplyr::select(-`...3`) %>%
    dplyr::rename(QAconsiderations = Considerations)
    
  final <- dplyr::bind_rows(NCCAhydro, nccaWQ) %>%
    dplyr::left_join(., sites, by = "SITE_ID") %>%
    dplyr::left_join(., QA, by = c("QAcode" =  "Unique Qualifier Code")) %>%
    dplyr::mutate(
      Latitude = dplyr::coalesce(Latitude.y, Latitude.x),
      Longitude = dplyr::coalesce(Longitude.y, Longitude.x),
      stationDepth = dplyr::coalesce(stationDepth.y, stationDepth.x, Depth),
      waterName = dplyr::coalesce(WTBDY_NM.y, WTBDY_NM.x),
      NCCRreg = dplyr::coalesce(NCCRreg.y, NCCRreg.x),
      Date = dplyr::coalesce(Date, DATE_COL),
      SAMPYEAR = lubridate::year(Date)
      ) %>%
    dplyr::select(-c(
      Latitude.x, Latitude.y,
      Longitude.x, Longitude.y,
      stationDepth.x, stationDepth.y, Depth,
      WTBDY_NM.x, WTBDY_NM.y,
      NCCRreg.x, NCCRreg.y,
      DATE_COL#, QAconsiderations,
      #QA_COMMENT, QAComment,
    )) %>%
    # Great lakes get's priority over spcifying each lake
    {if (greatLakes) {
      dplyr::filter(., NCCRreg == "Great Lakes")
    } else if (!is.null(Lakes)) {
      dplyr::filter(., waterName %in% Lakes)
    } else .}
  return(final)
}
