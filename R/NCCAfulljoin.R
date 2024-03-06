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
#' @param preFiles filepath to pre 2010's data
#' @param tenFiles filepath to 2010's data
#' @param fifteenFiles filepaht to 2015 data
#' @param NCCAhydrofiles2010 filepath to hydrogrpahic 2010 data
#' @param NCCAhydrofile2015 filepath to hydrogarphic 2015 data
#' @param NCCAjsecchifile2015  filepaht to secchi 2015 data
#' @return dataframe
#' @export
LoadNCCAfull <- function(siteFiles, preFiles=NULL, tenFiles=NULL, fifteenFiles=NULL, greatLakes=TRUE, Lakes=NULL,
                          NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015) {
  NCCAhydro <- .readNCCAhydro(NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015) %>%
    dplyr::select(UID, DATE_COL, SAMPLE_DEPTH_M, ANALYTE, RESULT, UNITS, STATION_DEPTH_M) %>%
    dplyr::rename(Date=  DATE_COL, sampleDepth = SAMPLE_DEPTH_M, Depth = STATION_DEPTH_M) %>%
    dplyr::mutate(UID = as.character(UID), STUDY = "NCCA")

  # Read NCCA WQ files 
  nccaWQ <- .readNCCA(siteFiles, preFiles, tenFiles, fifteenFiles, greatLakes=TRUE, Lakes=NULL)  %>%
    dplyr::filter(dplyr::between(LONGITUDE, -88, -84.9),
           dplyr::between(LATITUDE, 41.7, 46)) %>%
    dplyr::select(UID, SITE_ID, LATITUDE, LONGITUDE, Date, WTBDY_NM, NCCR_REG, STATION_DEPTH, ANALYTE, RESULT, UNITS, QAComment) %>%
    dplyr::mutate(UID = as.character(UID), 
            STUDY = "NCCA")
  return(dplyr::bind_rows(NCCAhydro, nccaWQ))
}