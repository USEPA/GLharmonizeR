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

  sites <- .readNCCASites(siteFiles) %>%
    dplyr::distinct(SITE_ID, .keep_all =T) 

  NCCAhydro <- .readNCCAhydro(NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015)
    # dplyr::rename(Date=  DATE_COL, sampleDepth = SAMPLE_DEPTH_M, Depth = STATION_DEPTH_M) %>%
    dplyr::mutate(UID = as.character(UID))

  # Read NCCA WQ files 
  nccaWQ <- .readNCCA(preFiles, tenFiles, fifteenFiles, tenQAfile, greatLakes=TRUE, Lakes=NULL)


  final <- dplyr::bind_rows(NCCAhydro, nccaWQ) %>%
    dplyr::left_join(sites, by = "SITE_ID") %>%
    dplyr::mutate(
      Latitude = dplyr::coalesce(LATITUDE.y, LATITUDE.x),
      Longitude = dplyr::coalesce(LONGITUDE.y, LONGITUDE.x),
      Depth = dplyr::coalesce(STATION_DEPTH.y, STATION_DEPTH.x, Depth),
      waterName = dplyr::coalesce(WTBDY_NM.y, WTBDY_NM.x),
      NCCRreg = dplyr::coalesce(NCCR_REG.y, NCCR_REG.x),
      Date = dplyr::coalesce(DATE_COL, Date),
      QAcomment = dplyr::coalesce(QA_COMMENT, QAconsiderations),
      SAMPYEAR = lubridate::year(Date)
      ) %>%
    dplyr::select(-c(
      LATITUDE.x, LATITUDE.y,
      LONGITUDE.x, LONGITUDE.y,
      STATION_DEPTH.x, STATION_DEPTH.y,
      WTBDY_NM.x, WTBDY_NM.y,
      NCCR_REG.x, NCCR_REG.y,
      DATE_COL, QAconsiderations 
    )) %>%
    # Great lakes get's priority over spcifying each lake
    {if (greatLakes) {
      dplyr::filter(., NCCRreg == "Great Lakes")
    } else if (!is.null(Lakes)) {
      dplyr::filter(., waterName %in% Lakes)
    }}

  return(final)
}