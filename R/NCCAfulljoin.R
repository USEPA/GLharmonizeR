LoadNCCAfull <- function(siteFiles, preFiles=NULL, tenFiles=NULL, fifteenFiles=NULL, 
                          NCCAhydrofiles2010, NCCAhydrofile2015, NCCAjsecchifile2015) {
  NCCAhydro <- .readNCCAhydro(NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015) %>%
    dplyr::select(UID, DATE_COL, SAMPLE_DEPTH_M, ANALYTE, RESULT, UNITS, STATION_DEPTH_M) %>%
    dplyr::rename(Date=  DATE_COL, sampleDepth = SAMPLE_DEPTH_M, Depth = STATION_DEPTH_M) %>%
    dplyr::mutate(UID = as.character(UID), STUDY = "NCCA")

  # Read NCCA WQ files 
  nccaWQ <- .readNCCA(siteFiles, preFiles, tenFiles, fifteenFiles)  %>%
    dplyr::filter(dplyr::between(LONGITUDE, -88, -84.9),
           dplyr::between(LATITUDE, 41.7, 46)) %>%
    dplyr::select(UID, Date, DEPTH, ANALYTE, RESULT, UNITS, QAComment) %>%
    dplyr::rename(sampleDepth = DEPTH, Comment = QAComment) %>%
    dplyr::mutate(UID = as.character(UID), 
            STUDY = "NCCA")
  return(dplyr::bind_rows(NCCAhydro, nccaWQ))
}