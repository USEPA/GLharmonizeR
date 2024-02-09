# This file opens the CSMI2015 water quality database hosted locally on the L: drive, and joins the data
# This is a hardcoded path to the CSMI 2015 water quality data. Note that if this path changes, we will need to update the path
#' Load and join data for CSMI 2015 from access database
#'
#' @description
#' `.LoadCSMI2015` returns a dataframe of all of the joined water quality data relating to CSMI 2015
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the filepath of the access database
#' @return dataframe of the fully joined water quality data from CSMI 2015
.LoadCSMI2015 <- function(filepath) {
  # Establish connection to the database
  dbi <- RODBC::odbcConnectAccess2007(filepath)

  # read and join tables
  WQ <- RODBC::sqlFetch(dbi, "L3b_LabWQdata") %>%
    # CTD
    dplyr::left_join(RODBC::sqlFetch(dbi, "L3b_CTDLayerData"), by = c("STIS#" = "STISkey")) %>%
    tidyr::pivot_longer(-c(`STIS#`, Chem_site, Chem_layer, SampleEvent, CTDdepth), names_to = "ANALYTE", values_to = "RESULT") %>%
    # Is it safe to say that CTD depth is also the sample grab depth for a given STIS/SampleEvent
    # Units and detection limits
    dplyr::left_join(RODBC::sqlFetch(dbi, "Metadata_WQanalytes"), by = c("ANALYTE" = "WQParam")) %>%
    # postition info (Station)
    dplyr::left_join(RODBC::sqlFetch(dbi, "L1_Stationmaster"), by = c("Chem_site" = "StationCodeKey")) %>%
    # Sample event names, WQdepth_m
    dplyr::left_join(RODBC::sqlFetch(dbi, "L3a_SampleLayerList"), by = c("STIS#" = "STISkey")) %>%
    # actual coordinates
    dplyr::left_join(RODBC::sqlFetch(dbi, "L2a_StationSampleEvent"), by = c("SampleEventFK" = "SampleEventKey")) %>%
    dplyr::rename(LATITUDE = LatDD_actual, LONGITUDE = LonDD_actual) %>%
    dplyr::filter(!grepl("_cmp", ASTlayername)) %>%
    dplyr::mutate(DateTime = as.POSIXct(paste(lubridate::date(SampleDate), lubridate::hour(TimeUTC)), format = "%Y-%m-%d %H"),
           DetectLimit = as.numeric(DetectLimit)) %>%
    # 90% of CTDdepth == WQdepth_m, on average they differ by -0.009 meters. So we'll call them equal
    dplyr::rename(sampleDepth = CTDdepth,
           Depth = DepthM_actual,
           UNITS = WQUnits,
           mdl = DetectLimit,
           ANL_CODE = WQlabelname,
           Date = SampleDate) %>%
    dplyr::select(
      -dplyr::contains(c("DD", "_target", "Cruise", "Time")),
      -c(Chem_site, Chem_layer, SampleEvent, Analyst, AltStationName, 
        PlaceName, ProjectName, SiteType, DepthStrata, PositNS, `PositNS#`,
        PositEW, BDLcorrection, SampleEventFK, ASTlayername, StationCodeFK,
        SurveyVessel, WQdepth_m,
        )
        )
           

  # Unused tables
  # metaChange <- RODBC::sqlFetch(dbi, "Metadata_ChangeLog") # editors, edited dates
  # metaChem<- RODBC::sqlFetch(dbi, "Metadata_ChemLayerDef") # written description of sampling depth
  # metaTherm <- RODBC::sqlFetch(dbi, "Metadata_ThermLayerDef") # comments on stratification
  # transect <- RODBC::sqlFetch(dbi, "Qry_TransectData") # This might contain all of the data in the whole database
  # therm <- RODBC::sqlFetch(dbi, "L2b_ThermStructure") # Numbers describing sampling w/r/t thermocline structures

  RODBC::odbcClose(dbi)

  # looking up, this seems like standard driver protocol if wanting to use more precise connection
  # For DBI and dbplyr
  # Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=C:\mydatabase.accdb;Uid=Admin;Pwd=;

  return(WQ)
}
