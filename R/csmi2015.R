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
#' @param csmi2015 a string specifying the filepath of the access database
#' @return dataframe of the fully joined water quality data from CSMI 2015
.LoadCSMI2015 <- function(csmi2015) {
  # Establish connection to the database
  dbi <- RODBC::odbcConnectAccess2007(csmi2015)

  # read and join tables
  WQ <- RODBC::sqlFetch(dbi, "L3b_LabWQdata") %>%
    # CTD
    # [ ] Join WQ with depth, then, simply row bind the CTD to it
    # [ ] Use station code key for SITE_ID
    # [ ] Use WQdepth_m from SampleLayerList for WQ depths 
    # [ ] redo the joins either traversing upwards or downwards the tree, with th, twice for each CTD and WQ
    # Then row bind them
    dplyr::left_join(RODBC::sqlFetch(dbi, "L3b_CTDLayerData"), by = c("STIS#" = "STISkey")) %>%
    tidyr::pivot_longer(-c(`STIS#`, Chem_site, Chem_layer, SampleEvent, CTDdepth), names_to = "ANALYTE", values_to = "RESULT") %>%
    # CTD depth is the same as sample grab depth for a given STIS/SampleEvent
    # Units and detection limits
    dplyr::left_join(RODBC::sqlFetch(dbi, "Metadata_WQanalytes"), by = c("ANALYTE" = "WQParam")) %>%
    # postition info (Station)
    dplyr::left_join(RODBC::sqlFetch(dbi, "L1_Stationmaster"), by = c("Chem_site" = "StationCodeKey")) %>%
    # Sample event names, WQdepth_m
    dplyr::left_join(RODBC::sqlFetch(dbi, "L3a_SampleLayerList"), by = c("STIS#" = "STISkey")) %>%
    # actual coordinates
    dplyr::left_join(RODBC::sqlFetch(dbi, "L2a_StationSampleEvent"), by = c("SampleEventFK" = "SampleEventKey")) %>%
    # [ ] Should we grab DD vs DDM?
    # [ ] Grab eastern time
    # [ ] Check if measure is below DL, replace with NA and put a flag
    # [ ] Check if the units have an issue since they don't exactly match the analytes 3 book (pH, temptr)
    # [ ] CTD and WQ need site, position data joined spearate use StationCodeFK
    dplyr::rename(Latitude = LatDD_actual, Longitude = LonDD_actual) %>%
    dplyr::filter(!grepl("_cmp", ASTlayername)) %>%
    dplyr::mutate(DateTime = as.POSIXct(paste(lubridate::date(SampleDate), lubridate::hour(TimeETC)), format = "%Y-%m-%d %H"),
           DetectLimit = as.numeric(DetectLimit)) %>%

    # 90% of CTDdepth == WQdepth_m, on average they differ by -0.009 meters. So we'll call them equal
    dplyr::rename(
      sampleDepth = CTDdepth,
      SITE_ID = Chem_site,
      stationDepth = DepthM_actual,
      UNITS = WQUnits,
      mdl = DetectLimit,
      sampleDateTime = DateTime
    ) %>%
    dplyr::select(
      -dplyr::contains(c("DD", "_target", "Cruise", "Time")),
      -c(Chem_layer, SampleEvent, Analyst, AltStationName, 
        PlaceName, ProjectName, SiteType, DepthStrata, PositNS, `PositNS#`,
        PositEW, BDLcorrection, SampleEventFK, ASTlayername, StationCodeFK,
        SurveyVessel, WQdepth_m,
        )) %>%
    dplyr::mutate(
      Study = "CSMI_2015",
      Year = 2015
      )  %>%
    dplyr::rename(STIS = `STIS#`, METHOD = AnalMethod)

  RODBC::odbcClose(dbi)

  return(WQ)
}

# Unused tables
# metaChange <- RODBC::sqlFetch(dbi, "Metadata_ChangeLog") # editors, edited dates
# metaChem<- RODBC::sqlFetch(dbi, "Metadata_ChemLayerDef") # written description of sampling depth
# metaTherm <- RODBC::sqlFetch(dbi, "Metadata_ThermLayerDef") # comments on stratification
# transect <- RODBC::sqlFetch(dbi, "Qry_TransectData") # This might contain all of the data in the whole database
# therm <- RODBC::sqlFetch(dbi, "L2b_ThermStructure") # Numbers describing sampling w/r/t thermocline structures


# looking up, this seems like standard driver protocol if wanting to use more precise connection
# For DBI and dbplyr
# Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=C:\mydatabase.accdb;Uid=Admin;Pwd=;