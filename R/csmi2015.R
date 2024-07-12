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
#' @return dataframe of the fully joined water quality data from CSMI 2015
.LoadCSMI2015 <- function() {
  # Establish connection to the database

  dbi <- RODBC::odbcConnectAccess2007("GL_Data-main/CSMI_2015/CSMI2015_newQuery.accdb")
  # Spatial information
  stationInfo <- RODBC::sqlFetch(dbi, "L1_Stationmaster") %>%
    dplyr::rename(
      SITE_ID = StationCodeKey,
      targetDepth = `DepthM _target`,
      targetLat = LatDD_target, 
      targetLon = LonDD_target) %>%
    dplyr::select(SITE_ID, targetDepth, targetLat, targetLon) %>%
  # sample information (time, depth)
    dplyr::full_join(., RODBC::sqlFetch(dbi, "L2a_StationSampleEvent") %>%

    # [x] Grab eastern time
    dplyr::rename(SITE_ID = StationCodeFK, Latitude = LatDD_actual, Longitude = LonDD_actual, sampleDate = SampleDate, sampleTime = TimeEST, stationDepth = DepthM_actual) %>%
    dplyr::select(SITE_ID, Latitude, Longitude, sampleDate, sampleTime, stationDepth, SampleEventKey))

  # Water chem sample depth
  chemInfo2 <- RODBC::sqlFetch(dbi, "L3a_SampleLayerList") %>% 
    dplyr::rename(STIS = STISkey, SampleEventKey = SampleEventFK, sampleDepth = WQdepth_m) %>%
    dplyr::full_join(., stationInfo)
  # impute missing coordinates from target coordinates if possible
  chem <- RODBC::sqlFetch(dbi, "L3b_LabWQdata") %>%
    # [x] Join WQ with depth, then, simply row bind the CTD to it
    tidyr::pivot_longer(NH4_ugNL:CtoN_atom, names_to = "ANALYTE", values_to = "RESULT") %>%
    dplyr::rename(STIS = `STIS#`, SITE_ID = Chem_site) %>%
    dplyr::select(STIS, SITE_ID, ANALYTE, RESULT) %>%
    dplyr::left_join(chemInfo2) %>%
    # fill missing positions with targetted positions
    dplyr::mutate(
      Latitude = dplyr::coalesce(Latitude, targetLat),
      Longitude = dplyr::coalesce(Longitude, targetLon),
      stationDepth = dplyr::coalesce(stationDepth, targetDepth)
    ) %>%
    dplyr::select(-dplyr::contains("target"), -c(WQlabelname))

  # XXX these are depth matched, so there is more data out there
  # i.e. we could find the raw data and get measures at every 1m 
  ctd <- RODBC::sqlFetch(dbi, "L3b_CTDLayerData") %>% 
    tidyr::pivot_longer(Temptr_C:pH, values_to = "RESULT", names_to = "ANALYTE") %>%
    dplyr::rename(STIS = STISkey, sampleDepth = CTDdepth) %>%
    dplyr::select(STIS, sampleDepth, ANALYTE, RESULT) %>%
    dplyr::left_join(chemInfo2) %>%
    dplyr::mutate(
      Latitude = dplyr::coalesce(Latitude, targetLat),
      Longitude = dplyr::coalesce(Longitude, targetLon),
      stationDepth = dplyr::coalesce(stationDepth, targetDepth)
    ) %>%
    dplyr::select(-c("WQlabelname"), -contains("target"))


  WQ <- dplyr::bind_rows(chem, ctd) %>%
    dplyr::left_join(RODBC::sqlFetch(dbi, "Metadata_WQanalytes"), by = c("ANALYTE" = "WQParam")) %>%
    # Sample event names, WQdepth_m
    # actual coordinates
    # [x] Check if measure is below DL, replace with NA and put a flag
    dplyr::mutate(
      mdl = as.numeric(DetectLimit),
      QAcomment = ifelse(RESULT <= mdl, "Below mdl", NA),
      RESULT = ifelse(RESULT <= mdl, NA, RESULT)
    ) %>%
    dplyr::filter(!grepl("_cmp", ASTlayername)) %>%
    dplyr::select(-ASTlayername) %>%

    # [x] combine date and time
    dplyr::mutate(
      sampleDateTime = lubridate::ymd_h(
        paste0(lubridate::date(sampleDate),
        "-",
        lubridate::hour(sampleTime)
      ))
    ) %>%

    dplyr::rename(
      UNITS = WQUnits,
      METHOD = AnalMethod
    ) %>%
    dplyr::mutate(
      Study = "CSMI_2015",
      Year = 2015
    ) %>%
    dplyr::select(-c(DetectLimit, BDLcorrection, Analyst)) %>%
    # simplify unit names

    # [x] Check if the units have an issue since they don't exactly match the analytes 3 book (pH, temptr)
      dplyr::mutate(
        UNITS = dplyr::case_when(
          UNITS == "ug N/L" ~ "ugl",
          UNITS == "ug P/L" ~ "ugl",
          UNITS == "ug/L" ~ "ugl",
          UNITS == "mg/L" ~ "mgl",
          UNITS == "oC" ~ "C",
          UNITS == "--" ~ NA,
          UNITS == "uS/cm" ~ "uscm",
          UNITS == "percent" ~ "%",
        )
        # [ ] what happend to ph log scale?
        # [ ] Did pH get removed?
      )
  # Note that conversions are done for all CSMI files together

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