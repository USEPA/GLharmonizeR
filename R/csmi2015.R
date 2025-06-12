#' Load and join data for CSMI 2015 from Access database
#'
#' @description
#' `.readCleanCSMI2015` returns a dataframe of joined water quality data from CSMI 2015
#'
#' @details
#' This is a hidden function, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @return dataframe of the fully joined water quality data from CSMI 2015
.readCleanCSMI2015 <- function(csmi2015, namingFile) {

  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor)) %>%
    dplyr::distinct() # Duplicate rows

  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "CSMI_Map", na.strings = c("", "NA")) %>%
    dplyr::mutate(ANALYTE = stringr::str_remove_all(ANALYTE, "\\.")) %>%
    dplyr::select(-Methods) %>% # Removing Methods because these were filled in manually in spreadsheet and are incomplete, they're not needed for joining, and we can pull out the methods in mdl object and join back in
    dplyr::select(-Units) # Should remove Units from these renamingTables so they don't cause confusion with the units parsed/read from the data. Units in renaming tables are prone to human error.


  # Establish connection to the database
  download.file(csmi2015, destfile = "tempCSMI2015.zip")
  unzip("tempCSMI2015.zip")
  file.remove("tempCSMI2015.zip")

  dbi <- RODBC::odbcConnectAccess2007("CSMI2015_newQuery/CSMI2015_newQuery.accdb")

  # Spatial information
  stationInfo <- RODBC::sqlFetch(dbi, "L1_Stationmaster") %>%
    dplyr::select(
      SITE_ID = StationCodeKey,
      targetDepth = `DepthM _target`,
      targetLat = LatDD_target,
      targetLon = LonDD_target
    )

    # sample information (time, depth)
  eventInfo <- RODBC::sqlFetch(dbi, "L2a_StationSampleEvent") %>%
      # Grab eastern time
      dplyr::select(SampleEventKey, SITE_ID = StationCodeFK, Latitude = LatDD_actual, Longitude = LonDD_actual, sampleDate = SampleDate, sampleTimeEST = TimeEST, stationDepth = DepthM_actual) %>%
      # one of the MAN_46 samples is out of bounds, so impute its coordinates
      # Target is lat=44.113283, lon=-87.468617
      # Looks like the coordinates are probably okay except that latitude should have started with 44, not 46. So leave longitude and replace latitude with 44.1116833333333 (instead of 46.1116833333333)
      dplyr::mutate(
        Latitude = ifelse(
          (SITE_ID == "Man_46") & (Latitude > 46),
          44.1116833333333, Latitude
        ),
        sampleDateTimeEST = sampleTimeEST,
        sampleDateTimeEST = lubridate::`date<-`(sampleDateTimeEST, sampleDate),
        sampleDateTime = lubridate::ymd_hms(sampleDateTimeEST, tz = "Etc/GMT+5"),
        sampleDateTime =  lubridate::with_tz(sampleDateTime, tzone = "UTC"),
        sampleDate = lubridate::date(sampleDateTime),
        sampleTimeUTC = lubridate::hour(sampleDateTime)
      ) %>%
     dplyr::select(-sampleTimeEST, -sampleDateTimeEST)

  # Water chem sample depth
  stisInfo <- RODBC::sqlFetch(dbi, "L3a_SampleLayerList") %>%
    dplyr::select(STIS = STISkey, SampleEventKey = SampleEventFK, sampleDepth = WQdepth_m,
      sampleType = ASTlayername) %>%
    dplyr::mutate(
      SampleEventKey = ifelse(
        STIS %in% as.character(5008:5015),
        "Fra_46_May_LE2", SampleEventKey
      )
    )
  # *** Note: STIS 5008-5015 has wrong SampleEventFK in L3a_SampleLayerList above ***
  # Should be Fra_46_May_LE2 - KV changed and will inform Anett
  # Note that this can be seen based on max depths of WQdepth_m, and in the SampleEvents in L3b_CTDLayerData


  sampleInfo <- dplyr::left_join(stisInfo, eventInfo, by = "SampleEventKey") %>%
    dplyr::left_join(stationInfo, by = "SITE_ID")



  mdls <- RODBC::sqlFetch(dbi, "Metadata_WQanalytes") %>%
    dplyr::select(ANALYTE = WQParam,
                  ReportedUnits = WQUnits,
                  mdl = DetectLimit,
                  LAB = Analyst,
                  METHOD = AnalMethod) %>%
    dplyr::mutate(
      Study = "CSMI_2015",
      ANALYTE = stringr::str_extract(ANALYTE, "[:alnum:]*")
      ) %>%
    dplyr::left_join(renamingTable, by = c("Study", "ANALYTE")) %>%
    dplyr::filter(CodeName != "Remove") %>% # Note also removes NAs - which is DOC
    dplyr::left_join(key, by = "CodeName") %>%
    dplyr::mutate(
      ReportedUnits = stringr::str_remove(ReportedUnits, "/"),
      ReportedUnits = tolower(ReportedUnits)) %>%
    # Manually change pH and temp units
    dplyr::mutate(
      ReportedUnits = ifelse(ANALYTE == "pH", "unitless", ReportedUnits),
      ReportedUnits = ifelse(ANALYTE == "Temptr", "c", ReportedUnits)
    ) %>%
    dplyr::left_join(conversions) %>%
    dplyr::mutate(
      mdl = as.numeric(mdl),
      mdl = ifelse(!is.na(ConversionFactor), mdl*ConversionFactor, mdl),
      ) %>%
    dplyr::select(ANALYTE, CodeName, mdl, LAB, METHOD) # Include LAB and METHOD


  # impute missing coordinates from target coordinates if possible
  chem <- RODBC::sqlFetch(dbi, "L3b_LabWQdata") %>%
    tidyr::pivot_longer(NH4_ugNL:CtoN_atom, names_to = "ANALYTE", values_to = "RESULT") %>%
    # NAs in this dataset were simply unmeasured
    tidyr::drop_na(RESULT) %>%
    dplyr::select(STIS = `STIS#`, SITE_ID = Chem_site, ANALYTE, RESULT) %>%
    dplyr::left_join(sampleInfo, by = "STIS") %>%
    # retain the site information from the sample information --
      # OK for Sjo_XtraDeep=Sjo_xd and MI18/Deep Stn 18 = Rac_xd
      # Needed to correct Fra_46 from L3a_SampleLayerList though
    dplyr::select(-SITE_ID.x) %>%
    # fill missing positions with targeted positions - NOT NEEDED
    # dplyr::mutate(
    #   Latitude = dplyr::coalesce(Latitude, targetLat),
    #   Longitude = dplyr::coalesce(Longitude, targetLon),
    #   stationDepth = dplyr::coalesce(stationDepth, targetDepth)
    # ) %>%
    dplyr::rename(SITE_ID = SITE_ID.y)

  # CTD data are matched to water chem by depth, so there are potentially more CTD data available
  # i.e. we could find the raw data and get measures at every 1m
  ctd <- RODBC::sqlFetch(dbi, "L3b_CTDLayerData") %>%
    tidyr::pivot_longer(Temptr_C:pH, values_to = "RESULT", names_to = "ANALYTE") %>%
    tidyr::drop_na(RESULT) %>%
    dplyr::select(STIS = STISkey, sampleDepth = CTDdepth, ANALYTE, RESULT) %>%
    dplyr::left_join(sampleInfo, by = "STIS") %>%
    # only want to keep the ctd sample depth
    dplyr::select(-sampleDepth.y) %>%
    # All have actual lat/longs so don't need coalesce statement
    # dplyr::mutate(
    #   Latitude = dplyr::coalesce(Latitude, targetLat),
    #   Longitude = dplyr::coalesce(Longitude, targetLon),
    #   stationDepth = dplyr::coalesce(stationDepth, targetDepth)
    # ) %>%
    dplyr::rename(sampleDepth = sampleDepth.x)

  WQ <- dplyr::bind_rows(chem, ctd) %>%
    dplyr::filter(!grepl("_cmp", sampleType)) %>%
    dplyr::select(-sampleType) %>%
    tidyr::separate_wider_delim(ANALYTE, delim = "_", names= c("ANALYTE", "UNITS"), too_many = "merge", too_few = "align_start") %>%
    dplyr::mutate(
      Study = "CSMI_2015",
      Year = 2015,
      ANALYTE = stringr::str_extract(ANALYTE, "[:alnum:]*")
    ) %>%
    dplyr::left_join(renamingTable) %>%
    dplyr::filter(!grepl("remove", CodeName, ignore.case=T)) %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    dplyr::left_join(key) %>%
    dplyr::mutate(
      ReportedUnits = tolower(ReportedUnits)) %>%
    # Manually change units so that conversions table joins correctly
    dplyr::mutate(
      ReportedUnits = ifelse(ANALYTE == "pH", "unitless", ReportedUnits),
      ReportedUnits = ifelse(ReportedUnits == "ugnl", "ug nl", ReportedUnits),
      ReportedUnits = ifelse(ReportedUnits == "ugl_lach", "ugl", ReportedUnits),
      ReportedUnits = ifelse(ReportedUnits == "ugl_sirms", "ugl", ReportedUnits),
      ReportedUnits = ifelse(ReportedUnits == "pct", "percent", ReportedUnits)
    ) %>%
    dplyr::left_join(conversions) %>%
    dplyr::mutate(RESULT = ifelse(!is.na(ConversionFactor), RESULT * ConversionFactor, RESULT)) %>%
    dplyr::left_join(mdls)

  RODBC::odbcClose(dbi)
  file.remove("CSMI2015_newQuery/CSMI2015_newQuery.accdb")
  unlink("CSMI2015_newQuery", recursive = TRUE)

  # missingness/joining checks in output:
  # sum(is.na(WQ$CodeName)): 0
  # sum(WQ$CodeName == "Remove"): 0
  # sum(is.na(WQ$TargetUnits)): 0
  # WQ %>% dplyr::filter(ReportedUnits != TargetUnits) %>% dplyr::reframe(sum(is.na(ConversionFactor))): 0 cases
  # sum(is.na(WQ$sampleDateTime))  # 0
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
