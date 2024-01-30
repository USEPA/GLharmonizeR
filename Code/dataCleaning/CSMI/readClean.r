# This file opens the CSMI2015 water quality database hosted locally on the L: drive, and joins the data
# This is a hardcoded path to the CSMI 2015 water quality data. Note that if this path changes, we will need to update the path

library(tidyverse)


filepath <- file.path(
  "L:",
  "Priv",
  "Great lakes Coastal",
  "2015 CSMI Lake Michigan",
  "WQ data and database",
  "CSMI data & database",
  "CSMI_LkMich2015_Database_working_minsRivMouths.accdb"
)

# Establish connection to the database
dbi <- RODBC::odbcConnectAccess2007(filepath)

WQ <- RODBC::sqlFetch(dbi, "L3b_LabWQdata") %>%
  # CTD
  left_join(RODBC::sqlFetch(dbi, "L3b_CTDLayerData"), by = c("STIS#" = "STISkey")) %>%
  pivot_longer(-c(`STIS#`, Chem_site, Chem_layer, SampleEvent, CTDdepth), names_to = "ANALYTE", values_to = "RESULT") %>%
  # Is it safe to say that CTD depth is also the sample grab depth for a given STIS/SampleEvent
  # Units and detection limits
  left_join(RODBC::sqlFetch(dbi, "Metadata_WQanalytes"), by = c("ANALYTE" = "WQParam"))  %>%
  # postition info (Station)
  left_join(RODBC::sqlFetch(dbi, "L1_Stationmaster"), by = c("Chem_site" = "StationCodeKey")) %>%
  # Sample event names, WQdepth_m
  left_join(RODBC::sqlFetch(dbi, "L3a_SampleLayerList"), by = c("STIS#"  = "STISkey")) %>%
  # actual coordinates
  left_join(RODBC::sqlFetch(dbi, "L2a_StationSampleEvent"), by = c("SampleEventFK" = "SampleEventKey"))

# Unused
# metaChange <- RODBC::sqlFetch(dbi, "Metadata_ChangeLog") # editors, edited dates
# metaChem<- RODBC::sqlFetch(dbi, "Metadata_ChemLayerDef") # written description of sampling depth
# metaTherm <- RODBC::sqlFetch(dbi, "Metadata_ThermLayerDef") # comments on stratification 
# transect <- RODBC::sqlFetch(dbi, "Qry_TransectData") # This might contain all of the data in the whole database
# therm <- RODBC::sqlFetch(dbi, "L2b_ThermStructure") # Numbers describing sampling w/r/t thermocline structures

RODBC::odbcClose(dbi)

# looking up, this seems like standard driver protocol if wanting to use more precise connection
# For DBI and dbplyr
# Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=C:\mydatabase.accdb;Uid=Admin;Pwd=;
