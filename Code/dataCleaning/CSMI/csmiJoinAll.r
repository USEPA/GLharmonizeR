# Join all CSMI water quality data
library(tidyverse)
source("Code/dataCleaning/CSMI/csmi2010.r")
source("Code/dataCleaning/CSMI/csmi2015.r")
source("Code/dataCleaning/CSMI/csmi2021.r")

fp <- file.path(
  "L:",
  "Priv",
  "Great lakes Coastal",
  "2002-2010 Water Quality",
  "2010")

csmi2010 <- .LoadCSMI2010(fp)



dir(file.path(
  "L:",
  "Priv",
  "Great lakes Coastal",
  "2002-2010 Water Quality",
  "2010"))
csmi2015 <- .LoadCSMI2015(file.path(
  "L:",
  "Priv",
  "Great lakes Coastal",
  "2015 CSMI Lake Michigan",
  "WQ data and database",
  "CSMI data & database",
  "CSMI_LkMich2015_Database_working_minsRivMouths.accdb"
))

csmi2021 <- .LoadCSMI2021(file.path(
  "C:",
  "Users",
  "ccoffman",
  "Environmental Protection Agency (EPA)",
  "Lake Michigan ML - General",
  "Raw_data",
  "CSMI",
  "2021"
))
