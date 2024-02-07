source("Code/dataCleaning/GLENDA/readCleanGLENDA.R")
source("Code/dataCleaning/CSMI/csmiJoinAll.r")
source("Code/dataCleaning/NCCA/waterQuality.r")
source("Code/dataCleaning/NCCA/hydrological.r")

library(tidyverse)
NCCAhydro <- .readNCCAhydro(
  hydrofiles2010 = c("https://www.epa.gov/sites/default/files/2016-01/assessed_ncca2010_hydrolab.csv", 
"https://www.epa.gov/sites/default/files/2016-01/not_assessed_ncca2010_hydrolab.csv"),
  hydrofile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_hydrographic_profile_great_lakes-data.csv",
  secchifile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_secchi_great_lakes-data.csv" 
  )

# READ NCCA WQ 
siteFiles <- "Data/Raw/NCCA"
preFiles <- c("Data/Raw/NCCA/nca_waterchemdata.csv")
tenFiles<- c("Data/Raw/NCCA/assessed_ncca2010_waterchem.csv", "Data/Raw/NCCA/nassessedWaterChem2010.csv") 
fifteenFiles <- c("Data/Raw/NCCA/ncca_2015_water_chemistry_great_lakes-data.csv")
nccaWQ <- readNCCA(siteFiles = siteFiles, preFiles = NULL, tenFiles = tenFiles, fifteenFiles = fifteenFiles)  %>%
  filter(between(LONGITUDE, -88, -84.9),
         between(LATITUDE, 41.7, 46))

# READ GLENDA
GLENDA <- readCleanGLENDA("Data/Raw/GLENDA/GLENDA.csv")

# READ CSMI
CSMI <- LoadCSMI(
  file.path("L:", "Priv", "Great lakes Coastal", "2002-2010 Water Quality", "2010"),
  file.path("L:", "Priv", "Great lakes Coastal", "2015 CSMI Lake Michigan",
    "WQ data and database", "CSMI data & database", "CSMI_LkMich2015_Database_working_minsRivMouths.accdb"),
  file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)",
    "Lake Michign ML - General", "Raw_data", "CSMI", "2021")
)


# Join data
colnames(CSMI)
colnames(NCCAhydro)
colnames(nccaWQ)
colnames(GLENDA)


# Drop unwanted analytes 

# Relabel analytes


