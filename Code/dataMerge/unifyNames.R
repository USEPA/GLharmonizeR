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
  ) %>%
  select(UID, DATE_COL, SAMPLE_DEPTH_M, ANALYTE, RESULT, UNITS, STATION_DEPTH_M) %>%
  rename(Date=  DATE_COL, sampleDepth = SAMPLE_DEPTH_M, Depth = STATION_DEPTH_M) %>%
  mutate(UID = as.character(UID))

# READ NCCA WQ 
siteFiles <- "Data/Raw/NCCA"
preFiles <- c("Data/Raw/NCCA/nca_waterchemdata.csv")
tenFiles<- c("Data/Raw/NCCA/assessed_ncca2010_waterchem.csv", "Data/Raw/NCCA/nassessedWaterChem2010.csv") 
fifteenFiles <- c("Data/Raw/NCCA/ncca_2015_water_chemistry_great_lakes-data.csv")
nccaWQ <- readNCCA(siteFiles = siteFiles, preFiles = NULL, tenFiles = tenFiles, fifteenFiles = fifteenFiles)  %>%
  filter(between(LONGITUDE, -88, -84.9),
         between(LATITUDE, 41.7, 46)) %>%
  select(UID, Date, DEPTH, ANALYTE, RESULT, UNITS, QAComment) %>%
  rename(sampleDepth = DEPTH, Comment = QAComment) %>%
  mutate(UID = as.character(UID))


# READ GLENDA
GLENDA <- readCleanGLENDA("Data/Raw/GLENDA/GLENDA.csv") %>%
  rename(UID = SAMPLE_ID, sampleDepth = SAMPLE_DEPTH_M, Depth = STN_DEPTH_M, Comment = RESULT_REMARK) %>%
  select(UID, sampleDate, Depth, sampleDepth, ANALYTE, VALUE, FRACTION, Comment) %>%
  mutate(UID = as.character(UID))

# READ CSMI
CSMI <- LoadCSMI(
  file.path("L:", "Priv", "Great lakes Coastal", "2002-2010 Water Quality", "2010"),
  file.path("L:", "Priv", "Great lakes Coastal", "2015 CSMI Lake Michigan",
    "WQ data and database", "CSMI data & database", "CSMI_LkMich2015_Database_working_minsRivMouths.accdb"),
  file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)",
    "Lake Michign ML - General", "Raw_data", "CSMI", "2021")
) %>%
  rename(UID = `STIS#`) %>%
  select(`STIS#`, Depth, FRACTION, LATITUDE, LONGITUDE, sampleDepth, ANALYTE, UNITS, RESULT, mdl, Date) %>%
  mutate(UID = as.character(UID))

# Join data
allWQ <- bind_rows(NCCAhydro, nccaWQ, CSMI, GLENDA) %>%
  drop_na(RESULT)


# Drop unwanted analytes 

# Relabel analytes
namingFile <- file.path(
  "C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)", "Lake Michigan ML - General", "Results", "Analytes3.xlsx"
)



renamingTable <- bind_rows(
  readxl::read_xlsx(namingFile, sheet = "GLENDA_Map",
  col_types = rep("text", 12)),
  readxl::read_xlsx(namingFile, sheet = "NCCA_Map",
  col_types = rep("text", 10)) ,
  readxl::read_xlsx(namingFile, sheet = "CSMI_Map", 
  col_types = rep("text", 10))
)


