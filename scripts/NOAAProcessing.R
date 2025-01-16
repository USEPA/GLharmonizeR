# Remember need to be on VPN to process this data

library(devtools)
library(tidyverse)
library(oce)
load_all()
options(insertCalculatedDataCTD=TRUE)

filepaths <- .getFilePaths()
noaaWQ <- filepaths["noaaWQ"]
noaaWQSites <- filepaths["noaaWQSites"]
namingFile <- filepaths["namingFile"]
#noaaSites <- openxlsx::read.xlsx(filepaths["noaaWQSites"])
n_max <- Inf

noaaFiles <- readRDS(url("https://github.com/kvitense/GL_Data/raw/refs/heads/main/NOAA/ctdFileMetaData.Rds", "rb")) %>%
  # remove files where we don't know the station
  drop_na(SITE_ID) %>%
  select(ctdFiles, SITE_ID, sampleDateTime, stationDepth, Latitude, Longitude) %>%
  mutate(
    ctdFiles = file.path("/Users", "ccoffman", "Environmental Protection Agency (EPA)",
      "Lake Michigan ML - General", "Raw_data", "NOAA", "CTD 2007-2022", ctdFiles))

key <- openxlsx::read.xlsx(filepaths["namingFile"], sheet = "Key") %>%
  dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
  dplyr::rename(TargetUnits = Units)

conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
  dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))
renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NOAA_Map") %>%
    filter(Study == "NOAActd") %>%
    distinct(Study, ANALYTE, CodeName)

#### Need to add to NOAA renaming table
# noaa ctd doesn't have spar so no cpar
#oce::read.oce(noaaFiles$ctdFiles[180])
noaaCTDdf <- noaaFiles %>%
  mutate(
    data = purrr::map(ctdFiles, \(x)
      tryCatch({
      .oce2df(suppressWarnings(oce::read.oce(x)), studyName = "NOAActd", bin = TRUE, downcast = TRUE) %>% 
        # take the station information froma the filepath
        rename(Latitude.ctd = Latitude, Longitude.ctd  = Longitude, sampleDateTime.ctd = sampleDateTime, stationDepth.ctd = stationDepth)},
      error = function (e) {
        message(paste("Error reading file", basename(x)))
        NULL
      }),
        .progress = TRUE)
  ) %>%
  tidyr::unnest(cols = data) %>%
  # prioritize ctd machine information
  mutate(
    Latitude = coalesce(Latitude, Latitude.ctd),
    Longitude = coalesce(Longitude, Longitude.ctd),
    sampleDateTime = coalesce(sampleDateTime.ctd, sampleDateTime),
    stationDepth = coalesce(stationDepth.ctd, stationDepth),
    Study = "NOAActd",
    UID = paste0(SITE_ID, "_", lubridate::date(sampleDateTime))
    ) %>%
  left_join(renamingTable, by = c("Study", "ANALYTE")) %>%
  rename(ReportedUnits = UNITS)  %>%
  mutate(ReportedUnits = tolower(ReportedUnits)) %>%
  left_join(key, by =  "CodeName") %>%
  left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
  mutate(
    RESULT = ifelse(!is.na(ConversionFactor), RESULT * ConversionFactor, RESULT)) %>%
  dplyr::select(Study, UID, SITE_ID, sampleDateTime, stationDepth, Latitude, Longitude, sampleDepth, CodeName, RESULT, Units = Explicit_Units, Category, ANALYTE_Orig_Name = ANALYTE, ConversionFactor, LongName) %>%
  filter(ANALYTE_Orig_Name != "par")

# Save the cleaned seabird Files
saveRDS(noaaCTDdf, "../GL_Data/NOAA/noaaCTD.Rds")

# noaaCTDdf %>%
# # Check which names might not be getting matched
#   filter(is.na(CodeName), ANALYTE_Orig_Name != "par") %>%
#   distinct(ANALYTE_Orig_Name)
# 
# 
# noaaCTDdf %>%
#   ggplot(aes(x = date(sampleDateTime))) +
#   geom_histogram()
# 
# read.oce(noaaFiles$cnvFiles[[7]])
# 
# 
# 
# test <- oce::read.oce(file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)",
#  "Lake Michigan ML - General", "Raw_data", "NOAA", "CTD 2007-2022",
#   "2022 CTD files", "Laurentian", "LTER", "7-18-22", "M15.XMLCON"))
# 
# 
# noaaCTDdf %>% 
#   filter(hour(sampleDateTime) == 12, minute(sampleDateTime) == 0) %>%
#   distinct(sampleDateTime.ctd, sampleDateTime)
# 