library(devtools)
library(tidyverse)
library(oce)
load_all()

filepaths <- .getFilePaths()
noaaWQ <- filepaths["noaaWQ"]
noaaWQSites <- filepaths["noaaWQSites"]
namingFile <- filepaths["namingFile"]
noaaSites <- read_csv(filepaths["noaaWQSites"])
n_max <- Inf

noaaFiles <- read_csv("https://github.com/kvitense/GL_Data/raw/refs/heads/main/NOAA/ctdFileMetaData.csv") %>%
  # remove files where we don't know the station
  drop_na(SITE_ID) %>%
  select(cnvFiles, SITE_ID, sampleDateTime, stationDepth, Latitude, Longitude) %>%
  mutate(
    cnvFiles = file.path("~", "Environmental Protection Agency (EPA)",
      "Lake Michigan ML - General", "Raw_data", "NOAA", "CTD 2007-2022", cnvFiles))

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

noaaCTDdf <- noaaFiles %>%
  mutate(
    data = purrr::map(cnvFiles, \(x)
      # oce throws error if salinity can't be calculated
      tryCatch({
        .oce2df(suppressWarnings(oce::read.oce(x)), studyName = "NOAActd", bin = TRUE, downcast = TRUE) %>% 
          # take the station information froma the filepath
          rename(Latitude.ctd = Latitude, Longitude.ctd  = Longitude, sampleDateTime.ctd = sampleDateTime, stationDepth.ctd = stationDepth)},
        error = function(cond) {message(conditionMessage(cond))}
        ), .progress = TRUE)
  ) %>%
  tidyr::unnest(data) %>%
  # prioritize ctd machine information
  mutate(
    Latitude = coalesce(Latitude.ctd, Latitude),
    Longitude = coalesce(Longitude.ctd, Longitude),
    sampleDateTime = coalesce(sampleDateTime.ctd, sampleDateTime),
    stationDepth = coalesce(stationDepth.ctd, stationDepth),
    Study = "NOAActd",
    UID = paste0(SITE_ID, "_", lubridate::date(sampleDateTime))
    ) %>%
  select(-c(Latitude.ctd, Longitude.ctd, sampleDateTime.ctd, stationDepth.ctd)) %>%
  left_join(renamingTable, by = c("Study", "ANALYTE")) %>%
  rename(ReportedUnits = UNITS)  %>%
  mutate(ReportedUnits = tolower(ReportedUnits)) %>%
  left_join(key, by =  "CodeName") %>%
  left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
  mutate(
    RESULT = ifelse(!is.na(ConversionFactor), RESULT * ConversionFactor, RESULT)) %>%
  dplyr::select(Study, UID, SITE_ID, sampleDateTime, stationDepth, Latitude, Longitude, sampleDepth, CodeName, RESULT, Units = Explicit_Units, Category)
# [ ] Impute TOD if needed , also flag it

# Save the cleaned seabird Files
saveRDS(noaaCTDdf, "../GL_Data/NOAA/noaaCTD.Rds")

noaaCTDdf %>%
  ggplot(aes(x = date(sampleDateTime))) +
  geom_histogram()

read.oce(noaaFiles$cnvFiles[[7]])