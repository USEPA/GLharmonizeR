# SECOND/FINAL NOAA CTD PROCESSING FILE TO RUN

# [ ] KV: Please add comments at the top of this document describing what this script does and the order in which the NOAA processing files are run. I noted that this appears to be the final NOAA CTD processing file.

# [ ] *** Note that KV has commented on the below code but hasn't run any of it yet. Code will need to be re-checked after below issues are addressed ***



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

noaaFiles <- readRDS(url("https://github.com/USEPA/GL_Data/raw/refs/heads/main/NOAA/ctdFileMetaData.Rds", "rb")) %>% # KV: This file is output from NOAActdNameParsing.R
  # remove files where we don't know the station
  drop_na(SITE_ID) %>%
  select(ctdFiles, SITE_ID, sampleDateTime, stationDepth, Latitude, Longitude) %>%
  mutate(
    ctdFiles = file.path("/Users", "ccoffman", "Environmental Protection Agency (EPA)",
      "Lake Michigan ML - General", "Raw_data", "NOAA", "CTD 2007-2022", ctdFiles))
# [ ] KV: Not a high priority, but note that the CTD file paths here are just synced files from Sharepoint and could probably be read in directly from Sharepoint, rather than having a user-specific path here. These paths can be found if you go to the files on Teams and hit 'Copy link'. https://usepa.sharepoint.com/:f:/r/sites/LakeMichiganML/Shared%20Documents/General/Raw_data/NOAA/CTD%202007-2022?csf=1&web=1&e=WmEi8R

# KV: Per comment below, any code in a script that uses the below Analytes3 tables needs to be a core package function that can accommodate changes to these tables
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
# [ ] Make sure years make sense
noaaCTDdf <- noaaFiles %>%
  mutate(
    data = purrr::map(ctdFiles, \(x)
      tryCatch({
      .oce2df(suppressWarnings(oce::read.oce(x)), studyName = "NOAActd", bin = TRUE, downcast = TRUE) %>%
        # Note: The above .oce2df() function is from ctdProcessing.R

        # take the station information froma the filepath
        dplyr::rename(Latitude.ctd = Latitude, Longitude.ctd  = Longitude, sampleDateTime.ctd = sampleDateTime, stationDepth.ctd = stationDepth)},
      error = function (e) {
        message(paste("Error reading file", basename(x)))
        NULL
      }),
    .progress = TRUE)
  ) %>%
  tidyr::unnest(cols = data) %>%

  # [ ] KV: Stop this function here and write out noaaCTD.Rds data (see note below)

  # [ ] KV: Any parts of processing scripts that use the naming/conversions file (Analytes3) need to be moved to its own package function so that any updates to Analytes3 results in the data being updated dynamically when the core package functions are run. As it stands now, these CTD files and names are static and are inconsistent with the rest of the package and the idea behind the tables in Analytes3

  ########## Everything below here needs to be moved to a NEW NOAA CTD package function that reads in noaaCTD.Rds and does the remaining mapping and harmonization ########################

  # prioritize ctd machine information
  mutate(
    Latitude = coalesce(Latitude, Latitude.ctd),
    Longitude = coalesce(Longitude, Longitude.ctd),
    sampleDateTime = coalesce(sampleDateTime.ctd, sampleDateTime),
    stationDepth = coalesce(stationDepth.ctd, stationDepth),
    Study = "NOAActd",
    UID = paste0(SITE_ID, "_", lubridate::date(sampleDateTime))
    ) %>%
  # [ ] KV: In the remaining steps below, ensure you are CAREFULLY checking that each join is working as expected using the tests that were previously outlined.
  # [ ] KV: Check that analyte names in NOAA_Map have proper case to match the data and are joining correctly
  left_join(renamingTable, by = c("Study", "ANALYTE")) %>%
  # [ ] KV: Need to include code here to remove any analytes mapped to 'Remove'. This should be done to remove par after adding it to NOAA_Map, per comment below. Remember, this is all dynamic and subject to change and needs to be robust to changes
  dplyr::rename(ReportedUnits = UNITS)  %>%
  mutate(ReportedUnits = tolower(ReportedUnits)) %>%
  # [ ] KV: Check that ReportedUnits are all formatted correctly for appropriate joining of the conversions table
  left_join(key, by =  "CodeName") %>%
  left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
  mutate(
    RESULT = ifelse(!is.na(ConversionFactor), RESULT * ConversionFactor, RESULT)) %>%
  # [ ] KV: Suggest NOT doing the below selection of column names, which is inconsistent with how the other data functions are written, and you've renamed some of these variables inappropriately (e.g., Excplit_Units should stay as Explicit_Units for consistency with the rest of the data).
  # [ ] KV: Check that you can just comment this out and that there wasn't another reason you did this selection (e.g., conflicting variable names). If there's no compelling reason you did the selection, remove the select() function below and the appropriate columns will ultimately be retained in the joinFullData function.
  dplyr::select(Study, UID, SITE_ID, sampleDateTime, stationDepth, Latitude, Longitude, sampleDepth, CodeName, RESULT, Units = Explicit_Units, Category, ANALYTE_Orig_Name = ANALYTE, ConversionFactor, LongName) %>%
  filter(ANALYTE_Orig_Name != "par")
  # [ ] KV: The above removal of par should be done in NOAA_Map for consistency and documentation, NOT hard coded here



# Save the seabird Files
noaaCTDdf %>%
  # a small number of dates are a century old
  # [ ] KV: This date issue probably needs to be investigated further - needs to be examined and potentially is a question for Steve Pothoven at NOAA
  filter(sampleDateTime > lubridate::ymd("1950-01-01")) %>% # KV: Also move this filter to the new function so that the date issue can be investigated

  ############# End of code that needs to be moved to a NEW NOAA CTD package function for appropriate mapping, etc. ################################

  saveRDS("../GL_Data/NOAA/noaaCTD.Rds")

# noaaCTDdf %>%
#   ggplot(aes(x = sampleDateTime)) +
#   geom_histogram()

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
