# Harmonize and join NOAA and GLENDA CTD

.cleanNOAAnSeabirdCTD <- function(){
  filepaths <- .getFilePaths()

  # KV: Per comment below, any code in a script that uses the below Analytes3 tables needs to be a core package function that can accommodate changes to these tables
  key <- openxlsx::read.xlsx(filepaths["namingFile"], sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(filepaths["namingFile"], sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))

  seaBirdrenamingTable <- openxlsx::read.xlsx(filepaths["namingFile"], sheet = "SeaBird_Map", na.strings = c("", "NA"))

  NOAArenamingTable <- openxlsx::read.xlsx(filepaths["namingFile"], sheet = "NOAA_Map") %>%
    filter(Study == "NOAActd") %>%
    distinct(Study, ANALYTE, CodeName)

  glenda <- readRDS(url(filepaths["seaBird"])) %>%
    dplyr::mutate(Study = "SeaBird") %>%
    mutate(
      STATION_ID = tolower(basename(STATION_ID)),
      STATION_ID = stringr::str_remove_all(STATION_ID, "[.cnv|.bin]"),
      STATION_ID = gsub("^\\d+", "", STATION_ID),
      UID = paste0(STATION_ID, "_", lubridate::date(sampleDateTime)),
      UNITS = dplyr::case_when(
        ANALYTE == "cpar" ~ "percent",
        ANALYTE == "pH" ~ "unitless"
      )
    ) %>%
    # Catch all for 2, 3, and fourth casts, not sure what Derive or (911) are 
    filter(!grepl("CAST|DERIVE|(911)", STATION_ID, ignore.case = T))  %>%
    # dropped without a flag
    tidyr::drop_na(RESULT) %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    # [x] KV: In the remaining steps below, ensure you are CAREFULLY checking that each join is working as expected using the tests that were previously outlined.
    dplyr::left_join(seaBirdrenamingTable, by = c("Study", "ANALYTE")) %>% # mean(is.na(test$CodeName)) : 0
    dplyr::filter(!grepl("remove", CodeName, ignore.case = T)) %>%
    dplyr::mutate(
      ReportedUnits = tolower(ReportedUnits),
      ReportedUnits = stringr::str_remove_all(ReportedUnits, "/")
    ) %>%
    # [x] KV: Check that ReportedUnits are all formatted correctly for appropriate joining of the conversions table
    dplyr::left_join(key, by = "CodeName") %>% # mean(is.na(test$TargetUnits)) : 0
    dplyr::mutate(TargetUnits = tolower(TargetUnits)) %>%
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    # [x] Need to include code to actually do the conversions, even if there weren't any when you originally ran this. Everything in Analytes3 is subject to change and the code needs to be robust to changes
    dplyr::mutate(RESULT = ifelse(!is.na(ConversionFactor), RESULT*ConversionFactor, RESULT)) # mean(is.na(test$TargetUnits))

      

  noaa <- readRDS(url(filepaths["noaaCTD"])) %>%
  # prioritize ctd machine information
    mutate(
      Latitude = coalesce(Latitude, Latitude.ctd),
      Longitude = coalesce(Longitude, Longitude.ctd),
      sampleDateTime = coalesce(sampleDateTime.ctd, sampleDateTime),
      stationDepth = coalesce(stationDepth.ctd, stationDepth),
      Study = "NOAActd",
      UID = paste0(SITE_ID, "_", lubridate::date(sampleDateTime))
    ) %>%
    # [x] KV: In the remaining steps below, ensure you are CAREFULLY checking that each join is working as expected using the tests that were previously outlined.
    # [x] KV: Check that analyte names in NOAA_Map have proper case to match the data and are joining correctly
    left_join(NOAArenamingTable, by = c("Study", "ANALYTE")) %>% # mean(is.na(test$CodeName))
    # [x] KV: Need to include code here to remove any analytes mapped to 'Remove'. This should be done to remove par after adding it to NOAA_Map, per comment below. Remember, this is all dynamic and subject to change and needs to be robust to changes
    dplyr::filter(!grepl("remove", CodeName, ignore.case=  T)) %>%
    dplyr::rename(ReportedUnits = UNITS)  %>%
    mutate(ReportedUnits = tolower(ReportedUnits)) %>%
    # [x] KV: Check that ReportedUnits are all formatted correctly for appropriate joining of the conversions table
    left_join(key, by =  "CodeName") %>% # mean(is.na(test$TargetUnits)) : 0 
    left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    mutate(
      RESULT = ifelse(!is.na(ConversionFactor), RESULT * ConversionFactor, RESULT)) %>%
    # [x] KV: Suggest NOT doing the below selection of column names, which is inconsistent with how the other data functions are written, and you've renamed some of these variables inappropriately (e.g., Excplit_Units should stay as Explicit_Units for consistency with the rest of the data).
    # [ ] KV: Check that you can just comment this out and that there wasn't another reason you did this selection (e.g., conflicting variable names). If there's no compelling reason you did the selection, remove the select() function below and the appropriate columns will ultimately be retained in the joinFullData function.
    #dplyr::select(Study, UID, SITE_ID, sampleDateTime, stationDepth, Latitude, Longitude, sampleDepth, CodeName, RESULT, Units = Explicit_Units, Category, ANALYTE_Orig_Name = ANALYTE, ConversionFactor, LongName)
    # [x] KV: The above removal of par should be done in NOAA_Map for consistency and documentation, NOT hard coded here
    # a small number of dates are a century old
    # [ ] KV: This date issue probably needs to be investigated further - needs to be examined and potentially is a question for Steve Pothoven at NOAA
    filter(sampleDateTime > lubridate::ymd("1950-01-01")) %>% # KV: Also move this filter to the new function so that the date issue can be investigated
    dplyr::mutate(Explicit_Units= ifelse(CodeName == "pH", "unitless", Explicit_Units))
  # [x] KV: Also the mutate() line editing Explicit_Units should be in the new GLNPO Seabird CTD function you will create, not added here.

  ctd <- dplyr::bind_rows(glenda, noaa)
  return(ctd)
}

# round(colMeans(is.na(ctd), 2)
#        sampleDepth           Latitude          Longitude         STATION_ID 
#               0.00               0.00               0.00               0.00
#     sampleDateTime       stationDepth            ANALYTE             RESULT
#               0.00               0.50               0.00               0.01 
#      ReportedUnits                UID              Study           CodeName
#               0.50               0.00               0.00               0.00
#           LongName        TargetUnits           Category     Explicit_Units
#               0.00               0.00               0.00               0.00
#   ConversionFactor        Lepak.input           ctdFiles            SITE_ID
#               0.69               0.99               0.50               0.50 
#       Latitude.ctd      Longitude.ctd sampleDateTime.ctd   stationDepth.ctd
#               0.95               0.95               0.50               1.00
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

# Look at seabird
# glenda %>% 
#   distinct(STATION_ID) %>%
#   print(n=399)
# glenda %>% 
#   distinct(CodeName)