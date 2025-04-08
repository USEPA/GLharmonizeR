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
    dplyr::filter(Study == "NOAActd") %>%
    dplyr::distinct(Study, ANALYTE, CodeName)

  glenda <- readRDS(url(filepaths["seaBird"])) %>%
    dplyr::mutate(Study = "SeaBird") %>%
    dplyr::mutate(
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
    dplyr::filter(!grepl("CAST|DERIVE|(911)", STATION_ID, ignore.case = T))  %>%
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
    dplyr::mutate(RESULT = ifelse(!is.na(ConversionFactor), RESULT*ConversionFactor, RESULT)) %>% # mean(is.na(test$TargetUnits))
    dplyr::rename(SITE_ID = STATION_ID)

      

  noaa <- readRDS(url(filepaths["noaaCTD"])) %>%
  # prioritize ctd machine information
    dplyr::mutate(
      Latitude = dplyr::coalesce(Latitude, Latitude.ctd),
      Longitude = dplyr::coalesce(Longitude, Longitude.ctd),
      sampleDateTime = dplyr::coalesce(sampleDateTime.ctd, sampleDateTime),
      stationDepth = dplyr::coalesce(stationDepth.ctd, stationDepth),
      Study = "NOAActd",
      UID = paste0(SITE_ID, "_", lubridate::date(sampleDateTime))
    ) %>%
    dplyr::select(-dplyr::ends_with(".ctd")) %>%
    # [x] KV: In the remaining steps below, ensure you are CAREFULLY checking that each join is working as expected using the tests that were previously outlined.
    # [x] KV: Check that analyte names in NOAA_Map have proper case to match the data and are joining correctly
    dplyr::left_join(NOAArenamingTable, by = c("Study", "ANALYTE")) %>% # mean(is.na(test$CodeName))
    # [x] KV: Need to include code here to remove any analytes mapped to 'Remove'. This should be done to remove par after adding it to NOAA_Map, per comment below. Remember, this is all dynamic and subject to change and needs to be robust to changes
    dplyr::filter(!grepl("remove", CodeName, ignore.case=  T)) %>%
    dplyr::rename(ReportedUnits = UNITS)  %>%
    dplyr::mutate(ReportedUnits = tolower(ReportedUnits)) %>%
    # [x] KV: Check that ReportedUnits are all formatted correctly for appropriate joining of the conversions table
    dplyr::left_join(key, by =  "CodeName") %>% # mean(is.na(test$TargetUnits)) : 0 
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::mutate(
      RESULT = ifelse(!is.na(ConversionFactor), RESULT * ConversionFactor, RESULT)) %>%
    # [x] KV: Suggest NOT doing the below selection of column names, which is inconsistent with how the other data functions are written, and you've renamed some of these variables inappropriately (e.g., Excplit_Units should stay as Explicit_Units for consistency with the rest of the data).
    # [x] KV: Check that you can just comment this out and that there wasn't another reason you did this selection (e.g., conflicting variable names). If there's no compelling reason you did the selection, remove the select() function below and the appropriate columns will ultimately be retained in the joinFullData function.
    #dplyr::select(Study, UID, SITE_ID, sampleDateTime, stationDepth, Latitude, Longitude, sampleDepth, CodeName, RESULT, Units = Explicit_Units, Category, ANALYTE_Orig_Name = ANALYTE, ConversionFactor, LongName)
    # [x] KV: The above removal of par should be done in NOAA_Map for consistency and documentation, NOT hard coded here
    # a small number of dates are a century old
    # [ ] KV: This date issue probably needs to be investigated further - needs to be examined and potentially is a question for Steve Pothoven at NOAA
    dplyr::filter(sampleDateTime > lubridate::ymd("1950-01-01")) %>% # KV: Also move this filter to the new function so that the date issue can be investigated
    dplyr::mutate(
      Explicit_Units= ifelse(CodeName == "pH", "unitless", Explicit_Units),
    )

  ctd <- dplyr::bind_rows(glenda, noaa) %>%
    # [x] KV: Also the mutate() line editing Explicit_Units should be in the new GLNPO Seabird CTD function you will create, not added here.
    dplyr::mutate( 
      # This is mostly intended to fill in missing values for seabird
      QAcomment = dplyr::case_when(
        is.na(stationDepth) & (sum(!is.na(stationDepth)) > 0) ~ "Station depth imputed from another site visit",
        is.na(stationDepth) & (sum(!is.na(sampleDepth)) > 0) ~ "Station depth imputed from maximum sample depth",
        .default = NA),
      QAcode = dplyr::case_when(
        is.na(stationDepth) & (sum(!is.na(stationDepth)) > 0) ~ "D",
        is.na(stationDepth) & (sum(!is.na(sampleDepth)) > 0) ~ "D",
        .default = NA),
      stationDepth = dplyr::case_when(
        is.na(stationDepth) & (sum(!is.na(stationDepth)) > 0) ~ mean(stationDepth, na.rm=TRUE),
        is.na(stationDepth) & (sum(!is.na(sampleDepth)) > 0) ~ max(sampleDepth, na.rm = TRUE),
        .default = stationDepth),
      .by = SITE_ID
    ) %>%
    dplyr::mutate(
      sampleDate = lubridate::date(sampleDateTime),
      sampleTimeUTC = lubridate::hour(sampleDateTime)
    )
  return(ctd)
}

# round(colMeans(is.na(ctd)), 2)
#      sampleDepth         Latitude        Longitude          SITE_ID 
#             0.00             0.00             0.00             0.00
#   sampleDateTime     stationDepth          ANALYTE           RESULT
#             0.00             0.00             0.00             0.01
#    ReportedUnits              UID            Study         CodeName 
#             0.50             0.00             0.00             0.00
#         LongName      TargetUnits         Category   Explicit_Units
#             0.00             0.00             0.00             0.00
# ConversionFactor      Lepak.input         ctdFiles       STATION_ID
#             0.69             0.99             0.50             0.50
#        QAcomment           QAcode       sampleDate       sampleTime 
#             0.50             0.50             0.00             0.00
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