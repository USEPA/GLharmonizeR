# Harmonize and join NOAA and GLNPO CTD

.cleanGLNPOSeabirdCTD <- function(){
  filepaths <- .getFilePaths()

  # KV: Per comment below, any code in a script that uses the below Analytes3 tables needs to be a core package function that can accommodate changes to these tables
  key <- openxlsx::read.xlsx(filepaths["namingFile"], sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(filepaths["namingFile"], sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor)) %>%
    dplyr::distinct() # Duplicate rows

  seaBirdrenamingTable <- openxlsx::read.xlsx(filepaths["namingFile"], sheet = "SeaBird_Map", na.strings = c("", "NA"))


  glnpo_seabird <- readRDS(url(filepaths["seaBird"])) %>%
    dplyr::mutate(Study = "SeaBird") %>%
    dplyr::mutate(
      STATION_ID = basename(STATION_ID),
      BASE=STATION_ID,
      STATION_ID = stringr::str_remove_all(STATION_ID, stringr::fixed(".cnv", ignore_case = TRUE) ),
      STATION_ID = stringr::str_remove_all(STATION_ID, stringr::fixed(".bin", ignore_case = TRUE)),
      STATION_ID = stringr::str_split_i(STATION_ID, "_", i=2),
      STATION_ID = stringr::str_split_i(STATION_ID, " ", i=1),

      UID = paste0(STATION_ID, "_", lubridate::date(sampleDateTime), "_", sampleDepth),
      UNITS = dplyr::case_when(
        ANALYTE == "cpar" ~ "percent",
        ANALYTE == "pH" ~ "unitless",
        .default = UNITS
      )
    ) %>%
    # Only keep first casts (remove CAST, comma with number, or 'B' if there's also an 'A')
   dplyr::filter(!grepl("CAST", STATION_ID, ignore.case = T))  %>%
   dplyr::filter(!grepl(",", STATION_ID, ignore.case = T))  %>%
    # Note there are some valid sites that end in B that should be kept (above all end in MB and are second casts):
    #  "MI30B" "MI31B" "MI42B"     "MI46B"    "MI48B"     "MI49B"
    # "MI50B"     "MI51B"     "MI52B"     "MI53B"
    dplyr::filter(!STATION_ID %in% c("MI18MB", "MI27M-B", "MI27MB", "MI41M-B", "MI41MB")) %>%
    # Remove A from: "MI18MA", "MI27M-A",  "MI27MA", "MI41M-A" , "MI41MA"
    dplyr::mutate(
      STATION_ID = dplyr::case_when(
        STATION_ID == "MI18MA" ~ "MI18M",
        STATION_ID == "MI27M-A" ~ "MI27M",
        STATION_ID == "MI27MA" ~ "MI27M",
        STATION_ID == "MI41M-A" ~ "MI41M",
        STATION_ID == "MI41MA" ~ "MI41M",
        .default = STATION_ID
      )) %>%
    # What does DERIVED mean in station ame? Unclear but seems to be redundant so removing.
    dplyr::filter(!grepl("DERIVE", STATION_ID, ignore.case = T))  %>%
    # drop NA values
    tidyr::drop_na(RESULT) %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    dplyr::left_join(seaBirdrenamingTable, by = c("Study", "ANALYTE")) %>% # sum(is.na(glnpo_seabird$CodeName)) : 0
    dplyr::filter(!grepl("remove", CodeName, ignore.case = T)) %>%
    dplyr::mutate(
      ReportedUnits = tolower(ReportedUnits),
      ReportedUnits = stringr::str_remove_all(ReportedUnits, "/")
    ) %>%
    dplyr::left_join(key, by = "CodeName") %>% # sum(is.na(glnpo_seabird$TargetUnits)) : 0
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::mutate(RESULT = ifelse(!is.na(ConversionFactor), RESULT*ConversionFactor, RESULT)) %>%
    # Check: if Reported units != Target units, then !is.na(ConversionFactor)
    dplyr::rename(SITE_ID = STATION_ID)


  # Good to here

  glnpo_seabird <- glnpo_seabird %>%
    # [x] KV: Also the mutate() line editing Explicit_Units should be in the new GLNPO Seabird CTD function you will create, not added here.
    # [ ] KV: Was above comment for Seabird or NOAA? Looks like it was added to NOAA instead


    ##### KV: This is not done correctly below and will need to be done in the joinFullData.R code for GLNPO and NOAA separately. GLNPO can be filled in from GLENDA station depth
    # Could calculate max depth here for each and label as such. Then use in the full join function to prioritize which depth to use across both seabird and glenda databases

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
      sampleTimeUTC = lubridate::hour(sampleDateTime), # Is Time zone dealt with here? How???
      UID = paste0(Study, UID, sep = "-")
    )
  return(glnpo_seabird)
}





.cleanNOAACTD <- function(){

  filepaths <- .getFilePaths()

  # KV: Per comment below, any code in a script that uses the below Analytes3 tables needs to be a core package function that can accommodate changes to these tables
  key <- openxlsx::read.xlsx(filepaths["namingFile"], sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(filepaths["namingFile"], sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor)) %>%
    dplyr::distinct() # Duplicate rows

  NOAArenamingTable <- openxlsx::read.xlsx(filepaths["namingFile"], sheet = "NOAA_Map") %>%
    dplyr::filter(Study == "NOAActd") %>%
    dplyr::distinct(Study, ANALYTE, CodeName)


  noaa <- readRDS(url(filepaths["noaaCTD"])) %>%
    # prioritize ctd machine information
    dplyr::mutate(
      Latitude = dplyr::coalesce(Latitude, Latitude.ctd),
      Longitude = dplyr::coalesce(Longitude, Longitude.ctd),
      sampleDateTime = dplyr::coalesce(sampleDateTime.ctd, sampleDateTime),
      stationDepth = dplyr::coalesce(stationDepth.ctd, stationDepth),
      Study = "NOAActd",
      UID = paste0(SITE_ID, "_", lubridate::date(sampleDateTime), "_", sampleDepth)
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
    # a small number of dates are a century old
    # [ ] KV: This date issue probably needs to be investigated further - needs to be examined and potentially is a question for Steve Pothoven at NOAA
    dplyr::filter(sampleDateTime > lubridate::ymd("1950-01-01")) %>% # KV: Also move this filter to the new function so that the date issue can be investigated
    dplyr::mutate(
      Explicit_Units= ifelse(CodeName == "pH", "unitless", Explicit_Units),
    )



  ####### I don't think the below code is probably needed for NOAA except for time handling - I don't think anything is imputed from NOAA CTD - should all be filled in from the water chem sites depths. ######

  noaa <- noaa %>%
    # [x] KV: Also the mutate() line editing Explicit_Units should be in the new GLNPO Seabird CTD function you will create, not added here.
    # [ ] KV: Was above comment for Seabird or NOAA? Looks like it was added to NOAA instead


    ##### KV: This is not done correctly below and will need to be done in the joinFullData.R code for GLNPO and NOAA separately. GLNPO can be filled in from GLENDA station depth
    # Could calculate max depth here for each and label as such. Then use in the full join function to prioritize which depth to use across both seabird and glenda databases

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
      sampleTimeUTC = lubridate::hour(sampleDateTime),
      UID = paste0(Study, UID, sep = "-")
    )

  return(noaa)

}
