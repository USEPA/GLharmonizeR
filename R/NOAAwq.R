
.loadNOAAwq <- function(noaaWQ, noaaWQ2, namingFile, noaaWQSites) {

  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor)) %>%
    unique() # Duplicate rows

  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NOAA_Map") %>%
    dplyr::select(Study, ANALYTE, CodeName, Units) # KV: Added Study

  # KV: Edited to save NOAA site info as object to be able to look at it, so it's not read in twice, and so it's not hidden in code
  noaa_site_info <- openxlsx::read.xlsx(noaaWQSites, sheet = "siteNameMapping")
    # dplyr::filter(Keep == "T")  %>%
  # [X] KV: Actually need to keep the Keep column joined in and then use that column to remove sites not in Lake Michigan
  # KV: Fixed errors in this file per Steve's comments


  # Supplemental NOAA data - contains SRP and SiO2
  newNOAA <- openxlsx::read.xlsx(noaaWQ2) %>%
    dplyr::mutate(time = "12:00") %>% # No times are included
    dplyr::mutate(
      QAcomment = "time imputed as noon",
    ) %>%
    tidyr::unite(sampleDateTime, Year, Month, Day, time, sep = "-", remove=FALSE) %>%
    dplyr::mutate(
      sampleDateTime = lubridate::ymd_hm(sampleDateTime),
      SITE_ID = tolower(Station)
    ) %>%
    dplyr::select(-c(DOY, Station)) %>%
    dplyr::rename(sampleDepth = Depth) %>%
    tidyr::pivot_longer(cols = SRP.ugl:SiO2.mgl, names_to = "ANALYTE", values_to = "RESULT") %>%
    tidyr::drop_na(RESULT) %>% # KV added to reduce data size in further manipulations
    tidyr::separate(ANALYTE, into = c("ANALYTE", "ReportedUnits"), sep = "\\.") %>%
    # [X] KV: Changed to ReportedUnits above to match code below
    # [X] KV: Need to further edit SITE_ID as in below in order for noaa_site_info to join correctly
    dplyr::mutate(
      SITE_ID = stringr::str_remove_all(tolower(SITE_ID), "[[:blank:]]"),
      SITE_ID = stringr::str_remove(SITE_ID, "leg"),
      SITE_ID = stringr::str_remove(SITE_ID, "#"),
    ) %>%
    dplyr::left_join(noaa_site_info, by = c("SITE_ID" = "Other.names")) %>%
    # SITE_ID.y is SITE_ID in noaa_site_info that all site names are mapped to, so keep this one
    dplyr::filter(Keep=="T") %>% # KV: Need to include this to remove midmusk site, etc.
    dplyr::select(-c(SITE_ID, Keep, Justification)) %>%
    dplyr::rename(SITE_ID = SITE_ID.y)

  # KV: Lots of details were missing in above code that are present in below code that did, in fact, need to be included.


  noaaWQdata <- openxlsx::read.xlsx(noaaWQ, sheet = "WQ 2007-2022") %>%
    dplyr::rename(SITE_ID = Station, sampleDepth = Depth.m) %>%
    dplyr::mutate(
      SITE_ID = stringr::str_remove_all(tolower(SITE_ID), "[[:blank:]]"),
      SITE_ID = stringr::str_remove(SITE_ID, "leg"),
      SITE_ID = stringr::str_remove(SITE_ID, "#"),
      # only useful notes are when reporting sampling time
      # [x] KV: Also need to specify eastern time zone? Here and elsewhere throughout data where appropriate. Otherwise, looks like UTC time zone is applied, which is not correct
      time = round(24 * as.numeric(notes)) - 1,
      # [x] KV: Round time above so that there aren't decimals. Or else, need to convert decimal to minutes
      QAcomment = ifelse(is.na(time), "time imputed as noon", NA),
      time = ifelse(is.na(time), "12", time)
    ) %>%
    dplyr::mutate(SITE_ID = ifelse(SITE_ID == "c1nobag", "c1", SITE_ID)) %>%
    dplyr::left_join(noaa_site_info, by = c("SITE_ID" = "Other.names")) %>%
    # SITE_ID.y is SITE_ID in noaa_site_info that all site names are mapped to, so keep this one
    dplyr::filter(Keep=="T") %>% # KV: Need to actually use Keep column to filter out sites we don't want - this was problem for newNOAA above
    dplyr::select(-c(SITE_ID, Keep, Justification)) %>%
    dplyr::rename(SITE_ID = SITE_ID.y) %>%
    tidyr::unite(sampleDateTime, Year, Month, Day, time, sep = "-", remove=FALSE) %>%
    # KV: Keeping extra date and time columns to check working correctly
    dplyr::mutate(
      sampleDateTime = lubridate::ymd_h(sampleDateTime),
    ) %>%
    # KV: secchi.m and PP.ugl are character - why?
    # KV: PP is character because has one "?", which is fine to have go to NA using as.numeric()
    # KV: secchi.m: the character values are CTB, plus one typo "7..5"
    # [X] KV: Fix 7..5 typo in secchi
    # [X] KV: Properly deal with secchi CTB - needs to be replaced with NA where has '(bottom)' in value,  flagged/noted in QAcode, and then a proper row for the study added to flagsMap.
    dplyr::mutate(
      QAcode = NA,
      Secchi.m = ifelse(Secchi.m=="7..5", "7.5", Secchi.m), # Fix typo for one secchi obs.
      QAcode = ifelse(grepl("bottom", Secchi.m), "CTB", QAcode),
      Secchi.m = as.numeric(Secchi.m), # induce NAs where includes "bottom"
      PP.ugl = as.numeric(PP.ugl), # induce NA for "?"
    ) %>%
    # KV commented this out - shouldn't be needed
    # dplyr::mutate(
    #   # fill in secchi for every sampling event and depth
    #   Secchi.m = mean(Secchi.m, na.rm = T),
    #   .by = c(sampleDateTime, SITE_ID),
    # ) %>%
    # [X] KV: Suggest making a QAcode column for adding CTB secchi flag in wide format, and then once pivoted to long, remove it for non-Secchi analytes
    tidyr::pivot_longer(cols = SurfaceTemp.C:N.mgl, names_to = "ANALYTE", values_to = "RESULT") %>%
    # Remove CTB in QAcomment if analyte is not Secchi
    dplyr::mutate(
      QAcode = ifelse(!ANALYTE=="Secchi.m", NA, QAcode)
    ) %>%
    # Remove NAs unless has CTB in QACode
    dplyr::filter(!is.na(RESULT) | !is.na(QAcode)) %>%
    dplyr::mutate(
      sampleDepth = ifelse(ANALYTE == "Secchi.m", NA, sampleDepth), # Remove sampleDepth for secchi
      sampleDepth = ifelse(ANALYTE == "SurfaceTemp.C", 0, sampleDepth) # Temp at very surface with handheld thermometer - ultimately removed via Analytes3
    ) %>%
    # Remove duplicates for when secchi and surface temp reported multiple times across depths
    dplyr::distinct() %>%
    tidyr::separate(ANALYTE, into = c("ANALYTE", "ReportedUnits"), sep = "\\.") %>%
    dplyr::select(-c(DOY, `CTD.#`, notes)) %>%

    # add in SRP and Silica
    dplyr::bind_rows(newNOAA) %>%

    dplyr::mutate(
      Study= "NOAA_WQ" # Modified to match Study name in Analytes3
    ) %>%
    dplyr::mutate(
      UID = paste(Study, SITE_ID, Year, Month, Day, sampleDepth, sep = "-")
      # [X] KV: UID needs to be dealt with after joining newNOAA
      # [X] KV: UID should probably be site ID, date, and sampleDepth
      # KV: Note that secchi and surface temp won't have matching UID because of sampleDepth
    ) %>%
    dplyr::left_join(renamingTable) %>%
    dplyr::filter(!grepl("remove", CodeName, ignore.case=T)) %>%
    dplyr::left_join(key, by = "CodeName") %>%
    # [X] KV: Did not join conversions table or do the conversions - and Part_C and Part_N both need to be converted, so the data are wrong
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::mutate(RESULT = ifelse(!is.na(ConversionFactor), RESULT * ConversionFactor, RESULT)) %>%
    # **** BE CAREFUL HERE IN CASE TARGET UNITS CHANGE FOR TP AND PP!!!! ****
    # add newly reported Mdls for TP and PP
    dplyr::mutate(
      mdl = dplyr::case_when(
        # Target units match what is desired already so keep as is for sake of time
        # these were given by Steve Pothoven
        CodeName == "Tot_P" ~ 0.2, # ug/L
        CodeName == "Part_P" ~ 0.05, # ug/L
        .default = NA
      )
    ) %>%
    # zeros are not non detects, they just weren't measured
    dplyr::filter(RESULT != 0 | !is.na(QAcode)) # Removes 3 in main file confirmed to be NA

  return(noaaWQdata)
}

# [X] KV: Need time flag added in flagsMap - put in QAcomment, blank for QAcode
# [X] KV: Need secchi CTB QAcode flag added in flagsMap - blank for QAcomment
# [X] KV: What about Steve's note about corrected values for Part_C on 9/6/2007 for Beta that were zero? - these are not included. Sent 12/13/2024 - edits_nutrients.xlsx
# [X] KV manually edited the main nutrient doc with the edits Steve sent for C and N


# file <- file.path(teamsFolder, "Raw_data", "NOAA", "CTD 2007-2022", "2022 CTD files", "6659", "Pothoven", "278-22", "raw", "SBE19plus_01906659_2022_10_12_0011.hex")
# file <- file.path(teamsFolder, "Raw_data", "NOAA", "CTD 2007-2022", "2007 ctd data", "2007", "079-07", "Laurent raw", "Omega0792007.CNV")
# test <- oce::read.oce(file)
