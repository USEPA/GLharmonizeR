
.loadNOAAwq <- function(noaaWQ, noaaWQ2, namingFile, noaaWQSites) {

  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor)) %>%
    dplyr::distinct() # Duplicate rows

  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NOAA_Map") %>%
    dplyr::select(Study, ANALYTE, CodeName)
    # dplyr::select(-Units) # Should remove Units from these renamingTables so they don't cause confusion with the units parsed/read from the data. Units in renaming tables are prone to human error.

  # NOAA site info
  noaa_site_info <- openxlsx::read.xlsx(noaaWQSites, sheet = "siteNameMapping")
  # Need to keep the 'Keep' column joined in and then use that column to remove sites not in Lake Michigan


  # Supplemental NOAA data - contains SRP and SiO2
  newNOAA <- openxlsx::read.xlsx(noaaWQ2) %>%
    # Note these supplemental data don't have times
    tidyr::unite(sampleDate, Year, Month, Day, sep = "-", remove=FALSE) %>%
    dplyr::mutate(
      sampleDate = lubridate::ymd(sampleDate),
      SITE_ID = tolower(Station)
    ) %>%
    dplyr::select(-c(DOY, Station)) %>%
    dplyr::rename(sampleDepth = Depth) %>%
    tidyr::pivot_longer(cols = SRP.ugl:SiO2.mgl, names_to = "ANALYTE", values_to = "RESULT") %>%
    tidyr::drop_na(RESULT) %>% # reduce data size in further manipulations
    tidyr::separate(ANALYTE, into = c("ANALYTE", "ReportedUnits"), sep = "\\.") %>%
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



  noaaWQdata <- openxlsx::read.xlsx(noaaWQ, sheet = "WQ 2007-2022") %>%
    dplyr::rename(SITE_ID = Station, sampleDepth = Depth.m) %>%
    # Fix bad time
    dplyr::mutate(
      notes = ifelse(notes=="10.050000000000001", "0.41875", notes),
    ) %>%
    dplyr::mutate(
      SITE_ID = stringr::str_remove_all(tolower(SITE_ID), "[[:blank:]]"),
      SITE_ID = stringr::str_remove(SITE_ID, "leg"),
      SITE_ID = stringr::str_remove(SITE_ID, "#"),
      # use notes where reporting sampling time is reported
      time = floor(24 * as.numeric(notes))
    ) %>%
    # *** What are the time zones in the notes column? ***
    # If these times are CTD times, then they are UTC - per below, they don't necessarily match up. The times seem like they could be local time, but 8 hr ahead doesn't match up
    # ***Just remove the times because unclear***
    # file <- "C:/Users/KVITENSE/Environmental Protection Agency (EPA)/Lake Michigan ML - General/Raw_data/NOAA/CTD 2007-2022/2019 CTD files/Laurentian/Pothoven_2019/5_21_19/cnv/Omega.cnv"
    # 13:25. but time in notes is 21:24 (8 hr ahead of UTC)
    # file <- "C:/Users/KVITENSE/Environmental Protection Agency (EPA)/Lake Michigan ML - General/Raw_data/NOAA/CTD 2007-2022/2019 CTD files/Laurentian/Pothoven_2019/6_11_19/cnv/M15.cnv"
    # 21:17; time in notes is 17:17 (4 hr behind UTC) - this is EDT
    # file <- "C:/Users/KVITENSE/Environmental Protection Agency (EPA)/Lake Michigan ML - General/Raw_data/NOAA/CTD 2007-2022/2019 CTD files/Laurentian/Pothoven_2019/7_8_19/cnv/M15.cnv"
    # 15:52 ; time in notes is 10:25 (5 hr behind UTC) - EST?
    # look <- .oce2df(suppressWarnings(oce::read.oce(file, requireSalinity=FALSE)),  bin = TRUE, downcast = TRUE)

    # tidyr::unite(sampleDateTime, Year, Month, Day, time, sep = "-", remove=FALSE) %>%
    # dplyr::mutate(
    #   sampleDateTime = lubridate::ymd(sampleDateTime),
    #   sampleDate = lubridate::date(sampleDateTime),
    #   sampleTimeUTC = lubridate::hour(sampleDateTime) # Not correct time zone?
    # ) %>%
    tidyr::unite(sampleDate, Year, Month, Day, sep = "-", remove=FALSE) %>%
    dplyr::mutate(
      sampleDate = lubridate::ymd(sampleDate),
    ) %>%
    dplyr::mutate(SITE_ID = ifelse(SITE_ID == "c1nobag", "c1", SITE_ID)) %>%
    dplyr::left_join(noaa_site_info, by = c("SITE_ID" = "Other.names")) %>%
    # SITE_ID.y is SITE_ID in noaa_site_info that all site names are mapped to, so keep this one
    dplyr::filter(Keep=="T") %>% # Need to  use Keep column to filter out sites we don't want
    dplyr::select(-c(SITE_ID, Keep, Justification)) %>%
    dplyr::rename(SITE_ID = SITE_ID.y) %>%

    # PP is character because has one "?", which is fine to have go to NA using as.numeric()
    # secchi.m: the character values are CTB, plus one typo "7..5"
    # Make a QAcode and QAcomment column for adding CTB secchi flag in wide format, and then once pivoted to long, remove it for non-Secchi analytes
    dplyr::mutate(
      QAcode = NA,
      QAcomment = NA,
      Secchi.m = ifelse(Secchi.m=="7..5", "7.5", Secchi.m), # Fix typo for one secchi obs.
      QAcode = ifelse(grepl("bottom", Secchi.m), "CTB", QAcode),
      QAcomment = ifelse(grepl("bottom", Secchi.m), "Secchi clear to bottom", QAcomment),
      Secchi.m = as.numeric(Secchi.m), # induce NAs where includes "bottom"
      PP.ugl = as.numeric(PP.ugl), # induce NA for "?"
    ) %>%
    tidyr::pivot_longer(cols = SurfaceTemp.C:N.mgl, names_to = "ANALYTE", values_to = "RESULT") %>%
    # Remove flags in QAcode and QAcomment if analyte is not Secchi
    dplyr::mutate(
      QAcode = ifelse(!ANALYTE=="Secchi.m", NA, QAcode),
      QAcomment = ifelse(!ANALYTE=="Secchi.m", NA, QAcomment)
    ) %>%
    # Remove NAs unless is not NA in QACode
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
      # KV: Note that secchi and surface temp won't have matching UID because of sampleDepth
    ) %>%
    dplyr::left_join(renamingTable) %>%
    dplyr::filter(!grepl("remove", CodeName, ignore.case=T)) %>%
    dplyr::left_join(key, by = "CodeName") %>%
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

