.loadNOAAwq <- function(noaaWQ, noaaWQ2, namingFile, noaaWQSites) {
  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))
  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NOAA_Map") %>%
    dplyr::select(ANALYTE, CodeName, Units)

  newNOAA <- openxlsx::read.xlsx(noaaWQ2) %>%
    dplyr::mutate(time = "12:00") %>%
    tidyr::unite(sampleDateTime, Year, Month, Day, time, sep = "-") %>%
    dplyr::mutate(
      sampleDateTime = lubridate::ymd_hm(sampleDateTime),
      SITE_ID = tolower(Station)
    ) %>%
    dplyr::select(-c(DOY, Station)) %>%
    dplyr::rename(sampleDepth = Depth) %>%
    tidyr::pivot_longer(cols = SRP.ugl:SiO2.mgl, names_to = "ANALYTE", values_to = "RESULT") %>%
    tidyr::separate(ANALYTE, into = c("ANALYTE", "UNITS"), sep = "\\.") %>%
    dplyr::left_join(
      openxlsx::read.xlsx(noaaWQSites, sheet = "siteNameMapping") %>% dplyr::filter(Keep == "T"),
      by = c("SITE_ID" = "Other.names"))

  noaaWQdata <- openxlsx::read.xlsx(noaaWQ, sheet = "WQ 2007-2022") %>%
    dplyr::rename(SITE_ID = Station, sampleDepth = Depth.m) %>%
    dplyr::mutate(
      SITE_ID = stringr::str_remove_all(tolower(SITE_ID), "[[:blank:]]"),
      SITE_ID = stringr::str_remove(SITE_ID, "leg"),
      SITE_ID = stringr::str_remove(SITE_ID, "#"),
      # only useful notes are when reporting sampling time
      time = 24 * as.numeric(notes),
      time = ifelse(is.na(time), "12", time)
    ) %>% 
    dplyr::mutate(SITE_ID = ifelse(SITE_ID == "c1nobag", "c1", SITE_ID)) %>%
    dplyr::left_join(
      openxlsx::read.xlsx(noaaWQSites, sheet = "siteNameMapping") %>% dplyr::filter(Keep == "T"),
      by = c("SITE_ID" = "Other.names")) %>%
    dplyr::select(-SITE_ID) %>%
    dplyr::rename(SITE_ID = SITE_ID.y) %>%
    dplyr::mutate(
      dplyr::across(SurfaceTemp.C:N.mgl, as.numeric),
      # assuming 12 noon for consistency with other datasets
      QAcomment = "time imputed as noon",
      Study= "NOAAwq",
      UID = paste(Study, sep = "-", dplyr::row_number())
    ) %>%
    tidyr::unite(sampleDateTime, Year, Month, Day, time, sep = "-") %>%
    dplyr::mutate(
      sampleDateTime = lubridate::ymd_h(sampleDateTime),
    ) %>%
    dplyr::mutate(
      # fill in secchi for every sampling event and depth
      secchi.m = mean(secchi.m, na.rm = T),
      .by = c(sampleDateTime, SITE_ID),
    ) %>%
    tidyr::pivot_longer(cols = SurfaceTemp.C:N.mgl, names_to = "ANALYTE", values_to = "RESULT") %>%
    # add in SRP and Silica
    dplyr::bind_rows(newNOAA) %>%
    # NA's and non-reports are the only two source of NAs in this dataset (not detection limit issues)
    tidyr::drop_na(RESULT) %>%
    tidyr::separate(ANALYTE, into = c("ANALYTE", "ReportedUnits"), sep = "\\.") %>%
    # the keep and justification are for the Sites, they do not pertain to the QA comments
    dplyr::select(-c(DOY, `CTD.#`, Keep, Justification)) %>%
    # convert lat lon
    dplyr::mutate(
      sampleDepth = ifelse(ANALYTE == "Surface.Temp", 1, sampleDepth),
      sampleDepth = ifelse(ANALYTE == "secchi", NA, sampleDepth)
    ) %>%
    dplyr::mutate(ANALYTE = tolower(ANALYTE)) %>%
    #dplyr::left_join(noaaWQunits, by = "ANALYTE") %>%
    # rename and convert units
    dplyr::left_join(renamingTable, by = "ANALYTE") %>%
    dplyr::left_join(key, by = "CodeName") %>%
    dplyr::mutate(ReportedUnits = ifelse(ANALYTE =="tp", "ugl", ReportedUnits)) %>%
    dplyr::mutate(Study = "NOAAwq") %>%
    dplyr::filter(!grepl("remove", CodeName, ignore.case=T)) %>%
    # zeros are not non detects, they just weren't measured
    dplyr::filter(RESULT != 0) %>%
    # add newly reported Mdls for TP and PP
    dplyr::mutate(
      MDL = dplyr::case_when(
        # units match what is desired already 
        # these were given by Steve Pothoven
        CodeName == "Tot_P" ~ 0.2,
        CodeName == "Part_P" ~ 0.05,
        .default = NA
      )
    )
  return(noaaWQdata)
}

# file <- file.path(teamsFolder, "Raw_data", "NOAA", "CTD 2007-2022", "2022 CTD files", "6659", "Pothoven", "278-22", "raw", "SBE19plus_01906659_2022_10_12_0011.hex")
# file <- file.path(teamsFolder, "Raw_data", "NOAA", "CTD 2007-2022", "2007 ctd data", "2007", "079-07", "Laurent raw", "Omega0792007.CNV")
# test <- oce::read.oce(file)
