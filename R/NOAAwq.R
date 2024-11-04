.loadNOAAwq <- function(noaaWQ, namingFile, noaaWQSites) {
  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))
  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NOAA_Map") %>%
    select(ANALYTE, CodeName, Units)
  

  noaaWQunits <- openxlsx::read.xlsx(noaaWQ, sheet = "WQ 2007-2022", rows = 2:3, check.names = T) %>%
    dplyr::select(-c(1:5)) %>%
    t() %>%
    as.data.frame() %>%
    dplyr::rename(ANALYTE = `1`) %>%
    dplyr::mutate(
      Units = rownames(.),
      ANALYTE = tolower(ANALYTE),
      Units = stringr::str_remove_all(Units, "\\.*"),
      Units = stringr::str_remove_all(Units, "[:digit:]")
    ) %>%
    dplyr::filter(
      !grepl("NA", ANALYTE),
      ANALYTE != "depth",
    ) %>%
    dplyr::mutate(
      Units = stringr::str_remove(Units, "\\/")
    )

  noaaWQdata <- openxlsx::read.xlsx(noaaWQ, sheet = "WQ 2007-2022", startRow = 3) %>%
    dplyr::rename(SITE_ID = Station, sampleDepth = Depth) %>%
    dplyr::mutate(
      SITE_ID = stringr::str_remove_all(tolower(SITE_ID), "[[:blank:]]"),
      SITE_ID = stringr::str_remove(SITE_ID, "leg"),
      SITE_ID = stringr::str_remove(SITE_ID, "#")
    ) %>% 
    dplyr::mutate(SITE_ID = ifelse(SITE_ID == "c1nobag", "c1", SITE_ID)) %>%
    dplyr::left_join(readr::read_csv(noaaWQSites), by = c("SITE_ID" = "Other.names")) %>%
    dplyr::select(-SITE_ID) %>%
    dplyr::rename(SITE_ID = SITE_ID.y) %>%
    dplyr::mutate(
      dplyr::across(Surface.Temp:N, as.numeric),
      # assuming 12 noon for consistency with other datasets
      Time = "12:00",
      QAcomment = "time imputed as noon",
      Study= "NOAAwq",
      UID = paste(Study, sep = "-", dplyr::row_number())
    ) %>%
    tidyr::unite(sampleDateTime, Year, Month, Day, Time, sep = "-") %>%
    dplyr::mutate(
      sampleDateTime = lubridate::ymd_hm(sampleDateTime),
    ) %>%
    dplyr::mutate(
      # fill in secchi for every sampling event and depth
      secchi = mean(secchi, na.rm = T),
      .by = c(sampleDateTime, SITE_ID),
    ) %>%
    tidyr::pivot_longer(cols = Surface.Temp:N, names_to = "ANALYTE", values_to = "RESULT") %>%
    # NA's and non-reports are the only two source of NAs in this dataset (not detection limit issues)
    tidyr::drop_na(RESULT) %>%
    dplyr::select(-c(X14, X15), DOY) %>%
    # convert lat lon
    dplyr::mutate(
      sampleDepth = ifelse(ANALYTE == "Surface.Temp", 0, sampleDepth),
      sampleDepth = ifelse(ANALYTE == "secchi", NA, sampleDepth)
    ) %>%
    dplyr::mutate(ANALYTE = tolower(ANALYTE)) %>%
    dplyr::left_join(noaaWQunits, by = "ANALYTE") %>%
    # rename and convert units
    dplyr::rename(ReportedUnits = Units) %>%
    dplyr::left_join(renamingTable, by = "ANALYTE") %>%
    dplyr::left_join(key, by = "CodeName") %>%
    dplyr::mutate(ReportedUnits = ifelse(ANALYTE =="tp", "ugl", ReportedUnits)) %>%
    dplyr::mutate(Study = "NOAAwq") %>%
    dplyr::filter(!grepl("remove", CodeName, ignore.case=T))
  return(noaaWQdata)
}

# file <- file.path(teamsFolder, "Raw_data", "NOAA", "CTD 2007-2022", "2022 CTD files", "6659", "Pothoven", "278-22", "raw", "SBE19plus_01906659_2022_10_12_0011.hex")
# file <- file.path(teamsFolder, "Raw_data", "NOAA", "CTD 2007-2022", "2007 ctd data", "2007", "079-07", "Laurent raw", "Omega0792007.CNV")
# test <- oce::read.oce(file)
