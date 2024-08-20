noaaReadClean <- function(noaaWQ, namingFile) {
  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))
  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NOAA_Map")

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


  noaaWQsites <- openxlsx::read.xlsx(noaaWQ, sheet = "sites") %>%
    dplyr::rename(SITE_ID = `Station:`, Latitude = Lat, Longitude = Long, stationDepth = `Depth.(m)`)

  noaaWQdata <- openxlsx::read.xlsx(noaaWQ, sheet = "WQ 2007-2022", startRow = 3) %>%
    dplyr::rename(SITE_ID = Station, sampleDepth = Depth) %>%
    dplyr::left_join(noaaWQsites, by = "SITE_ID") %>%
    dplyr::mutate(
      dplyr::across(Surface.Temp:N, as.numeric),
      # assuming 12 noon for consistency with other datasets
      Time = "12:00",
      UID = paste("NOAAwq", sep = "-", dplyr::row_number())
    ) %>%
    tidyr::unite(sampleDateTime, Year, Month, Day, Time, sep = "-") %>%
    dplyr::mutate(
      sampleDateTime = lubridate::ymd_hm(sampleDateTime),
    ) %>%
    dplyr::mutate(
      # fill in secchi for every sampling event and depth
      secchi = mean(secchi, na.rm = T),
      surfaceTemp = mean(Surface.Temp, na.rm = T),
      .by = c(sampleDateTime, SITE_ID),
    ) %>%
    tidyr::pivot_longer(cols = Surface.Temp:N, names_to = "ANALYTE", values_to = "RESULT") %>%
    dplyr::select(-c(X14, X15), DOY) %>%
    # convert lat lon
    tidyr::separate(col = Latitude, into = c("Latdeg", "Latmin"), sep = " ") %>%
    tidyr::separate(col = Longitude, into = c("Londeg", "Lonmin"), sep = " ") %>%
    dplyr::mutate(
      dplyr::across(c(Latdeg, Latmin), as.numeric),
      dplyr::across(c(Londeg, Lonmin), as.numeric),
      Latitude = Latdeg + Latmin / 60,
      Longitude = Londeg + Lonmin / 60,
      sampleDepth = ifelse(ANALYTE == "Surface.Temp", 0, sampleDepth),
      sampleDepth = ifelse(ANALYTE == "secchi", NA, sampleDepth)
    ) %>%
    dplyr::select(-c(Latdeg, Latmin, Londeg, Lonmin)) %>%
    dplyr::mutate(ANALYTE = tolower(ANALYTE)) %>%
    dplyr::left_join(noaaWQunits, by = "ANALYTE") %>%
    # rename and convert units
    dplyr::rename(ReportedUnits = Units) %>%
    dplyr::left_join(renamingTable, by = "ANALYTE") %>%
    dplyr::left_join(key, by = "CodeName") %>%
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::mutate(RESULT = ifelse(ReportedUnits == TargetUnits, RESULT, RESULT * ConversionFactor)) %>%
    dplyr::select(-c(
      `RL.Agree?`, `Original.comment/observation`, `Resolution.Comment`, Finalized,
      TargetUnits, Category, ConversionFactor, Lepak.input, X5
    ))

  return(noaaWQdata)
}

# file <- file.path(teamsFolder, "Raw_data", "NOAA", "CTD 2007-2022", "2022 CTD files", "6659", "Pothoven", "278-22", "raw", "SBE19plus_01906659_2022_10_12_0011.hex")
# file <- file.path(teamsFolder, "Raw_data", "NOAA", "CTD 2007-2022", "2007 ctd data", "2007", "079-07", "Laurent raw", "Omega0792007.CNV")
# test <- oce::read.oce(file)
