#' Load and join data for CSMI 2021 from csv excel files
#'
#' @description
#' `.readCleanCSMI2021` returns a dataframe of all of the joined water quality data relating to CSMI 2021
#'
#' @details
#m' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param csmi2021 a string specifying the directory to CSMI 2021 data
#' @return dataframe of the fully joined water quality data from CSMI 2021
.readCleanCSMI2021 <- function(csmi2021, namingFile) {
  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)
  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))
  
  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "CSMI_Map", na.strings = c("", "NA")) %>%
      dplyr::mutate(ANALYTE = stringr::str_remove_all(ANALYTE, "\\."))
  mdls <- file.path(csmi2021, "Chem2021_detection%20limits.xlsx") %>%
    # The detection limit file contains MDLs and the values used to impute results <MDL.
    openxlsx::read.xlsx(sheet = "detection limits", rows = 1:3) %>%
    dplyr::select(15:28) %>%
    dplyr::slice(1) %>%
    tidyr::pivot_longer(dplyr::everything(), values_to = "mdl", names_to = "ANALYTE") %>%
    dplyr::mutate(
      ANALYTE = stringr::str_extract(ANALYTE, "^[:alnum:]*"),
      ANALYTE = ifelse(ANALYTE == "chl", "Chla", ANALYTE)
      ) %>%
    dplyr::left_join(renamingTable) %>%
    dplyr::left_join(key) %>%
    dplyr::rename(ReportedUnits = Units) %>%
    dplyr::left_join(conversions) %>%
    dplyr::mutate(mdl = ifelse(!is.na(ConversionFactor), mdl * ConversionFactor, mdl)) %>%
    dplyr::select(ANALYTE, UNITS = TargetUnits, mdl) %>%
    dplyr::distinct()
  # CTD
  # \Lake Michigan ML - General\Raw_data\CSMI\2021\2020 LM CSMI LEII CTD combined_Fluoro_LISST_12.13.21.xlsx
  # Contact is James Gerads


  ## bin averaged over 1 meter depth intervals
  ## -9.99E-29 is NA
  ## There are already processed, formatted ready to use files Should we use that?
  ##
  


  CTD <- file.path(csmi2021, "2020%20LM%20CSMI%20LEII%20CTD%20combined_Fluoro_LISST_12.13.21.xlsx") %>%
    openxlsx::read.xlsx(
      sheet = "Lake Michigan 2020 CSMI Data", startRow = 2, na.strings = c("", "-9.99e-29"),
      check.names = TRUE
    ) %>%
    dplyr::rename(Site = X2, sampleDateTime = X3) %>%
    dplyr::mutate(
      sampleDateTime = as.POSIXct(sampleDateTime * 86400, origin = "1900-01-01", tz = "UTC"),
      sampleDateTime = lubridate::ymd_h(paste(sampleDateTime, "12"))) %>%
    # don't select bio samples, scans
    dplyr::select(2:5, 7,9,14, 21, 22, 23) %>%
    dplyr::mutate(
      Study = "CSMI_2021_CTD",
      # add UID before pivoting
      UID = paste0(Study, "-", 1:nrow(.))
    ) %>%
    tidyr::pivot_longer(
      cols = c(4:8),
      names_to = "ANALYTE",
      values_to = "RESULT"
    ) %>%
    dplyr::rename(
      sampleDepth = Depth..fresh.water..m.,
      Latitude = Latitude..deg.,
      Longitude = Longitude..deg.,
      SITE_ID = Site
    ) %>%
    # [x] Need to include pH 
    # [x] Don't rename, instead think about a regex pivot to also grab the units
    tidyr::separate_wider_regex(ANALYTE, patterns = c("ANALYTE" = ".*", "\\.\\.", "UNITS" = ".*\\..*"), too_few = "align_start") %>%
    dplyr::mutate(
      UNITS = stringr::str_remove_all(UNITS, "[^%|^[:alnum:]]"),
      UNITS = ifelse(grepl("^CPAR", ANALYTE, ignore.case=FALSE), "percent", UNITS), 
      ANALYTE = stringr::str_remove_all(ANALYTE, "\\.")
    )


  WQ <- file.path(csmi2021, "Chem2021_FinalShare.xlsx") %>% 
    openxlsx::read.xlsx(sheet = "DetLimitCorr") %>%
    dplyr::select(-30) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("L"), ~ as.numeric(.))) %>%
    tidyr::separate_wider_regex(`Time.(EST)`, patterns = c("time" = ".*", "tz" = "\\(.*\\)"), too_few = "align_start") %>%
    # [x] parse the time column along with date
    dplyr::mutate(
      Date = as.POSIXct(Date * 86400, origin = "1899-12-30", tz = "UTC"),
      # [x] flag it if we need to assume it's noon
      QAcomment = ifelse(grepl("no time", time, ignore.case = T) | is.na(time), "Assumed sample at noon", NA),
      time = ifelse(grepl("no time", time, ignore.case = T) | is.na(time), "12:00", time),
      time = stringr::str_remove_all(time, "[:space:]"),
      # one date is reported funkily so we report the average
      time = ifelse(time == "3:45/4:29", "4:07", time)
    ) %>%
    tidyr::unite("sampleDateTime", Date, time, sep = " ") %>%
    dplyr::mutate(sampleDateTime = lubridate::ymd_hm(sampleDateTime)) %>%
    dplyr::rename(stationDepth = `Site.Depth.(m)`, sampleDepth = `Separate.depths.(m)`) %>%
    dplyr::select(-c(
      Month, Ship, `Research.Project`, `Integrated.depths.(m)`, `DCL?`, `Stratified/.Unstratified?`,
      Station, tz
    )) %>%
    dplyr::mutate(Study = "CSMI_2021_WQ", UID = paste0(Study, "-", `STIS#`)) %>%
    tidyr::pivot_longer(-c(Study, UID, `STIS#`, Site, sampleDateTime, stationDepth, sampleDepth, QAcomment, Lake), names_to = "ANALYTE", values_to = "RESULT") %>%
    # NA's and non-reports are the only NA's in this dataset
    tidyr::drop_na(RESULT) %>%
    dplyr::mutate(
      ANALYTE = stringr::str_remove_all(ANALYTE, "[-|\\+|=]"),
    ) %>%
    tidyr::separate_wider_regex(ANALYTE, patterns = c("ANALYTE" = "^[:alpha:]*", "\\.", ".g.*L$" ), too_few = "align_start") %>%
    # figured out parsing before joining with CTD is WAAAAAAY easier
    dplyr::rename(SITE_ID = Site) %>%
    dplyr::bind_rows(., CTD) %>%
    dplyr::left_join(renamingTable) %>%
    dplyr::mutate(
      Year = 2021,
    ) %>%
    # water chem contains station depth, ctd contains lat lon, so for those sites that
    # have both types of measurement taking the group mean of those values will simply
    # replace the na values
    dplyr::mutate(
      # [x] repalce CTD station depth with Wchem station depth by taking gruoped mean
      # [x] Then do max depth if there are still missing - flag it if this needs to be done
      Latitude = ifelse(is.na(Latitude), mean(Latitude, na.rm = T), Latitude),
      Longitude = ifelse(is.na(Longitude), mean(Longitude, na.rm = T), Longitude),
      QAcomment = ifelse(is.na(stationDepth), paste(QAcomment, "station Depth estimated as the maximum sample Depth"), QAcomment),
      stationDepth = ifelse(is.na(stationDepth), max(sampleDepth, na.rm = T), stationDepth),
      .by = SITE_ID
    ) %>%
    dplyr::mutate(
      SITE_ID = stringr::str_remove_all(SITE_ID, "_"),
      ANALYTE = stringr::str_extract(ANALYTE, "^[:alpha:]*")
    )

  # grab additional site data from zooplankton files
  zooPlank <- file.path(csmi2021, "LakeMichigan_CSMI_2021_Zooplankton_Taxonomy_Densities.csv") %>%
    readr::read_csv(show_col_types = FALSE) %>%
    dplyr::rename(SITE_ID = TRANSECT) %>%
    dplyr::reframe(
      Latitude2 = mean(Latitude, na.rm = T), Longitude2 = mean(Longitude, na.rm = T),
      .by = SITE_ID
    ) %>%
    dplyr::mutate(SITE_ID = tolower(SITE_ID))

  # After adding site info from zooplank, missing lat/lons is 2%
  WQ <- WQ %>%
    dplyr::mutate(
      SITE_ID = tolower(SITE_ID),
      SITE_ID = stringr::str_replace(SITE_ID, "^pwa", "pw"),
      SITE_ID = stringr::str_replace(SITE_ID, "^lvd", "lud")
    ) %>%
    dplyr::left_join(zooPlank, by = "SITE_ID") %>%
    dplyr::mutate(
      Longitude = dplyr::coalesce(Longitude, Longitude2),
      Latitude = dplyr::coalesce(Latitude, Latitude2)
    ) %>%
    dplyr::select(-c(Latitude2, Longitude2)) %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    dplyr::left_join(key) %>%
    dplyr::left_join(conversions) %>%
    dplyr::mutate(RESULT = ifelse(!is.na(ConversionFactor), RESULT * ConversionFactor, RESULT)) %>%
    dplyr::left_join(mdls)

  # return the joined data
  return(WQ)
}


# [x] What % of chem have ctd and vice versa
# NOT a great number. Check out code bleow
# WQ %>%
#  reframe(
#   CTD = sum(Study == "CSMI_2021_CTD"),
#   WQ = sum(Study == "CSMI_2021_WQ"),
#   .by = c(SITE_ID)
#   ) %>%
#   mutate(CTD = CTD > 1, WQ = WQ > 1) %>%
#   count(CTD, WQ)


# Appears there are no chl-a measurements for the Gaurdian data,
# but USGS collected chl-a data at some of the same sites within a week or so. Need to confirm with Ryan/Aabir.
