#' Load and join data for CSMI 2021 from csv excel files
#'
#' @description
#' `.LoadCSMI2021` returns a dataframe of all of the joined water quality data relating to CSMI 2021
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param csmi2021 a string specifying the directory to CSMI 2021 data
#' @return dataframe of the fully joined water quality data from CSMI 2021
.LoadCSMI2021 <- function(csmi2021) {
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
    dplyr::mutate(sampleDateTime = lubridate::ymd_h(paste(lubridate::date(sampleDateTime), "12"))) %>%
    # don't select bio samples, scans
    dplyr::select(2:23) %>%
    dplyr::mutate(
      Study = "CSMI_2021_CTD",
      # add UID before pivoting
      UID = paste0(Study, "-", 1:nrow(.))
    ) %>%
    tidyr::pivot_longer(
      cols = c(4:20),
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
    tidyr::separate_wider_regex(ANALYTE, patterns = c("ANALYTE" = ".*", " ", "UNITS" = "\\[.*\\]"), too_few = "align_start") %>%
    dplyr::mutate(
      UNITS = stringr::str_remove_all(UNITS, "[^%|^[:alnum:]]"),
    )

  # Water chemistry  copied from
  # L:\Priv\Great lakes Coastal\2021 CSMI Lake Michigan\Data\Water chem
  # Contact is Annie Fosso
  DL <- file.path(csmi2021, "Chem2021_detection%20limits.xlsx") %>%
    # The detection limit file contains MDLs and the values used to impute results <MDL.
    openxlsx::read.xlsx(sheet = "detection limits", rows = 1:3) %>%
    dplyr::select(16:28) %>%
    dplyr::slice(2) %>%
    tidyr::pivot_longer(everything(), values_to = "mdl", names_to = "ANALYTE")

  WQ <- file.path(csmi2021, "Chem2021_FinalShare.xlsx") %>%
    openxlsx::read.xlsx(sheet = "DetLimitCorr") %>%
    dplyr::select(-30) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("L"), ~ as.numeric(.))) %>%
    # tidyr::pivot_longer(15:29, names_to = "ANALYTE", values_to = "RESULT") %>%
    # [x] parse the time column along with date
    # [x] flag it if we need to assume it's noon
    # split time and tz, infer timezones as needed, rejoin, convert to UTC
    tidyr::separate_wider_regex(`Time.(EST)`, patterns = c("time" = ".*", "tz" = "\\(.*\\)"), too_few = "align_start") %>%
    dplyr::mutate(
      time = ifelse(grepl("no time", time, ignore.case = T) | is.na(time), "12:00", time),
      QAcomment = ifelse(grepl("no time", time, ignore.case = T) | is.na(time), "Assumed sample at noon", NA),
      time = stringr::str_remove_all(time, "[:space:]")
    ) %>%
    # Deal with times separated by /
    tidyr::separate_wider_regex(time, patterns = c("time1" = ".*", "/", "time2" = ".*"), too_few = "align_start", cols_remove = TRUE) %>%
    # convert times to decimals to average them
    dplyr::mutate(
      time1 = ifelse(
        grepl(":", time1),
        format(as.POSIXct(paste(Sys.Date(), time1)), "%d"),
        time1
      ),
      time2 = ifelse(
        grepl(":", time2),
        format(as.POSIXct(paste(Sys.Date(), time2)), "%d"),
        time2
      ),
      time1 = as.numeric(time1),
      time2 = as.numeric(time2),
      time = dplyr::case_when(
        (!is.na(time1)) & (!is.na(time2)) ~ (time1 + time2) / 2,
        is.na(time1) & is.na(time2) ~ 0.5,
        (!is.na(time1)) & is.na(time2) ~ time1,
        (!is.na(time2)) & is.na(time1) ~ time2
      ),
      time = time - round(time),
      time = format(as.POSIXct(Sys.Date() + time), "%H:%m"),
      # Make sure dec times are less that 1
      # XXX some of the 15:00 times were converted to 5 as decimals
      # infer tz
      tz = ifelse(is.na(tz), "EST", tz),
      tz = sub("\\(", "", tz),
      tz = sub("\\)", "", tz),
      Date = lubridate::date(Date)
    ) %>%
    tidyr::unite("sampleDateTime", Date, time, sep = " ") %>%
    # XXX Gave up on tz's for now being within 1 hour is close enough
    dplyr::mutate(
      sampleDateTime = lubridate::ymd_hm(sampleDateTime)
    ) %>%
    dplyr::rename(stationDepth = `Site.Depth.(m)`, sampleDepth = `Separate.depths.(m)`) %>%
    dplyr::select(-c(
      Month, Ship, `Research.Project`, `Integrated.depths.(m)`, `DCL?`, `Stratified/.Unstratified?`,
      Station, tz, time1, time2
    )) %>%
    dplyr::mutate(Study = "CSMI_2021_WQ", UID = paste0(Study, 1:nrow(.))) %>%
    tidyr::pivot_longer(-c(Study, UID, `STIS#`, Site, sampleDateTime, stationDepth, sampleDepth, QAcomment, Lake), names_to = "ANALYTE", values_to = "RESULT") %>%
    # figured out parsing before joining with CTD is WAAAAAAY easier
    tidyr::separate_wider_regex(ANALYTE, c(ANALYTE = "[:graph:]*", "[:space:]*", UNITS = ".*$")) %>%
    dplyr::rename(SITE_ID = Site) %>%
    dplyr::bind_rows(., CTD) %>%
    dplyr::left_join(DL, by = "ANALYTE") %>%
    dplyr::mutate(
      # [x] Flag if below detection limit
      QA_CODE = dplyr::case_when(
        # Remember these cases are evaluated in order
        is.na(mdl) ~ NA,
        # If a value is equal to 1/2 the respective MDL, either replace it with NA or flag as nondetect with imputed value (or whatever you need to do to ensure consistency across datasets)
        RESULT < mdl ~ "Below detection limit"
      ),
      .default = NA,
      RESULT = dplyr::case_when(
        is.na(mdl) ~ RESULT,
        # If a value is equal to 1/2 the respective MDL, either replace it with NA or flag as nondetect with imputed value (or whatever you need to do to ensure consistency across datasets)
        RESULT < mdl ~ NA,
        .default = NA
      )
    ) %>%
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
      stationDepth = ifelse(is.na(stationDepth), mean(stationDepth, na.rm = T), stationDepth),
      stationDepth = ifelse(is.na(stationDepth), max(sampleDepth, na.rm = T), stationDepth),
      .by = SITE_ID
    ) %>%
    dplyr::mutate(
      SITE_ID = stringr::str_remove_all(SITE_ID, "_"),
      ANALYTE = stringr::str_remove_all(ANALYTE, "[^[:alpha:]]")
    ) %>%
    # remove unusable measures
    dplyr::filter(
      !ANALYTE %in% c(
        "Fluorescence", "Conductivity", "BeamAttenuation", "BeamTransmission", "SPAR", "PARIrradiance", "Density", "TimeElapsed", "PressureDigiquartz", "Bottles",
        "Altimeter", "DescentRate"
      )
    )

  # grab additional site data from zooplankton files
  zooPlank <- file.path(csmi2021, "LakeMichigan_CSMI_2021_Zooplankton_Taxonomy_Densities.csv") %>%
    readr::read_csv(show_col_types = FALSE) %>%
    dplyr::rename(SITE_ID = TRANSECT) %>%
    dplyr::reframe(
      Latitude2 = mean(Latitude, na.rm = T), Longitude2 = mean(Longitude, na.rm = T),
      .by = SITE_ID
    )

  # After adding site info from zooplank, missing lat/lons is 2%
  WQ <- WQ %>%
    dplyr::left_join(zooPlank, by = "SITE_ID") %>%
    dplyr::mutate(
      Longitude = dplyr::coalesce(Longitude, Longitude2),
      Latitude = dplyr::coalesce(Latitude, Latitude2)
    ) %>%
    dplyr::select(-c(Latitude2, Longitude2))

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
