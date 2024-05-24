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
.LoadCSMI2021 <- function(csmi2021, n_max = Inf){
  # CTD
  # \Lake Michigan ML - General\Raw_data\CSMI\2021\2020 LM CSMI LEII CTD combined_Fluoro_LISST_12.13.21.xlsx
  # Contact is James Gerads

  ## bin averaged over 1 meter depth intervals
  ## -9.99E-29 is NA
  ## There are already processed, formatted ready to use files Should we use that?
  ## 
  # [x] Don't rename, instead think about a regex pivot to also grab the units
  # If I do the pivot make sure to add the UID first 
  # [x] Need to include pH
  CTD <- file.path(csmi2021, "2020 LM CSMI LEII CTD combined_Fluoro_LISST_12.13.21.xlsx") %>%
    readxl::read_xlsx(sheet = "Lake Michigan 2020 CSMI Data", skip = 1, na = c("", -9.99e-29, n_max = n_max),
      .name_repair = "unique_quiet") %>% 
    dplyr::rename(Site = ...2, sampleDate = ...3) %>%
    # don't select bio samples, scans
    dplyr::select(2:23) %>%
    dplyr::mutate(
      Study = "CSMI_2021_CTD",
      UID = paste0(Study, "-", 1:nrow(.))
    ) %>%
    tidyr::pivot_longer(cols = c(4:20),
                 names_to = "ANALYTE",
                 values_to = "VALUE") %>%
    dplyr::rename(sampleDepth = `Depth [fresh water, m]`, Latitude = `Latitude [deg]`,
      Longitude = `Longitude [deg]`, SITE_ID = Site) %>%
    tidyr::separate_wider_regex(ANALYTE, patterns = c("ANALYTE" = ".*", " ", "UNITS" = "\\[.*\\]"), too_few = "align_start")  %>%
    dplyr::mutate(
      UNITS = stringr::str_remove_all(UNITS, "[^%|^[:alnum:]]"),
    )

  # Water chemistry  copied from 
  # L:\Priv\Great lakes Coastal\2021 CSMI Lake Michigan\Data\Water chem
  # Contact is Annie Fosso
  DL <- file.path(csmi2021, "Chem2021_detection limits.xlsx") %>%
    # The detection limit file contains MDLs and the values used to impute results <MDL.
    readxl::read_xlsx(sheet = "detection limits", .name_repair = "unique_quiet") %>%
    dplyr::select(23:38) %>%
    dplyr::mutate(Limit = dplyr::coalesce(...23, ...24)) %>%
    dplyr::select(-c(...23, ...24)) %>%
    tidyr::pivot_longer(-Limit, values_to = "RESULT", names_to = "ANALYTE") %>%
    tidyr::pivot_wider(id_cols = ANALYTE, names_from = Limit, values_from = RESULT, values_fn = mean) %>%
    dplyr::select(-`NA`)

  WQ <- file.path(csmi2021, "Chem2021_FinalShare.xlsx") %>%
    readxl::read_xlsx(sheet = "DetLimitCorr", .name_repair = "unique_quiet") %>%
    dplyr::select(-dplyr::contains("...")) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("L"), ~ as.numeric(.))) %>%
    # tidyr::pivot_longer(15:29, names_to = "ANALYTE", values_to = "RESULT") %>%
    dplyr::mutate(      # Haven't figured out how to parse these times, can come back to it if it's important 
      sampleDate= lubridate::date(Date)
    ) %>%
    dplyr::rename(stationDepth = `Site Depth (m)`,  sampleDepth = `Separate depths (m)`) %>%
      dplyr::select( -c(Month, Ship, Lake, `Research Project`, `Integrated depths (m)`, `DCL?`, `Stratified/ Unstratified?`,
                    `Time (EST)`, Station, Date )) %>%
    tidyr::pivot_longer(-c(1:4, sampleDate), names_to = "ANALYTE", values_to = "RESULT") %>%
    # figured out parsing before joining with CTD is WAAAAAAY easier
    tidyr::separate_wider_regex(ANALYTE, c(ANALYTE = "[:graph:]*", "[:space:]*", UNITS= ".*$")) %>%
  #  dplyr::left_join(latlons, by = "Site") %>%
    dplyr::mutate(Study = "CSMI_2021_WQ", UID = paste0(Study, 1:nrow(.))) %>%
    dplyr::rename(SITE_ID = Site) %>%


    dplyr::bind_rows(., CTD) %>% 
  #  dplyr::left_join(latlons, by = "Site") %>%
  #  dplyr::rename(Latitude = Latitude.x, Longitude = Longitude.x) %>%
  #  dplyr::select(-c(Latitude.y, Longitude.y)) %>%
    dplyr::left_join(DL, by = "ANALYTE") %>%
    dplyr::rename(mdl = `method detection limit`) %>%
    dplyr::select(-contains("detection limit corrected")) %>%
    dplyr::mutate(QA_CODE = dplyr::case_when(
      # If a value is equal to 1/2 the respective MDL, either replace it with NA or flag as nondetect with imputed value (or whatever you need to do to ensure consistency across datasets)
      RESULT < mdl ~ "nondetect"
    )) %>%
    dplyr::mutate(
      Year = 2021,
    ) %>%
    # water chem contains station depth, ctd contains lat lon, so for those sites that 
    # have both types of measurement taking the group mean of those values will simply
    # replace the na values
    dplyr::mutate(
      # [x] repalce CTD station depth with Wchem station depth by taking gruoped mean
      # [x] Then do max depth if there are still missing - flag it if this needs to be done
      Latitude = ifelse(is.na(Latitude), mean(Latitude, na.rm= T), Latitude),
      Longitude = ifelse(is.na(Longitude), mean(Longitude, na.rm= T), Longitude),
      stationDepth = ifelse(is.na(stationDepth), mean(stationDepth, na.rm= T), stationDepth),
      stationDepth = ifelse(is.na(stationDepth), max(sampleDepth, na.rm= T), stationDepth),
      .by = SITE_ID
    ) %>%
    dplyr::mutate(
      SITE_ID = stringr::str_remove_all(SITE_ID, "_")
    )
    dplyr::filter(
      ! ANALYTE %in% c("Fluorescence", "Conductivity", "Beam Attenuation", "Beam Transmission", "SPAR,", "PAR/Irradiance," "Density", "Time, Elapsed", "Pressure, Digiquartz", "Bottles", 
      "Altimeter", "Descent Rate")
    )
  # return the joined data
  return(WQ)
}


    # [x] What % of chem have ctd and vice versa 
    # NOT a great number. Check out code bleow
    WQ %>% 
     reframe(
      CTD = sum(Study == "CSMI_2021_CTD") > 1,
      WQ = sum(Study == "CSMI_2021_WQ") > 1,
      .by = c(SITE_ID, sampleDate)
      )  %>%
      count(CTD, WQ)


# Appears there are no chl-a measurements for the Gaurdian data, but USGS collected chl-a data at some of the same sites within a week or so. Need to confirm with Ryan/Aabir.
# A few Lat-longs are missing but probably can be found in profile data below TRUE
# But also, the lat/lons are incredibly low precision
