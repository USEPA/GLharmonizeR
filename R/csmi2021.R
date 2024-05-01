#' Load and join data for CSMI 2021 from csv excel files 
#'
#' @description
#' `.LoadCSMI2021` returns a dataframe of all of the joined water quality data relating to CSMI 2021
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the filepath of the access database
#' @return dataframe of the fully joined water quality data from CSMI 2021
.LoadCSMI2021 <- function(directoryPath){
  # CTD
  # \Lake Michigan ML - General\Raw_data\CSMI\2021\2020 LM CSMI LEII CTD combined_Fluoro_LISST_12.13.21.xlsx
  # Contact is James Gerads

  ## bin averaged over 1 meter depth intervals
  ## -9.99E-29 is NA
  ## There are already processed, formatted ready to use files Should we use that?
  ## 
  CTD <- file.path(directoryPath, "2020 LM CSMI LEII CTD combined_Fluoro_LISST_12.13.21.xlsx") %>%
    readxl::read_xlsx(sheet = "Lake Michigan 2020 CSMI Data", skip = 1, na = c("", -9.99e-29),
    .name_repair = "unique_quiet") %>% 
    dplyr::rename(Transect = ...1, Site = ...2, sampleDate = ...3,
                  Latitude = `Latitude [deg]`,Longitude = `Longitude [deg]` ) %>%
    # depth to nearest meter
    dplyr::mutate(
      dplyr::across(dplyr::contains("Depth"), round),
      ) %>% 
    dplyr::select(c(Transect:`CPAR/Corrected Irradiance [%]`, pH:Longitude, 26:32, 38:43)) %>%
    dplyr::rename(sampleDepth = `Depth [fresh water, m]`) %>%
    tidyr::pivot_longer(
      -c(Transect, Site, sampleDate, sampleDepth, Latitude, Longitude),
      names_to = "ANALYTE", values_to = "RESULT") %>%
    dplyr::mutate(
      UNITS = stringr::str_extract(ANALYTE, "\\[.*\\]"),
      ANALYTE = stringr::str_remove(ANALYTE, "\\s\\[.*\\]"),
      UNITS = stringr::str_remove(UNITS, "\\["),
      UNITS = stringr::str_remove(UNITS, "\\]"),
      Site = stringr::str_remove(Site, "_")
    )


  latlons <- CTD %>% 
    dplyr::reframe(Latitude = mean(Latitude, na.rm = TRUE), Longitude = mean(Longitude, na.rm = TRUE), .by = Site)

# Water chemistry  copied from 
# L:\Priv\Great lakes Coastal\2021 CSMI Lake Michigan\Data\Water chem
# Contact is Annie Fosso
  DL <- file.path(directoryPath, "Chem2021_detection limits.xlsx") %>%
    # The detection limit file contains MDLs and the values used to impute results <MDL.
    readxl::read_xlsx(sheet = "detection limits", .name_repair = "unique_quiet") %>%
    dplyr::select(23:38) %>%
    dplyr::mutate(Limit = dplyr::coalesce(...23, ...24)) %>%
    dplyr::select(-c(...23, ...24)) %>%
    tidyr::pivot_longer(-Limit, values_to = "RESULT", names_to = "ANALYTE") %>%
    tidyr::pivot_wider(id_cols = ANALYTE, names_from = Limit, values_from = RESULT, values_fn = mean) %>%
    dplyr::select(-`NA`) 

  WQ <- file.path(directoryPath, "Chem2021_FinalShare.xlsx") %>%
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
    dplyr::left_join(latlons, by = "Site")

  # Add station depths
  latlons <- latlons %>% 
    dplyr::left_join(WQ, by = c("Site", "Longitude", "Latitude")) %>%
    dplyr::select(Site, Latitude, Longitude, stationDepth) %>%
    dplyr::mutate(stationDepth = ifelse(is.na(stationDepth), mean(stationDepth, na.rm=T), stationDepth), .by = Site) %>%
    dplyr::arrange(Site)

  CTD <- CTD %>%
    dplyr::left_join(latlons, by = "Site") %>%
    dplyr::rename(Latitude = Latitude.x, Longitude = Longitude.x) %>%
    dplyr::select(-c(Latitude.y, Longitude.y))

  WQ <- dplyr::bind_rows(WQ, CTD) %>%
    dplyr::left_join(DL, by = "ANALYTE") %>%
    dplyr::rename(mdl = `method detection limit`, SITE_ID = Site) %>%
    dplyr::select(-contains("detection limit corrected")) %>%
    dplyr::mutate(QA_CODE = dplyr::case_when(
      # If a value is equal to 1/2 the respective MDL, either replace it with NA or flag as nondetect with imputed value (or whatever you need to do to ensure consistency across datasets)
      RESULT < mdl ~ "nondetect"
    )) %>%
    dplyr::mutate(
      Study = "CSMI_2021",
      Year = 2021
    )


  # return the joined data
  return(WQ)
}


# Appears there are no chl-a measurements for the Gaurdian data, but USGS collected chl-a data at some of the same sites within a week or so. Need to confirm with Ryan/Aabir.
# A few Lat-longs are missing but probably can be found in profile data below TRUE
# But also, the lat/lons are incredibly low precision
