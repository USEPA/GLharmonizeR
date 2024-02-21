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

# Water chemistry  copied from 
# L:\Priv\Great lakes Coastal\2021 CSMI Lake Michigan\Data\Water chem
# Contact is Annie Fosso
  DL <- file.path(directoryPath, "Chem2021_detection limits.xlsx") %>%
    # The detection limit file contains MDLs and the values used to impute results <MDL.
    readxl::read_xlsx(sheet = "detection limits") %>%
    dplyr::select(23:38) %>%
    dplyr::mutate(Limit = dplyr::coalesce(...23, ...24)) %>%
    dplyr::select(-c(...23, ...24)) %>%
    tidyr::pivot_longer(-Limit, values_to = "RESULT", names_to = "ANALYTE") %>%
    tidyr::pivot_wider(id_cols = ANALYTE, names_from = Limit, values_from = RESULT, values_fn = mean) %>%
    dplyr::select(-`NA`) 

  WQ <- file.path(directoryPath, "Chem2021_FinalShare.xlsx") %>%
    readxl::read_xlsx(sheet = "DetLimitCorr") %>%
    dplyr::select(-dplyr::contains("...")) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("L"), ~ as.numeric(.))) %>%
    # tidyr::pivot_longer(15:29, names_to = "ANALYTE", values_to = "RESULT") %>%
    dplyr::mutate(      # Haven't figured out how to parse these times, can come back to it if it's important 
      Date= lubridate::date(Date)
    ) %>%
    dplyr::rename(STATION_DEPTH = `Site Depth (m)`,  SAMPLE_DEPTH = `Separate depths (m)`) %>%
      dplyr::select( -c(Month, Ship, Lake, `Research Project`, `Integrated depths (m)`, `DCL?`, `Stratified/ Unstratified?`,
                    `Time (EST)`, Station )) %>%
    # thinking that the first three letters of Site are all that matters
    dplyr::mutate(Site = stringr::str_extract(Site, "^[:alnum:]{3}"))




  #mutate(Time = case_when(
  #  grepl("/", `Time (EST)`) ~ # it's an interval
  #  as.numeric(`Time (EST)`) == FALSE ~ # time of day is a decimal
  #  ymd_hm(paste(Date, `Time (EST)`))) %>% # Need to fix time zones here

  # CTD
  # \Lake Michigan ML - General\Raw_data\CSMI\2021\2020 LM CSMI LEII CTD combined_Fluoro_LISST_12.13.21.xlsx
  # Contact is James Gerads

  ## bin averaged over 1 meter depth intervals
  ## -9.99E-29 is NA
  ## There are already processed, formatted ready to use files Should we use that?
  ## 
  CTD <- file.path(directoryPath, "2020 LM CSMI LEII CTD combined_Fluoro_LISST_12.13.21.xlsx") %>%
    readxl::read_xlsx(sheet = "Lake Michigan 2020 CSMI Data", skip = 1, na = c("", -9.99e-29)) %>% 
    dplyr::rename(Transect = ...1, Station = ...2, Date = ...3,
                  Latitude = `Latitude [deg]`,Longitude = `Longitude [deg]` ) %>%
    # depth to nearest meter
    dplyr::mutate(
      dplyr::across(dplyr::contains("Depth"), round),
      )

  timeSpace <- CTD %>% select(Transect:Date, Latitude, Longitude)
  CTD <- CTD %>% select(-c(Transect:Date, Latitude, Longitude))

  # Any other QC'ing necessary such as number of scans per bin, time elapsed [Not supposing so]
  # only keep one depth, and temperature
  CTD <- purrr::map(list(first = CTD[,c(1:11, 18)],  second = CTD[,20:27], third = CTD[,c(31, 33:38)]),
    \(df)  df %>%
            dplyr::rename(Depth = 1) %>%
            dplyr::bind_cols(timeSpace, .)) %>%
    purrr::reduce(dplyr::full_join, by = c("Transect", "Station", "Date", "Depth", "Latitude", "Longitude")) %>%
    dplyr::mutate(Station = stringr::str_remove(Station, "_")) %>%
    dplyr::full_join(WQ, by = c("Transect" = "Site", "Date", "Depth" = "SAMPLE_DEPTH")) %>%
    dplyr::select(-contains("Station"), -c(`STIS#`)) %>%
    tidyr::pivot_longer(
      cols = -c(Transect:Depth), names_to = "ANALYTE", values_to = "RESULT") %>%
    dplyr::rename(SAMPLE_DEPTH = Depth) %>%
    ## ADD MDLS at the end
    dplyr::left_join(DL, by = "ANALYTE") %>%
    dplyr::rename(mdl = `method detection limit`, SITE_ID = Transect) %>%
    dplyr::select(-contains("detection limit corrected")) %>%
    dplyr::mutate(QA_CODE = dplyr::case_when(
      # If a value is equal to 1/2 the respective MDL, either replace it with NA or flag as nondetect with imputed value (or whatever you need to do to ensure consistency across datasets)
      RESULT < mdl ~ "nondetect"
    )) %>%
    tidyr::separate_wider_regex(ANALYTE, c(ANALYTE = "\\S*", "\\s*", UNITS= ".*")) %>%
    dplyr::mutate(
      UNITS = stringr::str_remove_all(UNITS, "\\]"),
      UNITS = stringr::str_remove_all(UNITS, "\\[")
      )


  # return the joined data
  return(CTD)
}


# Appears there are no chl-a measurements for the Gaurdian data, but USGS collected chl-a data at some of the same sites within a week or so. Need to confirm with Ryan/Aabir.
# Lat-longs are missing but probably can be found in profile data below TRUE