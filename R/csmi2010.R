# Should probably check that these data are not redundant with 2010 NCCA data


#' Load and join data for CSMI 2010 from csv excel files 
#'
#' @description
#' `.LoadCSMI2010` returns a dataframe of all of the joined water quality data relating to CSMI 2010
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @importFrom magrittr "%>%"
#' @param directorypath a string specifying the directory path of the access database
#' @return dataframe of the fully joined water quality data from CSMI 2010
.LoadCSMI2010 <- function(directoryPath){
  df <- readxl::read_xlsx(file.path(directoryPath, "GL2010db.xlsx")) %>%
    # Should we remove blk, intercal, recal, Ipom?
    dplyr::filter(!grepl("dup", `blk/dup other`)) %>%
    # group all of the different tables together
    tidyr::pivot_longer(dplyr::starts_with("STIS #"),
                  names_to = "__stis", values_to = "STIS#") %>%
    tidyr::pivot_longer(dplyr::starts_with("Sample Type"),
                  names_to = "__sampleType", values_to = "sampleType") %>%
    # particulate nitroget was contained in muyltiple tables so combine them (they're distinguished by their sample type)
    dplyr::mutate(`Part N ug/L` = dplyr::coalesce(`Part N ug/L...35`, `Part N ug/L...37`),
            DATE = lubridate::dmy(DATE),
            LATITUDE = as.numeric(`Acutal Lat (N)`), 
            LONGITUDE = as.numeric(`Actual Lon (W)`),
            sampleDepth = as.numeric(`Stn Depth (m)`),
            ) %>%
    dplyr::select(-dplyr::contains("..."), -dplyr::contains("__"))  %>%
    dplyr::rename(`Na+ mg/L` = `Na+ mg/l`,
    # Should double check that this is the station depth (maybe check to see if it was duplicated) 
           FRACTION = sampleType,
           Depth = `Stn Depth (m)`) %>%
    dplyr::mutate(Depth = as.numeric(Depth)) %>%
    tidyr::pivot_longer(c(8:25, 27, 33), names_pattern = "^([[:graph:]]*) (.*/L)$", names_to = c("ANALYTE", "UNITS"), values_to = "RESULT") %>%
    dplyr::select(-c(LAKE, SITE, STATION, PROJECT, `blk/dup other`, STIS, `Acutal Lat (N)`, `Actual Lon (W)`, DATE)) %>%
    dplyr::mutate(UNITS = str_remove(UNITS, "^[[:space:]]*"))


    # move detection limits to own column
  dls <- df %>%
    dplyr::filter(`STIS#` == "method detection limit") %>%
    dplyr::distinct(ANALYTE, RESULT) %>% 
    tidyr::drop_na() %>%
    dplyr::rename(mdl = RESULT) %>%
    dplyr::mutate(mdl = as.numeric(mdl))

  df <- df %>%
    dplyr::filter(`STIS#` != "STIS #") %>%
    dplyr::filter(! grepl("detection limit", `STIS#`)) %>%
    dplyr::left_join(dls, by = "ANALYTE") %>%
    tidyr::drop_na(RESULT) %>%
    dplyr::mutate(RESULT = as.numeric(RESULT)) 



  # Didn't find anything immediately usable in here, maybe it will come up when we do more intense QC
  # meta1 <- readxl::read_xls(file.path(directoryPath, "LMich10forms.xls"))
  # meta2 <- readxl::read_xls(file.path(directoryPath, "smpstts10.xls"))

  # CTD data, look like raw CTD measures, where the sheet names might correspond to the site?
#   ctd <- readxl::excel_sheets(file.path(
#     "L:",
#     "Priv",
#     "Great lakes Coastal",
#     "2010 MED Lake Michigan",
#     "CTD data",
#     "CTD_GB",
#     "CTD_Casts_2010",
#     "CTD Casts 2010 Excel.xlsx"
#   ))

  return(df)
}
