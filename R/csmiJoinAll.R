#' Load and join CSMI water quality data from 2010, 2015, and 2021.
#'
#' @description
#' `LoadCSMI` returns a dataframe of all of the joined water quality data relating to CSMI years 
#' 2010, 2015, and 2021.
#'
#' @details
#' This is the main functions users should use to load and assemble CSMI data
#' using this package. This function is also called in over arching functions 
#' to assemble data across multiple data sources.
#' @param csmi2010 a string specifying the directory containing CSMI 2010 data 
#' @param csmi2015 a string specifying the filepath of the CSMI2015 access database
#' @param csmi2021 a string specifying the directory containing the CSMI 2021 data 
#' @return dataframe of the fully joined water quality data from CSMI years 2010, 2015, 2021 
#' @export
.LoadCSMI <- function(csmi2010, csmi2015, csmi2021, namingFile, n_max= Inf) {
  # Load file to map analyte names to standard names 
  renamingTable <- readxl::read_xlsx(namingFile, sheet= "CSMI_Map", na = c("", "NA"),
    .name_repair = "unique_quiet") 
  CSMI <- dplyr::bind_rows(
    # We aren't including 2010 at this point
    # .LoadCSMI2010(csmi2010, n_max = n_max),
    # [x] 2015 has a lot of missing in VALUE column
    # This is just becuase the way the original data is stored
    .LoadCSMI2015(csmi2015),
    .LoadCSMI2021(csmi2021, n_max = n_max)
  ) %>%
    dplyr::mutate(
      RESULT = dplyr::coalesce(RESULT, value)
    ) %>%
    dplyr::select(-value) %>%
    dplyr::bind_rows(dplyr::tibble(FRACTION=character())) %>%
    # XXX this is for csmi 2010, since it's not included, leaving it commented out
    dplyr::mutate(FRACTION = dplyr::case_when(
      FRACTION == "F" ~ "Filtrate",
      FRACTION == "U" ~ "Total/Bulk",
      FRACTION == "A" ~ "Filtrate",
      FRACTION == "M" ~ "Filtrate",
      FRACTION == "D" ~ "Filtrate",
      FRACTION == "V" ~ "Total/Bulk",
      FRACTION == "PCN" ~ "Residue",
      FRACTION == "Not applicable" ~ NA,
      .default = FRACTION
    )) %>%
    # remove ion charges and units
    dplyr::mutate(
      ANALYTE = ifelse(Study == "CSMI_2015", stringr::str_remove(ANALYTE, "_.*"), ANALYTE),
      ANALYTE = ifelse(Study == "CSMI_2015", stringr::str_remove_all(ANALYTE, "\\+"), ANALYTE),
      ANALYTE = ifelse(Study == "CSMI_2015", stringr::str_remove_all(ANALYTE, "-"), ANALYTE),
      ANALYTE = ifelse(Study == "CSMI_2015", stringr::str_remove_all(ANALYTE, "="), ANALYTE),
      ANALYTE = ifelse(grepl("CSMI_2021", Study, ignore.case=T) & (ANALYTE == "chl-a"), stringr::str_remove_all(ANALYTE, "-"), ANALYTE),
      sampleDate = lubridate::date(sampleDate),
      # This only contains information about where along the water column 
      # But we already have that with depth
      ANL_CODE = NA
    ) %>%

    # Join CSMI to new names
    dplyr::left_join(renamingTable, by = c("Study", "ANALYTE", "ANL_CODE"), na_matches="na") %>%
    dplyr::rename(ReportedUnits = Units)

  conversions <- readxl::read_xlsx(namingFile, sheet = "UnitConversions", .name_repair = "unique_quiet") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))
  key <- readxl::read_xlsx(namingFile, sheet = "Key", .name_repair = "unique_quiet") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  CSMI %>% 
    dplyr::left_join(key, by = "CodeName") %>% 
    # Simplify unit strings
    dplyr::mutate(ReportedUnits = tolower(stringr::str_remove(ReportedUnits, "/"))) %>%
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::mutate(RESULT = ifelse(ReportedUnits == TargetUnits, RESULT,
                    RESULT  * ConversionFactor)) %>%
    dplyr::rename(Units = TargetUnits) %>%
    dplyr::mutate(
      UID = as.character(UID),
      STIS = as.character(STIS),
      `STIS#` = as.character(`STIS#`),
      UID = dplyr::coalesce(UID, STIS, `STIS#`),
      UID = paste0("CSMI-", UID, 1:nrow(.))
    )

}

  # Turn into test
  # test %>%
  #   filter(! TargetUnits == ReportedUnits) %>%
  #   filter(is.na(ConversionFactor)) %>%
  #   count(ReportedUnits, TargetUnits, ConversionFactor)

# CSMI fraction labels
# From "L:\Priv\Great lakes Coastal\2010 MED Lake Michigan\2010\LMich10forms.xls"
# Sheet "flow_charts"