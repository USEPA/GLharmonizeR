# Note that we are not including CSMI 2010 data currently because key details are unclear and not documented - commented out in csmiJoinAll.R

    # XXX this is for csmi 2010, since it's not included, leaving it commented out
    # dplyr::mutate(
    #   FRACTION = NA,
    #   FRACTION = as.character(FRACTION)
    #   ) %>%
    # dplyr::mutate(FRACTION = dplyr::case_when(
    #   FRACTION == "F" ~ "Filtrate",
    #   FRACTION == "U" ~ "Total/Bulk",
    #   FRACTION == "A" ~ "Filtrate",
    #   FRACTION == "M" ~ "Filtrate",
    #   FRACTION == "D" ~ "Filtrate",
    #   FRACTION == "V" ~ "Total/Bulk",
    #   FRACTION == "PCN" ~ "Residue",
    #   FRACTION == "Not applicable" ~ NA,
    #   .default = FRACTION
    # )) %>%





#' Load and join data for CSMI 2010 from csv excel files
#'
#' @description
#' `.readCleanCSMI2010` returns a dataframe of all of the joined water quality data relating to CSMI 2010
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @importFrom magrittr "%>%"
#' @param csmi2010 a string specifying the directory path of the access database
#' @return dataframe of the fully joined water quality data from CSMI 2010
.readCleanCSMI2010 <- function(csmi2010, n_max = Inf) {
  df <- openxlsx::read.xlsx(file.path(csmi2010, "GL2010db.xlsx"), sheet = 1, rows = 1:n_max) %>%
    dplyr::slice(9:dplyr::n()) %>%
    # Move spatial information to front to simplify table conversion
    dplyr::relocate(contains(c("Stn Depth", "Acutal", "Actual"))) %>%
    dplyr::select(-Notes)
  sampleCols <- which(grepl("Sample Type", names(df), ignore.case = T))
  tallFormatted <- df %>%
    dplyr::select(`Stn Depth (m)`:STIS) %>%
    # XXX DATE is filled with mixed forms of reporting date
    tidyr::fill(DATE, .direction = "down") %>%
    dplyr::mutate(
      sampleDate = lubridate::dmy(DATE),
      Latitude = as.numeric(`Acutal Lat (N)`),
      Longitude = as.numeric(`Actual Lon (W)`),
      stationDepth = as.numeric(`Stn Depth (m)`),
    ) %>%
    dplyr::select(-c(`Acutal Lat (N)`, `Actual Lon (W)`, `Stn Depth (m)`, `STIS #...1`)) %>%
    dplyr::select(-dplyr::starts_with("Part"), -DATE)


  # Define where new mini tables are
  tableBounds <- lapply((1:(length(sampleCols))), function(i) {
    df[, sampleCols[i]:
    ifelse(i + 1 <= length(sampleCols), (sampleCols[i + 1] - 1),
      (dim(df)[2])
    )]
  })

  tallFormatted <- purrr::map(
    tableBounds,
    \(df)  df %>%
      dplyr::rename(FRACTION = 1) %>%
      # drop STIS
      dplyr::select(!dplyr::contains("STIS")) %>%
      dplyr::bind_cols(tallFormatted, .) %>%
      tidyr::pivot_longer(-c(FRACTION, LAKE, SITE, STATION, PROJECT, sampleDate, `blk/dup other`, STIS, stationDepth, Latitude, Longitude), names_to = "ANALYTE", values_to = "RESULT")
  ) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::mutate(
      RESULT = as.numeric(RESULT),
      ANALYTE = stringr::str_remove(ANALYTE, "\\.\\.\\.[[:number:]]*"),
      numerator = tolower(stringr::str_extract(ANALYTE, ".g")),
      denominator = tolower(stringr::str_extract(ANALYTE, "[[:alpha:]]$")),
      UNITS = stringr::str_trim(paste0(numerator, denominator)),
      ANALYTE = stringr::str_trim(ifelse(
        grepl("Part", ANALYTE, ignore.case = TRUE),
        stringr::str_extract(ANALYTE, "^Part[[:blank:]]*."),
        stringr::str_extract(ANALYTE, "^[[:graph:]]*")
      )),
      ANALYTE = ifelse(
        grepl("Part  C", ANALYTE, ignore.case = TRUE),
        "Part C",
        ANALYTE
      ),
      ANALYTE = stringr::str_remove_all(ANALYTE, "\\+"),
      ANALYTE = stringr::str_remove_all(ANALYTE, "-"),
      ANALYTE = stringr::str_remove_all(ANALYTE, "="),
      FRACTION = dplyr::case_when(
        FRACTION == "F" ~ "Filtrate",
        FRACTION == "U" ~ "Total/Bulk",
        FRACTION == "A" ~ "Filtrate",
        FRACTION == "M" ~ "Filtrate",
        FRACTION == "D" ~ "Filtrate",
        FRACTION == "V" ~ "Total/Bulk",
        FRACTION == "PCN" ~ "Residue",
        FRACTION == "Not applicable" ~ NA,
        .default = FRACTION
      )
    )


  # move detection limits to own column
  df <- openxlsx::read.xlsx(file.path(csmi2010, "GL2010db.xlsx"), sheet = 1, rows = 1:2) %>%
    dplyr::slice(1:2)

  dls <- df %>%
    dplyr::select(dplyr::one_of((names(.)[colMeans(is.na(.)) == 0]))) %>%
    dplyr::select(-1) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to = "ANALYTE", values_to = "mdl") %>%
    dplyr::distinct(ANALYTE, mdl) %>%
    dplyr::mutate(mdl = as.numeric(mdl)) %>%
    tidyr::drop_na()

  df <- tallFormatted %>%
    # Drop missing STIS number or placeholder values
    dplyr::filter(STIS != "STIS #") %>%
    dplyr::filter(!grepl("detection limit", STIS)) %>%
    dplyr::left_join(dls, by = "ANALYTE") %>%
    tidyr::drop_na(RESULT) %>%
    dplyr::mutate(
      RESULT = as.numeric(RESULT),
      Study = "CSMI_2010",
      Year = 2010
    )



  # Didn't find anything immediately usable in here, maybe it will come up when we do more intense QC
  # meta1 <- openxlsx::read.xlsx(file.path(directoryPath, "LMich10forms.xls"))
  # meta2 <- openxlsx::read.xlsx(file.path(directoryPath, "smpstts10.xls"))

  # CTD data, look like raw CTD measures, where the sheet names might correspond to the site?
  #   ctd <- openxlsx::read.xlsx(file.path(
  #     "L:",
  #     "Priv",
  #     "Great lakes Coastal",
  #     "2010 MED Lake Michigan",
  #     "CTD data",
  #     "CTD_GB",
  #     "CTD_Casts_2010",
  #     "CTD Casts 2010 Excel.xlsx"
  #   ))

  # missingness/joining checks in output:
  # mean(is.na(df$CodeName)): 0
  # mean(df$CodeName == "Remove"): 0
  # mean(is.na(df$TargetUnits)): 0
  # df %>% filter(ReportedUnits != TargetUnits) %>% reframe(mean(is.na(ConversionFactor))): 0 cases
  # mean(is.na(df$sampleDateTime))  # 0
  return(df)
}
