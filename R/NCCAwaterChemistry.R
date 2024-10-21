#' Read in all NCCA water quality from 2010
#'
#' @description
#' `.readNCCA2010` returns water quality data measured during the NCCA study in 2010
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param NCCAwq2010 a string specifying the directory of the data
#' @return dataframe
.readNCCA2010 <- function(NCCAwq2010, n_max = Inf) {
  df <-NCCAwq2010 %>%
    purrr::map_dfr(readr::read_csv,
      n_max = n_max,
      show_col_types = FALSE,
      col_types = readr::cols(
        # DONE Load MDL, MRL, PQL
        "DATE_COL" = readr::col_date(format = "%m/%d/%Y"),
        "LAB_SAMPLE_ID" = "-",
        "SAMPLE_ID" = "-",
        "BATCH_ID" = "-",
        "DATE_ANALYZED" = "-",
        "HOLDING_TIME" = "-",
        "MDL" = "d",
        "MRL" = "d",
        "PQL" = "d",
      )
    ) %>%
    dplyr::rename(
      ANL_CODE = PARAMETER,
      ANALYTE = PARAMETER_NAME,
      sampleDateTime = DATE_COL
    ) %>%
    dplyr::mutate(
      # Combine Nitrate  Nitrite
      METHOD = as.character(METHOD),
      .by = c(UID, SITE_ID, sampleDateTime)
      # [x] make sure Nitrate and Nitrite are already mg/L (this is true)
      # DOCTHIS We assume sampling events that don't have certain analytes reported
      # DOCTHIS remove the observation when either one is missing  Also for Nitrate / Nitrite
    ) %>%
    dplyr::filter(ANALYTE != "Nitrate", ANALYTE != "Nitrite") %>%
    # All NCCA WQ samples at 0.5m
    dplyr::mutate(
      sampleDepth = 0.5
    ) %>%
    dplyr::mutate(
      # [x] add this to Analytes3
      Study = "NCCA_WChem_2010",
      # QACODE =ifelse((STATE=="WI") & (PARAMETER == "CHLA"), QACODE, paste(QACODE, sep = "; ", "WSLH"))
    ) %>%
    dplyr::rename(
      QAcode = QACODE
    )
  return(df)
}

#' Read in all NCCA water quality from 2015
#'
#' @description
#' `.readNCCA2015` returns water quality data measured during the NCCA study in 2015
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param NCCAwq2015 a string specifying the directory of the data
#' @return dataframe
.readNCCA2015 <- function(NCCAwq2015, n_max = Inf) {
  df <- readr::read_csv(NCCAwq2015,
    n_max = n_max,
    show_col_types = FALSE,
    col_types = readr::cols(
      "UID" = "d",
      "SITE_ID" = "c",
      "DATE_COL" = "c",
      "ANALYTE" = "c",
      "LRL" = "d",
      "MDL" = "d",
      "METHOD" = "c",
      "NARS_FLAG" = "c",
      "NARS_COMMENT" = "c",
      "RESULT" = "d",
      "RESULT_UNITS" = "c"
    )
  ) %>%
    dplyr::rename(
      sampleDateTime = DATE_COL,
      QAcode = NARS_FLAG,
      QAcomment = NARS_COMMENT,
      UNITS = RESULT_UNITS,
      ANL_CODE = ANALYTE
    ) %>%
    dplyr::mutate(
      sampleDateTime = lubridate::dmy(sampleDateTime),
      # Combine Nitrate adn Nitrite
      # [x] does this create a problem (check if whenever Nitrate is missing Nitrite is missing).
      # No. we settled that this behaves well for one or both missing
      # XXX maybe less confusing to do pivot_wider first
      # [x] make 2 separate mutates so the .by goes more obvsiously with the mean (if not pivoted wider)
      # We decided not to do this actually. That it is easy enough to read and understand
      Nitrite = mean(ifelse(ANL_CODE == "NITRITE_N", RESULT, NA), na.rm = TRUE),
      Nitrate = mean(ifelse(ANL_CODE == "NITRATE_N", RESULT, NA), na.rm = TRUE),
      `Nitrate/Nitrite` = Nitrate + Nitrite,
      RESULT = dplyr::case_when(
        ANL_CODE == "NITRATE_N" ~ `Nitrate/Nitrite`,
        .default = RESULT
      ),
      # Change the names
      ANL_CODE = dplyr::case_when(
        ANL_CODE == "NITRATE_N" ~ "Diss_NOx",
        .default = ANL_CODE
      ),
      UNITS = dplyr::case_when(
        # Replacing units that had NA's added to their end to make things simpler
        (ANL_CODE == "AMMONIA_N") & (lubridate::year(sampleDateTime) == 2015) ~ "mgL",
        (ANL_CODE == "Diss_NOx") & (lubridate::year(sampleDateTime) == 2015) ~ "mgL",
        (ANL_CODE == "SRP") & (lubridate::year(sampleDateTime) == 2015) ~ "mgL",
        (ANL_CODE == "PTL") & (lubridate::year(sampleDateTime) == 2015) ~ "mgL",
        .default = UNITS
      ),
      .by = c(UID, SITE_ID, sampleDateTime)
    ) %>%
    dplyr::select(
      -dplyr::contains("NITR")
    ) %>%
    # Don't need to drop Nitrate because we enter Nitrate/Nitrite in its stead
    dplyr::filter(
      ANL_CODE != "NITRITE_N"
    ) %>%
    # All NCCA WQ samples at 0.5m
    dplyr::mutate(
      sampleDepth = 0.5,
      Study = "NCCA_WChem_2015"
    ) %>%
    # cleaning up flags ending with empty characters
    dplyr::mutate(
      QAcode = stringr::str_replace(QAcode, ",", ";"),
      QAcode = stringr::str_remove(QAcode, ";$"),
      QAcomment = stringr::str_remove(QAcomment, ";$"),
      QAcode= stringr::str_remove_all(QAcode, "NA;"),
      QAcomment = stringr::str_remove_all(QAcomment, "NA;"),
      QAcomment = stringr::str_remove_all(QAcomment, ": "),
      QAcomment = ifelse(QAcomment == "", NA, QAcomment),
      QAcode = stringr::str_replace_all(QAcode, ";", ","),
      QAcode = stringr::str_remove_all(QAcode, " "),
      QAcode = ifelse(QAcode == "NA", NA, QAcode)
    )

  return(df)
}

#' Read in all NCCA water quality from 2020
#'
#' @description
#' `.readNCCA2020` returns water quality data measured during the NCCA study in 2020
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param NCCAwq2020 a string specifying the directory of the data
#' @param NCCAsites2020 a string specifying the directory of the data
#' @return dataframe
.readNCCA2020 <- function(NCCAwq2020, NCCAsites2020, n_max = Inf) {
  sites <- readr::read_csv(NCCAsites2020, show_col_types = FALSE) %>%
    # keeping enough to add station depth information for a given sampling event (respecting visit number)
    dplyr::select(
      UNIQUE_ID,
      STATION_DEPTH,
      # keeping for potential filtering
      EPA_REG, GREAT_LAKE, LAKE_REG, NCCA_REG, NPS_PARK
    )  %>%
    distinct()

  # data has siteID, lat/lon, chem info, date, just need stationDepth from sites file
  df <- readr::read_csv(NCCAwq2020,
    n_max = n_max,
    show_col_types = FALSE,
    col_types = readr::cols(
      "UID" = "d",
      "SITE_ID" = "c",
      "DATE_COL" = readr::col_datetime(format = "%m/%d/%Y"),
      "ANALYTE" = "c",
      "RL" = "d",
      "MDL" = "d",
      "MATRIX" = "c",
      "LAB" = "c",
      "NARS_FLAG" = "c",
      "NARS_COMMENT" = "c",
      "RESULT" = "d",
      "RESULT_UNITS" = "c",
      # drop nonspecified columns
      .default = "-"
    )
  ) %>%
    dplyr::rename(
      sampleDateTime = DATE_COL,
      QAcode = NARS_FLAG,
      QAcomment = NARS_COMMENT,
      UNITS = RESULT_UNITS,
    ) %>%
    tidyr::pivot_wider(id_cols = UID:sampleDateTime, names_from = ANALYTE, values_from = LAB:QAcomment) %>%
    # Combine Nitrate adn Nitrite
    dplyr::mutate(
      RESULT_NITRATE_NITRITE_N = ifelse(is.na(RESULT_NITRATE_NITRITE_N), RESULT_NITRITE_N + RESULT_NITRATE_N, RESULT_NITRATE_NITRITE_N),
    ) %>%
    tidyr::pivot_longer(cols= LAB_PTL:QAcomment_SILICA, names_pattern = "^([[:alpha:]]*)_(.*)$", names_to = c(".value", "ANALYTE"), names_repair = "unique") %>%
    dplyr::select(-ANALYTE...7) %>%
    dplyr::rename(ANL_CODE = ANALYTE...4) %>%
    # Filter out nitrate and nitrite separately
    dplyr::filter(! ANL_CODE %in% c("NITRATE_N", "NITRITE_N")) %>%
    dplyr::mutate(
      ANL_CODE = ifelse(ANL_CODE == "NITRATE_NITRITE_N", "Diss_NOx", ANL_CODE),
      # Assert reported units
      ReportedUnits = dplyr::case_when(
        ANL_CODE == "COND" ~ "uscm",
        ANL_CODE == "TKN" ~ "mgL",
        ANL_CODE == "CHLORIDE" ~ "mgL",
        ANL_CODE == "Alkalinity" ~ "mgL",
        ANL_CODE == "SULFATE" ~ "mgL",
        ANL_CODE == "PH" ~ "unitless",
        ANL_CODE == "SILICA" ~ "mgL",
        ANL_CODE == "Diss_NOx" ~ "mgL", # Original: mg N/L Might need to convert this
        ANL_CODE == "SRP" ~ "mgL", # Original: mg P/L Might need to convert
        ANL_CODE == "CHLA" ~ "ugL",
        ANL_CODE == "AMMONIA_N" ~ "mgL", # Original: mg N/L Might need to convert
        ANL_CODE == "PTL" ~ "mgL",
        ANL_CODE == "DIN" ~ "mgL"
    ),
      sampleDepth = 0.5,
      Study = "NCCA_WChem_2020"
    )

  return(df)
}
