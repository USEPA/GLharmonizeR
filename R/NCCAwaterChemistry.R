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
      # Note these are all NY samples with nitrate and nitrite included in addition to nitrate/nitrite but doesn't hurt to keep this code
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
.readNCCA2015 <- function(NCCAwq2015, NCCAsites2015, n_max = Inf) {
  sites <- readr::read_csv(NCCAsites2015, show_col_types = FALSE) %>%
    # cutdown number of lats and longs
    dplyr::select(
      UID,
      SITE_ID,
      # [x] how to choose the best projection here (DD vs DD83) is this constient across sources?
      # This only applies to targetted sites, actual coordinates are unique and settles this problem
      # No missingness, so no need to coalesce
      Latitude = LAT_DD83,
      Longitude = LON_DD83,
      stationDepth = STATION_DEPTH,
      WTBDY_NM = GREAT_LAKE
    ) %>%
    tidyr::drop_na()

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
      "RESULT_UNITS" = "c",
      "LAB_SAMPLE_ID" = "-",
      "DATE_ANALYZED" = "-",
      "DATE_RECEIVED" = "-",
    )
  ) %>%
    dplyr::rename(
      siteID = SITE_ID,
      sampleDateTime = DATE_COL,
      QAcode = NARS_FLAG,
      QAcomment = NARS_COMMENT,
      reportedUnits = RESULT_UNITS,
      sampleID = SAMPLE_ID,
      batchID = BATCH_ID,
    ) %>%
    dplyr::mutate(
      sampleDateTime = lubridate::dmy(sampleDateTime)
    ) %>%
    tidyr::pivot_wider(id_cols = c(PUBLICATION_DATE:NCCA_REG), names_from = ANALYTE, values_from = LAB:sampleID) %>%
    # Combine Nitrate adn Nitrite
    dplyr::mutate(
      # Hide result in Nitrate so don't need to make all of the other columns
      RESULT_NITRATE_N =  RESULT_NITRITE_N + RESULT_NITRATE_N,
    ) %>%
    tidyr::pivot_longer(cols= LAB_PH:sampleID_SILICA, names_pattern = "^([[:alpha:]]*)_(.*)$", names_to = c(".value", "ANL_CODE"), names_repair = "unique") %>%
    dplyr::mutate(
      # Change the names
      ANL_CODE = dplyr::case_when(
        ANL_CODE == "NITRATE_N" ~ "Diss_NOx",
        .default = ANL_CODE
      ),
      reportedUnits = dplyr::case_when(
        ANL_CODE == "COND" ~ "uscm", # this is specific conductance at 25C
        ANL_CODE == "TKN" ~ "mgL",
        ANL_CODE == "CHLORIDE" ~ "mgL",
        ANL_CODE == "Alkalinity" ~ "mgL",
        ANL_CODE == "SULFATE" ~ "mgL",
        ANL_CODE == "PH" ~ "unitless",
        ANL_CODE == "SILICA" ~ "mgL", # Do we need to convert?
        ANL_CODE == "Diss_NOx" ~ "mgL", # Original: mg N/L Might need to convert this
        ANL_CODE == "SRP" ~ "mgL", # Original: mg P/L Might need to convert
        ANL_CODE == "CHLA" ~ "ugL",
        ANL_CODE == "AMMONIA_N" ~ "mgL", # Original: mg N/L Might need to convert
        ANL_CODE == "PTL" ~ "mgL",
        ANL_CODE == "DIN" ~ "mgL",
        ANL_CODE == "NTL" ~ "mgL"
        )
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
    ) %>%
    dplyr::select(
      UID,
      SITE_ID = siteID,
      sampleDateTime,
      NCCA_REG,
      ANL_CODE,
      LAB,
      ANALYTE,
      LRL,
      MDL,
      METHOD,
      QAcode, QAcomment,
      RESULT,
      reportedUnits,
      sampleDepth, 
      Study
    ) %>%
    dplyr::left_join(sites) %>%
    # Do this for the joining
    dplyr::mutate(
      ANALYTE = ANL_CODE,
      METHOD = ifelse(is.na(METHOD), Study, METHOD),
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
      UID,
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
        ANL_CODE == "COND" ~ "uscm", # [ ] is this specific conductance at 25C
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
    ) %>%
    dplyr::left_join(sites) %>%
    # Do this for later on joining
    dplyr::mutate(
      ANALYTE = ANL_CODE,
      )

  return(df)
}
