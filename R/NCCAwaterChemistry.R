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
  NCCAwq2010 %>%
    purrr::map_dfr(readr::read_csv,
      n_max = n_max,
      show_col_types= FALSE,
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
      Study = "NCCA_WChem_2010"
    ) %>%
    dplyr::rename(
      QAcode = QACODE
    )
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
    show_col_types= FALSE,
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
  # 95% missingness beofre LRL 88% after
  # 40% missingness before MDL 24% after
  # Didn't do anything for method so removing it's inference 
  # fill in lab specific quantities
  dplyr::mutate(
    LRL = mean(LRL, na.rm = T),
    MDL = mean(MDL, na.rm= T),
    # Lab isn't missing
    .by = c(LAB, ANL_CODE)
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
      # Only 5% couldn't be imputed this way
      NitriteMDL = mean(ifelse(ANL_CODE == "NITRITE_N", MDL, NA), na.rm = TRUE),
      NitrateMDL = mean(ifelse(ANL_CODE == "NITRATE_N", MDL, NA), na.rm = TRUE),
      `Nitrate/Nitrite` = Nitrate + Nitrite,
      NitrateNitriteMDL = NitriteMDL + NitrateMDL,
      RESULT = dplyr::case_when(
        ANL_CODE == "NITRATE_N" ~ `Nitrate/Nitrite`,
        .default = RESULT
      ),
      # [x] create mdl for imputing nitrate nitrite
      MDL = ifelse(ANL_CODE == "NITRATE_N", NitrateNitriteMDL, MDL),
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
    dplyr::mutate(
      QAcode = stringr::str_replace(QAcode, ",", ";"),
      QAcode = paste(QAcode, ifelse(LAB == "WSLH", "WSLH", ""), sep = ";"),
      QAcode = stringr::str_remove(QAcode, ";$"),
      QAcomment = stringr::str_remove(QAcomment, ";$"),
      QAcomment = stringr::str_remove(QAcomment, ";$"),
      QAcomment = paste(QAcomment, ifelse(LAB == "WSLH", "WSLH used large filters for Chla-A", ""), sep = ";"),
      QAcomment = stringr::str_remove_all(QAcomment, "NA;"),
      QAcomment = stringr::str_remove_all(QAcomment, ": "),
      QAcomment = ifelse(QAcomment == "", NA, QAcomment),
      QAcode = stringr::str_replace_all(QAcode, ";", ","),
      QAcode = stringr::str_remove_all(QAcode, " "),
      QAcode = ifelse(QAcode == "NA", NA, QAcode) 
    )
  
  return(df)
}
