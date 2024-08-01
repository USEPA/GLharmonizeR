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
    # dplyr::mutate(
    #   # Combine Nitrate  Nitrite
    #   Nitrite = mean(ifelse(ANALYTE == "Nitrite", RESULT, NA), na.rm = TRUE),
    #   Nitrate = mean(ifelse(ANALYTE == "Nitrate", RESULT, NA), na.rm = TRUE),
    #   METHOD = as.character(METHOD),
    #   `Nitrate/Nitrite` = Nitrate + Nitrite,
    #   # DONE does this create a problem (check if whenever Nitrate is missing Nitrite is missing)
    #   # NO. We discussed these in our meetings and decide it has the correct behavior when one or more
    #   # values are missing
    #   # XXX maybe easier to do pivot_wider first
    #   # DONE make sure Nitrate and Nitrite are already mg/L (this is true)
    #   # DOCTHIS We assume sampling events that don't have certain analytes reported
    #   # DOCTHIS remove the observation when either one is missing  Also for Nitrate / Nitrite
    #   RESULT = dplyr::case_when(
    #     ANALYTE == "Nitrate" ~ `Nitrate/Nitrite`,
    #     .default = RESULT
    #   ),
    #   ANALYTE = dplyr::case_when(
    #     ANALYTE == "Nitrate" ~ "Nitrate/Nitrite",
    #     .default = ANALYTE
    #   ),
    #   ANL_CODE = dplyr::case_when(
    #     ANALYTE == "Nitrate" ~ "Diss_NOx",
    #     .default = ANALYTE
    #   ),
    #   UNITS = dplyr::case_when(
    #     ANALYTE == "Nitrate" ~ "mgl",
    #     .default = UNITS
    #   ),
    #   .by = c(UID, SITE_ID, sampleDateTime)
    # ) %>%
    # [x] remove vlaues if one is missing
    #dplyr::filter((!is.na(Nitrite)) & (!is.na(Nitrate))) %>%
    # dplyr::select(
    #   # Remove the columns that we created called Nitrate, Nitrite and Nitrate/Nitrite
    #   -dplyr::contains("Nitr")
    # ) %>%
    # # Don't need to drop Ambient PAR because we enter CPAR in its stead
    # dplyr::filter(
    #   ANALYTE != "Nitrite"
    # ) %>%
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
  readr::read_csv(NCCAwq2015,
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
    dplyr::mutate(
      QAcode = paste(QAcode, ifelse(LAB == "WSLH", "WSLH", ""), sep = ";"),
      QAcode = stringr::str_remove(QAcode, ";$"),
      QAcomment = stringr::str_remove(QAcomment, ";$"),
      QAcomment = stringr::str_remove(QAcomment, ";$"),
      QAcomment = paste(QAcomment, ifelse(LAB == "WSLH", "WSLH used large filters for Chla-A", ""), sep = ";"),
      QAcomment = stringr::str_remove_all(QAcomment, "NA;"),
      QAcomment = stringr::str_remove_all(QAcomment, ": "),
      QAcode = stringr::str_replace_all(QAcode, ";", ","),
      QAcode = stringr::str_remove_all(QAcode, " ")
    )
}

# 2020/2021
# SITE_ID, Date_COL,  LAT/LON_DD (not 83), STATION_DEPTH


#' Read in all NCCA from 2000s, 2010, and 2015
#'
#' @description
#' `.readNCCAchemistry` returns water quality data along with spatial data from the
#'  site information measured through NCCA study in the early 2000s as well as in 2010, and 2015
#'
#' @details
#' The spatial information for sites is read in using the .readSites helper functions, this is then
#' joined to the water quality and ultimately output as a data table.
#' @param filepath a string specifying the directory of the data
#'
#' @return dataframe
.readNCCAchemistry <- function(NCCAwq2010 = NULL, NCCAwq2015 = NULL, NCCAwqQA = NULL, n_max = n_max) {
  # [ ] Replace these argument names to match the overall function
  dfs <- list()
  # [ ] Incorporate QA into both 2010 and 2015, and make the argument consistently nccaWQqaFile
  if (!is.null(NCCAwq2010)) dfs[[1]] <- .readNCCA2010(NCCAwq2010, n_max = n_max) else print("2010 WQ filepath not specified or trouble finding")
  if (!is.null(NCCAwq2015)) dfs[[2]] <- .readNCCA2015(NCCAwq2015, n_max = n_max) else print("2015 WQ filepath not specified or trouble finding")

  # DONE check if bind_rows breaks with one file
  dfs <- dplyr::bind_rows(dfs) %>%
    # QC filters
    # filter(! QACODE %in% c("J01", "Q08", "ND", "Q", "H", "L"))
    dplyr::mutate(
      SAMPYEAR = lubridate::year(sampleDateTime),
    )

  QA <- openxlsx::read.xlsx(NCCAwqQA, sheet = "NCCAQAcounts2") %>%
      dplyr::distinct(QAcode, Definition, QAconsiderations)

  test <- dfs %>%
    # fuzzy join solution from 
    # https://stackoverflow.com/questions/69574373/joining-two-dataframes-on-a-condition-grepl
    fuzzyjoin::fuzzy_join(
      QA,
      # didn't include sampyear, because codes seem to have stayed the same
      by = c("QAcode"),
      match_fun = list(stringr::str_detect),
      mode = "left"
    ) %>%
    dplyr::mutate(
      QAcode = dplyr::coalesce(QAcode.x, QAcode.y),
    ) %>%
    dplyr::select(-c(dplyr::ends_with(".x"), dplyr::ends_with(".y")))
}
