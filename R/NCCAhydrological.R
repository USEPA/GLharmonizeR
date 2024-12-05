
#' Load and join NCCA 2010 hydrographic data from csv files
#'
#' @description
#' `.loadNCCAhydro2010` returns a dataframe of all of the hydrographic data relating to NCCA 2010
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the filepath of the data
#'
#' @return dataframe
.loadNCCAhydro2010 <- function(NCCAhydrofiles2010, NCCAsites2010, namingFile, n_max = n_max) {
  sites <- .loadNCCASite2010(NCCAsites2010) %>%
    dplyr::mutate(
      SITE_ID = stringr::str_remove(SITE_ID, "^NCCA[:alpha:]{0,2}10-"),
      SITE_ID = stringr::str_remove(SITE_ID, "^GLBA10-"),
      #SITE_ID = stringr::str_replace(SITE_ID, "-GLBA10-", "-"),
      )
  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))
  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NCCA_Map", na.strings = c("", "NA")) %>%
    # remove nas from table to remove ambiguities on joinging
    dplyr::mutate(
      ANALYTE = ifelse(is.na(ANALYTE), ANL_CODE, ANALYTE),
      ANL_CODE = ifelse(is.na(ANL_CODE), ANALYTE, ANL_CODE),
      Methods = ifelse(is.na(Methods), Study, Methods)
    )


  df <- NCCAhydrofiles2010 %>%
    purrr::map_dfr(readr::read_csv, n_max = n_max, show_col_types = FALSE) %>%
    # filter to just downcast (include IM_CALC because that's where Secchi is, NREC wasn't
    # observed in great lakes so don't need to worry about it)
    dplyr::filter(CAST %in% c("DOWNCAST", "IM_CALC")) %>%
    dplyr::rename(
      ANALYTE = PARAMETER_NAME,
      sampleDepth = SDEPTH,
      stationDepth = `STATION_DEPTH(m)`,
      QAcode = QA_CODE,
      QAcomment = QA_COMMENT
    ) %>%
    dplyr::select(-PARAMETER) %>% 
    dplyr::mutate(
      UNITS = ifelse(ANALYTE == "pH", "unitless", UNITS),
      UNITS = ifelse(ANALYTE == "Mean secchi", "m", UNITS),
      # turbidity is removed anyway
      UNITS = ifelse(ANALYTE == "Turbidity", "unitless", UNITS),
      sampleDepth = ifelse(sampleDepth == -9, NA, sampleDepth)
    ) %>%
    dplyr::mutate(
      UNITS  = rev(names(table(UNITS)))[[1]],
      .by = ANALYTE
    ) %>%
    dplyr::reframe(.by = UID:ANALYTE, RESULT = mean(RESULT, na.rm = T), dplyr::across(UNITS:QAcomment, function(x) toString(unique(x)))) %>%  
    tidyr::pivot_wider(id_cols = UID:CAST, names_from = ANALYTE, values_from = UNITS:RESULT, values_fill = list("RESULT" = 9999999, NA)) %>%
    # fill in mean secchi in same rows where other results appear
    dplyr::mutate(`RESULT_Mean secchi` =  mean(`RESULT_Mean secchi`, na.rm = T), .by = c(DATE_COL, SITE_ID)) %>%
    # remove where sechhi used to appear
    tidyr::drop_na(sampleDepth) %>%
    # [x] filter out where either ambientPAR or underPAR check if this does what we think
    dplyr::filter(!is.na(`RESULT_Underwater PAR`) & !is.na(`RESULT_Ambient PAR`)) %>%
    dplyr::mutate(`RESULT_Corrected PAR` = `RESULT_Underwater PAR`/ `RESULT_Ambient PAR`) %>%
    dplyr::select(-c(`RESULT_Underwater PAR`, `RESULT_Ambient PAR`)) %>%
    tidyr::pivot_longer(cols= `UNITS_Mean secchi`:`RESULT_Corrected PAR`, names_pattern = "(.*)_(.*)$", names_to = c(".value", "ANALYTE")) %>%
    # there aren't any comments so we assume that if value is nan (something we put in) it was not originally reported
    dplyr::filter(RESULT != 9999999) %>%
    dplyr::mutate(sampleDepth = ifelse(ANALYTE == "Mean secchi", NA, sampleDepth)) %>%
    dplyr::mutate(DATE_COL = lubridate::mdy(DATE_COL)) %>%
    dplyr::mutate(
    #  sampleDepth = ifelse(sampleDepth == -9.0, NA, sampleDepth),
      Study = "NCCA_hydro_2010",
      UNITS = ifelse(ANALYTE == "Corrected PAR", "percent", UNITS),
      # make site ids look like site id file
      SITE_ID = stringr::str_remove_all(SITE_ID, "NCCA10-"),
      SITE_ID = stringr::str_remove_all(SITE_ID, "NCCAGL10-"),
      SITE_ID = stringr::str_remove_all(SITE_ID, "GLBA10-"),
    ) %>%

    # add station info
    dplyr::left_join(sites, by = "SITE_ID") %>%
    dplyr::mutate(stationDepth = dplyr::coalesce(stationDepth.x, stationDepth.y)) %>%
    dplyr::select(-c(stationDepth.x, stationDepth.y)) %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    dplyr::mutate(
      ANALYTE = stringr::str_trim(ifelse(is.na(ANALYTE), ANL_CODE, ANALYTE)),
    ) %>%
    # convert units
    dplyr::left_join(renamingTable, by = c("ANALYTE", "Study")) %>%
    dplyr::left_join(key) %>%
    dplyr::left_join(conversions) %>%
    dplyr::mutate(RESULT = ifelse(is.na(ConversionFactor), RESULT, RESULT * ConversionFactor)) %>%
    dplyr::filter(CodeName != "Remove")

  return(df)
}



#' Load and join NCCA 2015 hydrographic data from csv files
#'
#' @description
#' `.loadNCCAhydro2015` returns a dataframe of all of the hydrographic data relating to NCCA 2010
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param NCCAhydrofile2015 a string specifying the filepath of the data
#' @return dataframe
.loadNCCAhydro2015 <- function(NCCAhydrofile2015, NCCAsites2015, namingFile, n_max = Inf) {
  sites <- .loadNCCASite2015(NCCAsites2015)
  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))
  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NCCA_Map", na.strings = c("", "NA")) %>%
    # remove nas from table to remove ambiguities on joinging
    dplyr::mutate(
      ANALYTE = ifelse(is.na(ANALYTE), ANL_CODE, ANALYTE),
      ANL_CODE = ifelse(is.na(ANL_CODE), ANALYTE, ANL_CODE),
      Methods = ifelse(is.na(Methods), Study, Methods)
    )

  df <- readr::read_csv(NCCAhydrofile2015, n_max = n_max, show_col_types = FALSE) %>%
    # the only comments mention no measurment data or typo
    # [x] Need to remove all NARS_COMMENTs
    dplyr::filter(CAST == "DOWNCAST") %>%
    dplyr::mutate(
      `Corrected PAR` = LIGHT_UW / LIGHT_AMB,
      sampleDateTime = as.Date(DATE_COL, origin = "1900-1-1"),
      Study = "NCCA_hydro_2015"
    ) %>%
    dplyr::filter(!is.na(LIGHT_UW) | !is.na(LIGHT_AMB)) %>%
    dplyr::select(
      -c(LIGHT_AMB, LIGHT_UW)
    ) %>%
    dplyr::rename(sampleDepth = DEPTH, stationDepth = STATION_DEPTH) %>%
    tidyr::pivot_longer(c(TRANS, CONDUCTIVITY:TEMPERATURE, `Corrected PAR`), names_to = "ANALYTE", values_to = "RESULT") %>%
    dplyr::select(-DATE_COL) %>%
    dplyr::mutate(
      UNITS = dplyr::case_when(
      # [x] can we make this more year specific
      # These were take from hdyro 2015 metadata file
      ANALYTE == "DO" ~ "mgl",
      ANALYTE == "TEMPERATURE" ~ "c",
      ANALYTE == "CONDUCTIVITY" ~ "uscm",
      ANALYTE == "Corrected PAR" ~ "%",
      ANALYTE == "TRANS" ~ "%",
      ANALYTE == "PH" ~ "unitless",
    )) %>%
    dplyr::left_join(renamingTable) %>%
    dplyr::filter(CodeName != "Remove") %>%
    dplyr::left_join(key) %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    dplyr::left_join(conversions) %>%
    dplyr::mutate(RESULT = ifelse(is.na(ConversionFactor), RESULT, RESULT * ConversionFactor))


# rename, change units, remove CodeName
  return(df)
}

#' Load and join NCCA 2020 hydrographic data from csv files
#'
#' @description
#' `.loadNCCAhydro2020` returns a dataframe of all of the hydrographic data relating to NCCA 2010
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param NCCAhydrofile2020 a string specifying the filepath of the data
#' @return dataframe
.loadNCCAhydro2020 <- function(NCCAhydrofile2020, NCCAsites2020, namingFile, n_max = Inf) {
  sites <- .loadNCCASite2020(NCCAsites2020)
  df <- readr::read_csv(NCCAhydrofile2020, n_max = n_max, show_col_types = FALSE) %>%
    # the only comments mention no measurment data or typo
    # [x] Need to remove all NARS_COMMENTs
    dplyr::filter(CAST == "DOWN", NCCA_REG == "Great Lakes") %>%
    dplyr::mutate(
      # This will be NA if either are missing
      `Corrected PAR` = LIGHT_UW / LIGHT_AMB,
      sampleDateTime = lubridate::mdy(DATE_COL),
      Study = "NCCA_hydro_2015"
    ) %>%
    dplyr::rename(sampleDepth = DEPTH, stationDepth = STATION_DEPTH) %>%
    tidyr::pivot_longer(c(CONDUCTIVITY:TEMPERATURE, `Corrected PAR`), names_to = "ANALYTE", values_to = "RESULT") %>%
    dplyr::select(UID, SITE_ID,
      Latitude = LAT_DD, Longitude = LON_DD, stationDepth, sampleDepth, QAcode = NARS_FLAG, QAcomment = NARS_COMMENT,
      sampleDateTime, Study, ANALYTE, RESULT) %>%
    dplyr::filter(! ANALYTE %in% c("LIGHT_AMB", "LIGHT_UW")) %>%
    dplyr::mutate(
      UNITS = dplyr::case_when(
      # [x] can we make this more year specific
      # These were take from hdyro 2015 metadata file
      ANALYTE == "DO" ~ "mgl",
      ANALYTE == "TEMPERATURE" ~ "c",
      ANALYTE == "CONDUCTIVITY" ~ "uscm",
      ANALYTE == "Corrected PAR" ~ "%",
      ANALYTE == "TRANS" ~ "%",
      ANALYTE == "PH" ~ "unitless",
    )) %>%
    dplyr::filter(CodeName != "Remove")

  return(df)
}

#' Load and join secchi data for NCCA 2015 from csv files
#'
#' @description
#' `.loadNCCAsecchi2015` returns a dataframe of all of the joined secchi data relating to NCCA 2015
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the filepath of the data
#' @return dataframe of the fully joined secchi data from NCCA 2015
.loadNCCAsecchi2015 <- function(NCCAsecchifile2015, NCCAsites2015, namingFile, n_max = Inf) {
  sites <- .loadNCCASite2015(NCCAsites2015)
  df <- readr::read_csv(NCCAsecchifile2015, n_max = n_max, show_col_types = FALSE) %>%
    dplyr::filter(
      # Kept estimated and based on trans
      # We will explicitly test if the measurements are greater than station depth
      !grepl("missing for this site", SECCHI_COMMENT, ignore.case = T),
      !grepl("unavailable for this site", SECCHI_COMMENT, ignore.case = T)
    ) %>%
    # Confirmed that the reference date with Hugh and by reformatting in Excel
    dplyr::mutate(
      SECCHI_TIME = round(as.numeric(SECCHI_TIME) * 24),
      DATE_COL = as.Date(DATE_COL, origin = "1900-1-1"),
      sampleDate = paste(DATE_COL, SECCHI_TIME, sep = "_"),
      sampleDateTime = lubridate::ymd_h(sampleDate),
    ) %>%
    # This may look like we are keeping MEAN_SECCHI_DEPTH to average with the others,
    # However, we filter it out in the mean call
    tidyr::pivot_longer(c(MEAN_SECCHI_DEPTH, DISAPPEARS, REAPPEARS), names_to = "SecchiType", values_to = "RESULT") %>%
    dplyr::reframe(
      SITE_ID = toString(unique(SITE_ID)),
      ANALYTE = "Secchi",
      DATE_COL = unique(DATE_COL),
      stationDepth = mean(STATION_DEPTH, na.rm = T),

      # MEAN_SECCHI_EEPTH is the estimated column (from Kd)
      # So we find the actual mean Mean of of the different casts manually
      RESULT = mean(ifelse(SecchiType != "MEAN_SECCHI_DEPTH", RESULT, NA), na.rm = T),
      # Compress all comments and note clear to bottom to be combined
      # [x] change CLEAR_TO_BOTTOM to actually checking if Disappear/Reappear >= to Depth
      # Check to see if this works
      CLEAR_TO_BOTTOM = RESULT >= stationDepth,
      QAcomment = toString(unique(SECCHI_COMMENT)),
      .by = c(UID)
    ) %>%
    dplyr::mutate(
      # Looked through and saw none of the QAcomments were relevent
      # So only relevant comments are related to whether clear to bottom
      QAcomment = NA,
      QAcode = dplyr::case_when(
        CLEAR_TO_BOTTOM == TRUE ~ "CTB"
      ),
      QAcomment = dplyr::case_when(
        CLEAR_TO_BOTTOM == TRUE ~ "Secchi clear to bottom"
      )
    ) %>%
    dplyr::mutate(
      Study = "NCCA_secchi_2015",
      Units = "m",
      ANALYTE = "Secchi",
      CodeName = "Secchi",
      LongName = "Secchi"
    ) %>%
    dplyr::left_join(sites)
  return(df)
}



#' Load and join hydrographic and secchi data for NCCA 2010 and 2015
#'
#' @description
#' `.loadNCCAhydro` returns a dataframe of all of the hydrographic data relating to NCCA 2010 and 2015
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the filepath of the data
#' @return dataframe
.loadNCCAhydro <- function(
    NCCAhydrofiles2010, NCCAsites2010,
    NCCAhydrofile2015, NCCAsites2015,
    NCCAsecchifile2015, namingFile,
    n_max = Inf) {
  dplyr::bind_rows(
    .loadNCCAhydro2010(NCCAhydrofiles2010, NCCAsites2010, namingFile, n_max = n_max),
    .loadNCCAhydro2015(NCCAhydrofile2015, NCCAsites2015, namingFile, n_max = n_max),
    .loadNCCAsecchi2015(NCCAsecchifile2015, NCCAsites2015, namingFile, n_max = n_max)
  )
}
