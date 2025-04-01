#' Read in all NCCA water chemistry from 2010
#'
#' @description
#' `.loadNCCAwq2010` returns water chemistry data measured during the NCCA study in 2010
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param NCCAwq2010 a string specifying the directory of the data
#' @return dataframe
.loadNCCAwq2010 <- function(NCCAwq2010, NCCAsites2010, namingFile, n_max = Inf) {
  sites <- .loadNCCASite2010(NCCAsites2010) %>%
    dplyr::distinct()
  
  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units) %>%
    dplyr::distinct()

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor)) %>% 
    dplyr::distinct() # Duplicate rows

  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NCCA_Map", na.strings = c("", "NA")) %>%
    # remove nas from table to remove ambiguities on joining
    dplyr::mutate(
      ANALYTE = ifelse(is.na(ANALYTE), ANL_CODE, ANALYTE),
      ANL_CODE = ifelse(is.na(ANL_CODE), ANALYTE, ANL_CODE)#,
      # Methods = ifelse(is.na(Methods), Study, Methods) # There are no methods reported for GL in 2010
      # KV: Any methods reported in NCCA_Map are a result of not originally filtering the dataset to GL - shouldn't be needed
      # KV: Here and elsewhere, prefer not to fill in Methods with Study - just leave blank. Joins fine.
    ) %>%
    dplyr::distinct()

  df <- readr::read_csv(
    NCCAwq2010,
    n_max = n_max,
    show_col_types = FALSE,
    col_types = readr::cols(
      "DATE_COL" = readr::col_date(format = "%m/%d/%Y"),
      "LAB_SAMPLE_ID" = "-",
      "SAMPLE_ID" = "-",
      "BATCH_ID" = "-",
      "DATE_ANALYZED" = "-",
      "HOLDING_TIME" = "-",
      "MDL" = "d",
      "MRL" = "d"
      # "PQL" = "d", # No PQLs for GL
    )
    ) %>%
    # Remove sites that aren't Great Lakes (i.e., do not have "GL" in the SITE_ID)
    dplyr::filter(grepl("GL",SITE_ID)) %>%
    dplyr::rename(
      ANL_CODE = PARAMETER,
      ANALYTE = PARAMETER_NAME,
      sampleDateTime = DATE_COL
      # [x] KV: Note that sampleDateTime here does not have a time, only a date. Is time imputed somewhere? If so, it needs a flag.
    ) %>%
    # All NCCA WQ samples at 0.5m
    dplyr::mutate(
      sampleDateTime = lubridate::ymd_hm(paste0(sampleDateTime, " 12:00")),
      sampleDepth = 0.5,
      # [x] add this to Analytes3
      Study = "NCCA_WChem_2010",
      # QACODE =ifelse((STATE=="WI") & (PARAMETER == "CHLA"), QACODE, paste(QACODE, sep = "; ", "WSLH"))
      ANL_CODE = ifelse(is.na(ANL_CODE), ANALYTE, ANL_CODE),
      ANALYTE = ifelse(is.na(ANALYTE), ANL_CODE, ANALYTE),
      QACODE = paste(QACODE, "T", sep = "; ")
    ) %>%
    dplyr::rename(
      QAcode = QACODE,
      ReportedUnits = UNITS
    ) %>%
    ) %>%
    # Note that methods are all NA for GL sites but leaving as-is for generality
    dplyr::left_join(renamingTable, by = c("Study", "ANALYTE", "ANL_CODE", "METHOD" = "Methods")) %>%
    dplyr::left_join(key, by = dplyr::join_by(CodeName)) %>%
    dplyr::filter(CodeName != "Remove") %>%
    # KV: conversions did not join correctly without editing ReportedUnits
    dplyr::mutate(
      ReportedUnits = stringr::str_remove(ReportedUnits, "/"),
      ReportedUnits = tolower(ReportedUnits)) %>%
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::mutate(
      RESULT = ifelse(is.na(ConversionFactor), RESULT, RESULT * ConversionFactor),
      MDL = ifelse(!is.na(ConversionFactor), MDL * ConversionFactor, MDL),
      MRL = ifelse(!is.na(ConversionFactor), MRL * ConversionFactor, MRL)
      ) %>%
    dplyr::left_join(sites)

  # missingness/joining checks in output:
  # mean(is.na(df$CodeName)): 0
  # mean(df$CodeName == "Remove"): 0
  # mean(is.na(df$TargetUnits)): 0
  # df %>% filter(ReportedUnits != TargetUnits) %>% reframe(mean(is.na(ConversionFactor))): 0
  # mean(is.na(df$sampleDateTime))  # 0
  return(df)
}

#' Read in all NCCA water chemistry from 2015
#'
#' @description
#' `.loadNCCAwq2015` returns water chemistry data measured during the NCCA study in 2015
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param NCCAwq2015 a string specifying the directory of the data
#' @return dataframe
.loadNCCAwq2015 <- function(NCCAwq2015, NCCAsites2015, namingFile, n_max = Inf) {
  sites <- readr::read_csv(NCCAsites2015, show_col_types = FALSE) %>%
    # cutdown number of lats and longs
    dplyr::select(
      UID,
      SITE_ID,
      Latitude = LAT_DD83,
      Longitude = LON_DD83,
      stationDepth = STATION_DEPTH,
      WTBDY_NM = GREAT_LAKE
    ) %>%
    tidyr::drop_na() %>%
    dplyr::distinct()

  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units) %>%
    dplyr::distinct()

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor)) %>%
    dplyr::distinct() # Duplicate rows

  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NCCA_Map", na.strings = c("", "NA")) %>%
    # remove nas from table to remove ambiguities on joining
    dplyr::mutate(
      ANALYTE = ifelse(is.na(ANALYTE), ANL_CODE, ANALYTE),
      ANL_CODE = ifelse(is.na(ANL_CODE), ANALYTE, ANL_CODE)#,
      # Methods = ifelse(is.na(Methods), Study, Methods) # Removing to see if still works - prefer to not fill in Methods if not available (keep as NA - don't seem to need it, joins fine.
    ) %>%
    dplyr::distinct()

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
      ReportedUnits = RESULT_UNITS,
      sampleID = SAMPLE_ID,
      batchID = BATCH_ID,
    ) %>%
    dplyr::select(-STUDY, -VISIT_NO, -YEAR, -INDEX_NCCA15, -PUBLICATION_DATE, -PSTL_CODE, -NCCA_REG) %>%
    dplyr::mutate(
      sampleDateTime = lubridate::dmy_hm(paste(sampleDateTime, "12:00")),
      QAcode = ifelse(is.na(QAcode), "T", paste(QAcode, "T", sep = "; ")),
      QAcomment = ifelse(is.na(QAcomment), "Time imputed as noon", paste(QAcomment, "Time imputed as noon", sep = "; ")),
      # [x] KV: Note that sampleDateTime here does not have a time, only a date. Is time imputed somewhere? If so, it needs a flag
    )
  
  # Derive NH3 + NH4 for consistency across datasets
  nhDf <- df %>%
    dplyr::filter(ANALYTE %in% c("NITRITE_N", "NITRATE_N")) %>%
    dplyr::reframe(
      .by = c(UID, siteID, LAB, sampleDateTime, sampleID),
      NO3 = mean(ifelse(ANALYTE == "NITRITE_N", RESULT, NA), na.rm = T),
      NO4 = mean(ifelse(ANALYTE == "NITRATE_N", RESULT, NA), na.rm = T),
      NO3mdl = mean(ifelse(ANALYTE == "NITRITE_N", MDL, NA), na.rm = T),
      NO4mdl = mean(ifelse(ANALYTE == "NITRATE_N", MDL, NA), na.rm = T),
      NO3lrl = mean(ifelse(ANALYTE == "NITRITE_N", LRL, NA), na.rm = T),
      NO4lrl = mean(ifelse(ANALYTE == "NITRATE_N", LRL, NA), na.rm = T),
      # [x] KV: Add together the MDLs for nitrate and nitrite here, too. Looks like there are 15 cases where they both exist, and where they are both non-detects. Doesn't look like LRL are available for both, so just do MDL.
      RESULT = NO3 + NO4,
      MDL = NO3mdl + NO4mdl,
      LRL = NO3lrl + NO4lrl,
      ANALYTE = "Diss_NOx",
      ReportedUnits = "mgL",
      METHOD = toString(unique(METHOD)),
      LAB = toString(unique(LAB)),
      QAcode = toString(unique(QAcode)),
      QAcomment = toString(unique(QAcomment)),
    ) %>% 
    dplyr::select(-c(NO3, NO4, NO3mdl, NO4mdl, NO3lrl, NO4lrl))
    # check <- df %>% filter(!is.na(MDL_NITRITE_N) & !is.na(MDL_NITRATE_N))
    # You can see these cases and that they are non-detects. Please add together the MDLs in the same manner as RESULT.
    # - saw that the mdls appear to be adding up correctly
  
  df <- df %>% 
    dplyr::filter(! ANALYTE %in% c("NITRITE_N", "NITRATE_N")) %>%
    dplyr::bind_rows(nhDf) %>%
    # [x] KV: Redo/address the pivoting issues here, per comments above.
    dplyr::mutate(
      ReportedUnits = dplyr::case_when(
        ANALYTE == "COND" ~ "uscm", # this is specific conductance at 25C
        ANALYTE == "TKN" ~ "mgL",
        ANALYTE == "CHLORIDE" ~ "mgL",
        ANALYTE == "Alkalinity" ~ "mgL",
        ANALYTE == "SULFATE" ~ "mgL",
        ANALYTE == "PH" ~ "unitless",
        ANALYTE == "SILICA" ~ "mgL", 
        ANALYTE == "Diss_NOx" ~ "mgL", 
        ANALYTE == "SRP" ~ "mgL", 
        ANALYTE == "CHLA" ~ "ugL",
        ANALYTE == "AMMONIA_N" ~ "mgL", 
        ANALYTE == "PTL" ~ "mgL",
        ANALYTE == "DIN" ~ "mgL",
        ANALYTE == "NTL" ~ "mgL"
        ),
      # All NCCA WQ samples at 0.5m
      sampleDepth = 0.5,
      Study = "NCCA_WChem_2015",
      # cleaning up flags ending with empty characters
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
      LAB,
      ANALYTE,
      LRL,
      MDL,
      METHOD,
      QAcode, QAcomment,
      RESULT,
      ReportedUnits,
      sampleDepth,
      Study
    ) %>%
    dplyr::left_join(sites) %>%
    # Do this for the joining
    dplyr::left_join(renamingTable, by = c("Study", "ANALYTE", "METHOD" = "Methods")) %>%
    dplyr::filter(CodeName != "Remove") %>% 
    dplyr::left_join(key, by = join_by(CodeName)) %>%
    dplyr::mutate(
      ReportedUnits = tolower(ReportedUnits)) %>%  # Adding this because I don't understand why it's not failing based on matching on case
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::mutate(RESULT = ifelse(is.na(ConversionFactor), RESULT, RESULT * ConversionFactor),
                  MDL = ifelse(!is.na(ConversionFactor), MDL * ConversionFactor, MDL),
                  LRL = ifelse(!is.na(ConversionFactor), LRL * ConversionFactor, LRL))
  # missingness/joining checks in output:
  # mean(is.na(df$CodeName)): 0
  # mean(df$CodeName == "Remove"): 0
  # mean(is.na(df$TargetUnits)): 0
  # df %>% filter(ReportedUnits != TargetUnits) %>% reframe(mean(is.na(ConversionFactor))): 0
  # mean(is.na(df$sampleDateTime))  # 0
  return(df)
}



##### NOTE KV HAS NOT REVIEWED THE 2020 WATER CHEMISTRY FUNCTION BELOW ####
# [ ] Should this be removed ( I see commments in this function now )



#' Read in all NCCA water chemistry from 2020
#'
#' @description
#' `.loadNCCAwq2020` returns water chemistry data measured during the NCCA study in 2020
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param NCCAwq2020 a string specifying the directory of the data
#' @param NCCAsites2020 a string specifying the directory of the data
#' @return dataframe
.loadNCCAwq2020 <- function(NCCAwq2020, NCCAsites2020, namingFile, n_max = Inf) {
  sites <- readr::read_csv(NCCAsites2020, show_col_types = FALSE) %>%
    dplyr::distinct()

  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units) %>%
    unique() # Duplicate rows

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))%>% 
    dplyr::distinct()
  
  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NCCA_Map", na.strings = c("", "NA")) %>%
    # remove nas from table to remove ambiguities on joinging
    dplyr::mutate(
      ANALYTE = ifelse(is.na(ANALYTE), ANL_CODE, ANALYTE),
      ANL_CODE = ifelse(is.na(ANL_CODE), ANALYTE, ANL_CODE),
      Methods = ifelse(is.na(Methods), Study, Methods)
    ) %>%
    dplyr::distinct()


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
    )
  # compute NOx to be consitent with other data sources
  noDf <- df %>%
    dplyr::filter(ANALYTE %in% c("NITRITE_N", "NITRATE_N", "NITRATE_NITRITE_N")) %>%
    dplyr::reframe(
      .by = c(UID, SITE_ID, LAB, sampleDateTime),
      NO3 = mean(ifelse(ANALYTE == "NITRITE_N", RESULT, NA), na.rm = T),
      NO4 = mean(ifelse(ANALYTE == "NITRATE_N", RESULT, NA), na.rm = T),
      NOx = mean(ifelse(ANALYTE == "NITRATE_NITRITE_N", RESULT, NA), na.rm = T),
      NO3mdl = mean(ifelse(ANALYTE == "NITRITE_N", MDL, NA), na.rm = T),
      NO4mdl = mean(ifelse(ANALYTE == "NITRATE_N", MDL, NA), na.rm = T),
      NOxmdl = mean(ifelse(ANALYTE == "NITRATE_NITRITE_N", MDL, NA), na.rm = T),
      NO3rl = mean(ifelse(ANALYTE == "NITRITE_N", RL, NA), na.rm = T),
      NO4rl = mean(ifelse(ANALYTE == "NITRATE_N", RL, NA), na.rm = T),
      NOxrl = mean(ifelse(ANALYTE == "NITRATE_NITRITE_N", RL, NA), na.rm = T),
      RESULT = ifelse(is.na(NOx), NO3 + NO4, NOx),
      MDL = ifelse(is.na(NOxmdl), NO3mdl + NO4mdl, NOxmdl),
      RL = ifelse(is.na(NOxrl), NO3rl + NO4rl, NOxrl),
      ANALYTE = "Diss_NOx",
      UNITS = "mgl",
      QAcode = toString(unique(QAcode)),
      QAcomment = toString(unique(QAcomment)),
    ) %>%
    dplyr::select(-c(NO3, NO4, NOx, NO3mdl, NO4mdl, NOxmdl, NO3rl, NO4rl, NOxrl))

  df <- df %>%
    dplyr::filter(! ANALYTE %in% c("NITRITE_N", "NITRATE_N", "NITRATE_NITRITE_N")) %>%
    dplyr::bind_rows(noDf) %>%
    # Assert reported units
    dplyr::mutate(
      ReportedUnits = dplyr::case_when(
        ANALYTE == "COND" ~ "uscm", # [ ] is this specific conductance at 25C
        ANALYTE == "TKN" ~ "mgL",
        ANALYTE == "CHLORIDE" ~ "mgL",
        ANALYTE == "Alkalinity" ~ "mgL",
        ANALYTE == "SULFATE" ~ "mgL",
        ANALYTE == "PH" ~ "unitless",
        ANALYTE == "SILICA" ~ "mgL",
        ANALYTE == "Diss_NOx" ~ "mgL", # Original: mg N/L Might need to convert this
        ANALYTE == "SRP" ~ "mgL", # Original: mg P/L Might need to convert
        ANALYTE == "CHLA" ~ "ugL",
        ANALYTE == "AMMONIA_N" ~ "mgL", # Original: mg N/L Might need to convert
        ANALYTE == "PTL" ~ "mgL",
        ANALYTE == "DIN" ~ "mgL"
      ),
      sampleDepth = 0.5,
      Study = "NCCA_WChem_2020"
    ) %>%
    # [x] still >50% missingness  in lat/lon
    # this is due to coastal ocean measures
    dplyr::left_join(sites) %>%
    tidyr::drop_na(LON_DD) %>%
    # Do this for the joining
    dplyr::left_join(renamingTable, by = c("Study", "ANALYTE")) %>%
    dplyr::filter(CodeName != "Remove") %>% 
    dplyr::left_join(key, by = join_by(CodeName)) %>%
    dplyr::mutate(
      ReportedUnits = tolower(ReportedUnits)) %>%  # Adding this because I don't understand why it's not failing based on matching on case
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::mutate(
      RESULT = ifelse(is.na(ConversionFactor), RESULT, RESULT * ConversionFactor),
      MDL = ifelse(!is.na(ConversionFactor), MDL * ConversionFactor, MDL),
      RL = ifelse(!is.na(ConversionFactor), RL * ConversionFactor, RL),
    )

  # missingness/joining checks in output:
  # mean(is.na(df$CodeName)) # 0
  # mean(df$CodeName == "Remove") # 0
  # mean(is.na(df$TargetUnits)) # 0
  # df %>% filter(ReportedUnits != TargetUnits) %>% reframe(mean(is.na(ConversionFactor))) # 0
  # mean(is.na(df$sampleDateTime))  # 0
  return(df)
}
