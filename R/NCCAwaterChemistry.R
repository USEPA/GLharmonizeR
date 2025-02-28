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
  sites <- .loadNCCASite2010(NCCAsites2010) #%>%
    # dplyr::mutate(SITE_ID = stringr::str_extract(SITE_ID, "\\d{3,4}$"))
  # Do not alter the SITE_ID to avoid confusion with original source
  
  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))

  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NCCA_Map", na.strings = c("", "NA")) %>%
    # remove nas from table to remove ambiguities on joining
    dplyr::mutate(
      ANALYTE = ifelse(is.na(ANALYTE), ANL_CODE, ANALYTE),
      ANL_CODE = ifelse(is.na(ANL_CODE), ANALYTE, ANL_CODE)#,
      # Methods = ifelse(is.na(Methods), Study, Methods) # There are no methods reported for GL in 2010
      # Any methods reported in NCCA_Map are a result of not originally filtering the dataset to GL - shouldn't be needed
    )

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
    ) %>%
    # All NCCA WQ samples at 0.5m
    dplyr::mutate(
      sampleDepth = 0.5,
      # [x] add this to Analytes3
      Study = "NCCA_WChem_2010",
      # QACODE =ifelse((STATE=="WI") & (PARAMETER == "CHLA"), QACODE, paste(QACODE, sep = "; ", "WSLH"))
      ANL_CODE = ifelse(is.na(ANL_CODE), ANALYTE, ANL_CODE),
      ANALYTE = ifelse(is.na(ANALYTE), ANL_CODE, ANALYTE)
    ) %>%
    dplyr::rename(
      QAcode = QACODE,
      ReportedUnits = UNITS
    ) %>% 
    # Note that methods are all NA for GL sites but leaving as-is for generality
    dplyr::left_join(renamingTable, by = c("Study", "ANALYTE", "ANL_CODE", "METHOD" = "Methods")) %>%
    dplyr::left_join(key, by = dplyr::join_by(CodeName)) %>%
    dplyr::filter(CodeName != "Remove") %>%
    dplyr::mutate(
      ReportedUnits = stringr::str_remove(ReportedUnits, "/"),
      # ReportedUnits = stringr::str_remove(ReportedUnits, "\\\\"),
      ReportedUnits = tolower(ReportedUnits)) %>% 
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::mutate(
      RESULT = ifelse(is.na(ConversionFactor), RESULT, RESULT * ConversionFactor),
      MDL = ifelse(!is.na(ConversionFactor), MDL * ConversionFactor, MDL),
      MRL = ifelse(!is.na(ConversionFactor), MRL * ConversionFactor, MRL)
      ) %>%
    dplyr::left_join(sites) 

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
    tidyr::drop_na()

  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))

  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NCCA_Map", na.strings = c("", "NA")) %>%
    # remove nas from table to remove ambiguities on joining
    dplyr::mutate(
      ANALYTE = ifelse(is.na(ANALYTE), ANL_CODE, ANALYTE),
      ANL_CODE = ifelse(is.na(ANL_CODE), ANALYTE, ANL_CODE)#,
      # Methods = ifelse(is.na(Methods), Study, Methods) # Removing to see if still works - prefer to not fill in Methods if not available (keep as NA - don't seem to need it, joins fine.
    )

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
      sampleDateTime = lubridate::dmy(sampleDateTime)
    ) %>%
    
    ## **** As mentioned in the comments for .loadNCCAhydro2010(), I don't think these approaches of pivoting the whole dataset to do calculations is working well and is introducing problems. I would suggest instead splitting out the data that you need to do manipulations on (nitrate and nitrite) and dealing with them separately, them joining them back in. ***********
    tidyr::pivot_wider(id_cols = c(UID:sampleDateTime), names_from = ANALYTE, values_from = LAB:sampleID) %>% # 4705 rows
    # Combine Nitrate and Nitrite
    dplyr::mutate(
      # Hide result in Nitrate so don't need to make all of the other columns
      RESULT_NITRATE_N =  RESULT_NITRITE_N + RESULT_NITRATE_N,
    ) %>%
    tidyr::pivot_longer(cols= LAB_PH:sampleID_SILICA, names_pattern = "^([[:alpha:]]*)_(.*)$", names_to = c(".value", "ANL_CODE"), names_repair = "unique") %>%
    # if no result or comment, this is created by the pivot_wider and needs to be removed
    dplyr::filter(!is.na(RESULT) | !is.na(QAcode) | !is.na(QAcomment)) %>% # 4680 rows
    # **** End up with 25 fewer rows than began with -- can't figure out why ****
    dplyr::mutate(
      # Change the names
      ANL_CODE = dplyr::case_when(
        ANL_CODE == "NITRATE_N" ~ "Diss_NOx",
        .default = ANL_CODE
      ),
      ReportedUnits = dplyr::case_when(
        ANL_CODE == "COND" ~ "uscm", # this is specific conductance at 25C
        ANL_CODE == "TKN" ~ "mgL",
        ANL_CODE == "CHLORIDE" ~ "mgL",
        ANL_CODE == "Alkalinity" ~ "mgL",
        ANL_CODE == "SULFATE" ~ "mgL",
        ANL_CODE == "PH" ~ "unitless",
        ANL_CODE == "SILICA" ~ "mgL", 
        ANL_CODE == "Diss_NOx" ~ "mgL", 
        ANL_CODE == "SRP" ~ "mgL", 
        ANL_CODE == "CHLA" ~ "ugL",
        ANL_CODE == "AMMONIA_N" ~ "mgL", 
        ANL_CODE == "PTL" ~ "mgL",
        ANL_CODE == "DIN" ~ "mgL",
        ANL_CODE == "NTL" ~ "mgL"
        )
    ) %>%
    # All NCCA WQ samples at 0.5m
    dplyr::mutate(
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
      ANL_CODE,
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
    dplyr::mutate(
      ANALYTE = ANL_CODE,
      # METHOD = ifelse(is.na(METHOD), Study, METHOD),
    ) %>%
    dplyr::left_join(renamingTable, by = c("Study", "ANALYTE", "ANL_CODE", "METHOD" = "Methods")) %>%
    dplyr::filter(CodeName != "Remove") %>% 
    dplyr::left_join(key, by = join_by(CodeName)) %>%
    dplyr::mutate(
      ReportedUnits = tolower(ReportedUnits)) %>%  # Adding this because I don't understand why it's not failing based on matching on case
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::mutate(RESULT = ifelse(is.na(ConversionFactor), RESULT, RESULT * ConversionFactor),
                  MDL = ifelse(!is.na(ConversionFactor), MDL * ConversionFactor, MDL),
                  LRL = ifelse(!is.na(ConversionFactor), LRL * ConversionFactor, LRL))

  return(df)
}



##### NOTE KV HAS NOT REVIEWED THE 2020 WATER CHEMISTRY FUNCTION BELOW ####



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
    # keeping enough to add station depth information for a given sampling event (respecting visit number)
    dplyr::select(
      UID,
      STATION_DEPTH,
      # keeping for potential filtering
      EPA_REG, GREAT_LAKE, LAKE_REG, NCCA_REG, NPS_PARK
    )  %>%
    dplyr::distinct()

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
    ) %>%
    dplyr::filter(CodeName != "Remove")
    
    # [ ] TODO converrt units

  return(df)
}
