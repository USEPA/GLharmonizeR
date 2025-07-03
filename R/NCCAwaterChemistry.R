#' Read in NCCA water chemistry from 2010
#'
#' @description
#' `.loadNCCAwq2010` returns water chemistry data measured during the NCCA study in 2010
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param NCCAwq2010 a string specifying the URL for the data
#' @param NCCAsites2010 a string specifying the URL for the site data
#' @param namingFile a string specifying the URL for the analyte naming file
#' @param n_max Number of rows to read in from the data file (this is just for testing purposes)
#'
#' @return dataframe
.loadNCCAwq2010 <- function(NCCAwq2010, NCCAsites2010, namingFile, n_max = Inf) {

  sites <- .loadNCCASite2010(NCCAsites2010)
  # Instead of joining site info, will fill in NCCAwq2010 site info using NCCAhydrofiles2010 data in NCCAfulljoin.R (.loadNCCA) because the stationDepths were checked for accuracy in the 2010 hydro data (compared to CTD depths) and want station info to match

  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor)) %>%
    dplyr::distinct() # Duplicate rows

  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NCCA_Map", na.strings = c("", "NA")) %>%
    # remove nas from table to remove ambiguities on joining
    dplyr::mutate(
      ANALYTE = ifelse(is.na(ANALYTE), ANL_CODE, ANALYTE),
      ANL_CODE = ifelse(is.na(ANL_CODE), ANALYTE, ANL_CODE)#,
      # KV: Any methods reported in NCCA_Map are a result of not originally filtering the dataset to GL - shouldn't be needed
    )  %>%
    dplyr::select(-Units) # Should remove Units from these renamingTables so they don't cause confusion with the units parsed/read from the data. Units in renaming tables are prone to human error.



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
      )
    ) %>%
    # Remove sites that aren't Great Lakes (i.e., do not have "GL" in the SITE_ID)
    dplyr::filter(grepl("GL",SITE_ID)) %>%
    dplyr::rename(
      ANL_CODE = PARAMETER,
      ANALYTE = PARAMETER_NAME,
      sampleDate = DATE_COL,
      QAcode = QACODE,
      ReportedUnits = UNITS
    ) %>%
    # All NCCA WQ samples at 0.5m
    dplyr::mutate(
      sampleDate = lubridate::ymd(sampleDate),
      sampleDepth = 0.5,
      Study = "NCCA_WChem_2010",
      # QACODE =ifelse((STATE=="WI") & (PARAMETER == "CHLA"), QACODE, paste(QACODE, sep = "; ", "WSLH")) # See flagsMap: likely will not be able to flag pore size issue for Chla for WSLH data because these data don't have lab, and we don't want to flag all WI samples
      ANL_CODE = ifelse(is.na(ANL_CODE), ANALYTE, ANL_CODE),
      ANALYTE = ifelse(is.na(ANALYTE), ANL_CODE, ANALYTE)
    ) %>%
    dplyr::select(-PQL) %>% # No PQLs for GLs
    # Note that methods are all NA for GL sites but leaving as-is for generality
    dplyr::left_join(renamingTable, by = c("Study", "ANALYTE", "ANL_CODE", "METHOD" = "Methods")) %>% # sum(is.na(df$CodeName))
    dplyr::filter(CodeName != "Remove") %>%
    dplyr::left_join(key, by = dplyr::join_by(CodeName)) %>% # sum(is.na(df$TargetUnits))
    dplyr::mutate(
      ReportedUnits = stringr::str_remove(ReportedUnits, "/"),
      ReportedUnits = tolower(ReportedUnits)) %>%
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    # df %>% dplyr::filter(ReportedUnits != TargetUnits) %>% dplyr::reframe(sum(is.na(ConversionFactor)))
    dplyr::mutate(
      RESULT = ifelse(is.na(ConversionFactor), RESULT, RESULT * ConversionFactor),
      MDL = ifelse(!is.na(ConversionFactor), MDL * ConversionFactor, MDL),
      MRL = ifelse(!is.na(ConversionFactor), MRL * ConversionFactor, MRL)
      ) #%>%
    # dplyr::left_join(sites)

  # Instead of joining site info, will fill in NCCAwq2010 site info using NCCAhydrofiles2010 data in NCCAfulljoin.R (.loadNCCA) because the stationDepths were checked for accuracy in the 2010 hydro data  (compared to CTD depths) and want station info to match

  # Note that I used hydro file for station depth in all cases except 2

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
#' @param NCCAwq2015 a string specifying the URL for the data
#' @param NCCAsites2015 a string specifying the URL for the site data
#' @param namingFile a string specifying the URL for the analyte naming file
#' @param n_max Number of rows to read in from the data file (this is just for testing purposes)
#'
#' @return dataframe
.loadNCCAwq2015 <- function(NCCAwq2015, NCCAsites2015, namingFile, n_max = Inf) {

  sites <- .loadNCCASite2015(NCCAsites2015)

  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor)) %>%
    dplyr::distinct() # Duplicate rows

  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NCCA_Map", na.strings = c("", "NA")) %>%
    # remove nas from table to remove ambiguities on joining
    dplyr::mutate(
      ANALYTE = ifelse(is.na(ANALYTE), ANL_CODE, ANALYTE),
      ANL_CODE = ifelse(is.na(ANL_CODE), ANALYTE, ANL_CODE)#,
    )  %>%
    dplyr::select(-Units) # Should remove Units from these renamingTables so they don't cause confusion with the units parsed/read from the data. Units in renaming tables are prone to human error.



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
      sampleDate = DATE_COL,
      QAcode = NARS_FLAG,
      QAcomment = NARS_COMMENT,
      ReportedUnits = RESULT_UNITS,
      sampleID = SAMPLE_ID,
      batchID = BATCH_ID,
    ) %>%
    dplyr::select(-STUDY, -VISIT_NO, -YEAR, -INDEX_NCCA15, -PUBLICATION_DATE, -PSTL_CODE, -NCCA_REG, -batchID) %>%
    dplyr::mutate(
      sampleDate = lubridate::dmy(sampleDate),
    )



  # Derive Diss_NOx as NH3 + NH4 for consistency across datasets
  nhDf <- df %>%
    dplyr::filter(ANALYTE %in% c("NITRITE_N", "NITRATE_N")) %>%
    dplyr::reframe(
      .by = c(UID, siteID, LAB, sampleDate, sampleID),
      NO3 = mean(ifelse(ANALYTE == "NITRITE_N", RESULT, NA), na.rm = T),
      NO4 = mean(ifelse(ANALYTE == "NITRATE_N", RESULT, NA), na.rm = T),
      NO3mdl = mean(ifelse(ANALYTE == "NITRITE_N", MDL, NA), na.rm = T),
      NO4mdl = mean(ifelse(ANALYTE == "NITRATE_N", MDL, NA), na.rm = T),
      NO3lrl = mean(ifelse(ANALYTE == "NITRITE_N", LRL, NA), na.rm = T),
      NO4lrl = mean(ifelse(ANALYTE == "NITRATE_N", LRL, NA), na.rm = T),
      # Add together the MDLs for nitrate and nitrite here, too. Looks like there are 15 cases where they both exist, and where they are both non-detects.
      RESULT = NO3 + NO4,
      MDL = NO3mdl + NO4mdl,
      LRL = NO3lrl + NO4lrl, # All NA - doesn't look like LRL are ever available for both
      ANALYTE = "Diss_NOx",
      ReportedUnits = "mgL",
      METHOD = toString(unique(METHOD)),
      LAB = toString(unique(LAB)),
      QAcode = toString(unique(QAcode)),
      QAcomment = toString(unique(QAcomment)),
    ) %>%
    # Remove values where only NO3 or NO4 was a nondetect but not both, or where only one value was available (all WSLH have only nitrate). So remove cases where one is NA and the other isn't - need to either both be missing with an MDL reported, or both have measured values.
    dplyr::filter( !( (!is.na(NO3) & is.na(NO4)) |  (is.na(NO3) & !is.na(NO4))  ) ) %>%
    # Confirmed no QAcodes with 'ND, NA'
    # Confirmed if RESULT is NA, should have an ND QAcode and an MDL
    # check <- nhDf %>% dplyr::filter(is.na(RESULT) )
    dplyr::select(-c(NO3, NO4, NO3mdl, NO4mdl, NO3lrl, NO4lrl)) %>%
    # Need to replace METHOD "NA" with NA - renamingTable join was failing
    dplyr::mutate(
      METHOD = ifelse(METHOD=="NA", NA, METHOD)
    )



  df <- df %>%
    dplyr::filter(! ANALYTE %in% c("NITRITE_N", "NITRATE_N")) %>%
    dplyr::bind_rows(nhDf) %>%
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
      sampleDate,
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
    # add station info
    dplyr::left_join(sites, by = c("UID", "SITE_ID")) %>%
    # Note that there are cases where the stationDepth in the secchi and hydro files do not match the sites file. Sites file stationDepth appears to be more accurate and lines up with original secchi CTB flags, so use this one
    # Used sites stationDepth for hydro and secchi, so fine to use it here. Note that stationDepth is not a column in the 2015 water chem dataset, so don't need to choose between them
    dplyr::left_join(renamingTable, by = c("Study", "ANALYTE", "METHOD" = "Methods")) %>% # sum(is.na(df$CodeName))
    dplyr::filter(CodeName != "Remove") %>% # Pretty sure this removes analytes not mapping so need to do the check for missing CodeNames first
    dplyr::left_join(key, by = dplyr::join_by(CodeName)) %>% # sum(is.na(df$TargetUnits))
    dplyr::mutate(
      ReportedUnits = tolower(ReportedUnits)
      ) %>%
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    # df %>% dplyr::filter(ReportedUnits != TargetUnits) %>% dplyr::reframe(sum(is.na(ConversionFactor)))
    dplyr::mutate(RESULT = ifelse(is.na(ConversionFactor), RESULT, RESULT * ConversionFactor),
                  MDL = ifelse(!is.na(ConversionFactor), MDL * ConversionFactor, MDL),
                  LRL = ifelse(!is.na(ConversionFactor), LRL * ConversionFactor, LRL))


    return(df)
}









##### NOTE KV HAS NOT REVIEWED THE 2020 WATER CHEMISTRY FUNCTION BELOW ####
# *It is not currently being used in the package*



#' Read in all NCCA water chemistry from 2020
#'
#' @description
#' `.loadNCCAwq2020` returns water chemistry data measured during the NCCA study in 2020
#'
#' @details
#' THIS FUNCTION HAS NOT BEEN REVIEWED. This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param NCCAwq2020 a string specifying the URL for the data
#' @param NCCAsites2020 a string specifying the URL for the data
#' @param namingFile a string specifying the URL for the analyte naming file
#' @param n_max Number of rows to read in from the data file (this is just for testing purposes)
#' @return dataframe
.loadNCCAwq2020 <- function(NCCAwq2020, NCCAsites2020, namingFile, n_max = Inf) {
  sites <- readr::read_csv(NCCAsites2020, show_col_types = FALSE) %>%
    dplyr::distinct()

  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units) %>%
    dplyr::distinct() # Duplicate rows

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
    dplyr::left_join(key, by = dplyr::join_by(CodeName)) %>%
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
