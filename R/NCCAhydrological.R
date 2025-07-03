
#' Load and join NCCA 2010 hydrographic data from online csv files
#'
#' @description
#' `.loadNCCAhydro2010` returns a dataframe of all of the hydrographic data relating to NCCA 2010
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param NCCAhydrofiles2010 a string specifying the URL for the data
#' @param NCCAsites2010 a string specifying the URL for the site data
#' @param namingFile a string specifying the URL for the analyte naming file
#' @param n_max Number of rows to read in from the data file (this is just for testing purposes)
#'
#' @return dataframe
.loadNCCAhydro2010 <- function(NCCAhydrofiles2010, NCCAsites2010, namingFile, n_max = n_max) {

  sites <- .loadNCCASite2010(NCCAsites2010)

  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor)) %>%
    dplyr::distinct() # Duplicate rows

  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NCCA_Map", na.strings = c("", "NA")) %>%
    # remove NAs from table to avoid ambiguities on joining
    dplyr::mutate(
      ANALYTE = ifelse(is.na(ANALYTE), ANL_CODE, ANALYTE),
      ANL_CODE = ifelse(is.na(ANL_CODE), ANALYTE, ANL_CODE)#,
      # Any methods reported in NCCA_Map are a result of not originally filtering the dataset to GL - shouldn't be needed
    ) %>%
    dplyr::select(-Units) # Should remove Units from these renamingTables so they don't cause confusion with the units parsed/read from the data. Units in renaming tables are prone to human error.



  df <- NCCAhydrofiles2010 %>%
    purrr::map_dfr(readr::read_csv, n_max = n_max, show_col_types = FALSE) %>%
    # Remove sites that aren't Great Lakes (i.e., do not have "GL" in the SITE_ID)
    dplyr::filter(grepl("GL",SITE_ID)) %>%
    # filter to just downcast (include IM_CALC because that's where Secchi is)
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
      UNITS = ifelse(ANALYTE == "Turbidity", "unknown", UNITS),
      sampleDepth = ifelse(sampleDepth == -9, NA, sampleDepth)
    ) %>%
    # Note: CAST_COMMENT and CAST_FLAG are all NA
    # Note: Averaging over RESULT is necessary below because sometimes multiple values are present at same depth. Looking at data, this looks like due to possible data entry error due to upcasts being mislabeled, but this is a reasonable solution.
    dplyr::reframe(.by = UID:ANALYTE,
                   RESULT = mean(RESULT, na.rm = T),
                   dplyr::across(UNITS:QAcomment, function(x) toString(unique(x)))) %>%
    # Also fill in stationDepth for rows where missing - and sometimes slight discrepancy between station depth for Kd/secchi and surface - Kd/secchi seems to match site file - surface seems related to CTD data - use max for now and coalesce with site info file later
    dplyr::mutate(.by = c(SITE_ID, DATE_COL, UID),
                  stationDepth = max(stationDepth, na.rm = T)) %>%
    dplyr::mutate(sampleDate = lubridate::mdy(DATE_COL)) # no times



  # Calculate CPAR by splitting out separately, then rejoin below
  parDf <- df %>%
    dplyr::filter(ANALYTE %in% c("Ambient PAR", "Underwater PAR")) %>%
    # derive cpar
    dplyr::summarize(
      .by = -c(RESULT, ANALYTE, UNITS, QAcode, QAcomment),
      APAR = mean(ifelse(ANALYTE == "Ambient PAR", RESULT, NA), na.rm = T),
      UPAR = mean(ifelse(ANALYTE == "Underwater PAR", RESULT, NA), na.rm = T),
      RESULT = UPAR / APAR,
      ANALYTE = "Corrected PAR",
      # stationDepth = mean(stationDepth, na.rm = T),
      UNITS = "percent",
      QAcode = toString(unique(QAcode)),
      QAcomment = toString(unique(QAcomment)),
    ) %>%
    dplyr::select(-c(APAR, UPAR)) %>%
    dplyr::filter(!is.na(RESULT))
  # 3 NA values because UW PAR is missing



  df <- df %>% dplyr::filter(! ANALYTE %in% c("Ambient PAR", "Underwater PAR")) %>%
    dplyr::bind_rows(parDf) %>%
    # Replace -9 secchi values with NA
    dplyr::mutate(RESULT = ifelse(RESULT == -9 & ANALYTE == "Mean secchi", NA, RESULT)) %>%
    # Below line doesn't remove anything, but ensures there are no missing values without a QAcode present
    dplyr::filter(!(is.na(RESULT) & is.na(QAcode))) %>%
    dplyr::mutate(
      Study = "NCCA_hydro_2010",
      UNITS = ifelse(ANALYTE == "Conductivity", "uscm", UNITS) # adding because there's a weird special character
    ) %>%
    # add station info
    dplyr::left_join(sites, by = "SITE_ID") %>%
    # Resolve discrepancies in stationDepth between site file and data file
    dplyr::mutate(stationDepth = ifelse(SITE_ID %in% c("NCCAGL10-1095", "NCCAGL10-GLBA10-165"), stationDepth.y, stationDepth.x)) %>%
    # look <- df2 %>% dplyr::select(SITE_ID, WTBDY_NM, stationDepth.x, stationDepth.y, stationDepth) %>% unique() %>% dplyr::filter(abs(stationDepth.x - stationDepth.y) > 1)
    # "NCCAGL10-1124"  stationDepth.x correct   (larger)
    # "NCCAGL10-GLBA10-165"   stationDepth.y correct (smaller)
    # "NCCAGL10-GLBA10-026" stationDepth.x correct (larger)
    # "NCCAGL10-1184"    stationDepth.x correct (smaller)
    # "NCCAGL10-1213"       stationDepth.x correct (larger)
    # "NCCAGL10-1095"    stationDepth.y correct (smaller)
    dplyr::select(-c(stationDepth.x, stationDepth.y)) %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    dplyr::left_join(renamingTable, by = c("ANALYTE", "Study")) %>% # sum(is.na(df2$CodeName))
    dplyr::filter(!grepl("remove", CodeName, ignore.case=T))  %>%
    dplyr::left_join(key) %>% # sum(is.na(df2$TargetUnits))
    dplyr::mutate(
      ReportedUnits = as.character(ReportedUnits),
      ReportedUnits = stringr::str_remove(ReportedUnits, "/"),
      ReportedUnits = tolower(ReportedUnits)) %>% #
    dplyr::left_join(conversions) %>%
    # df2 %>% dplyr::filter(ReportedUnits != TargetUnits) %>% dplyr::reframe(sum(is.na(ConversionFactor)))
    dplyr::mutate(RESULT = ifelse(is.na(ConversionFactor), RESULT, RESULT * ConversionFactor))

  return(df)
}





#' Load and join NCCA 2015 hydrographic data from online csv files
#'
#' @description
#' `.loadNCCAhydro2015` returns a dataframe of all of the hydrographic data relating to NCCA 2010
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param NCCAhydrofile2015 a string specifying the URL for the data
#' @param NCCAsites2015 a string specifying the URL for the site data
#' @param namingFile a string specifying the URL for the analyte naming file
#' @param n_max Number of rows to read in from the data file (this is just for testing purposes)
#'
#' @return dataframe
.loadNCCAhydro2015 <- function(NCCAhydrofile2015, NCCAsites2015, namingFile, n_max = Inf) {

  sites <- .loadNCCASite2015(NCCAsites2015)

  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor)) %>%
    dplyr::distinct() # Duplicate rows

  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NCCA_Map", na.strings = c("", "NA")) %>%
    # remove nas from table to remove ambiguities on joinging
    dplyr::mutate(
      ANALYTE = ifelse(is.na(ANALYTE), ANL_CODE, ANALYTE),
      ANL_CODE = ifelse(is.na(ANL_CODE), ANALYTE, ANL_CODE)
    ) %>%
    dplyr::select(-Units) # Should remove Units from these renamingTables so they don't cause confusion with the units parsed/read from the data. Units in renaming tables are prone to human error.



  df <- readr::read_csv(NCCAhydrofile2015, n_max = n_max, show_col_types = FALSE) %>%
    dplyr::filter(CAST == "DOWNCAST") %>%
    dplyr::mutate(
      `Corrected PAR` = LIGHT_UW / LIGHT_AMB,
      sampleDate = as.Date(DATE_COL, origin = "1899-12-30"),
      Study = "NCCA_hydro_2015"
    ) %>%
  # In Excel the date is represented as the integer part of the number, and counts the number of days since 1899-12-31. For example 1 is 1900-01-01.
  # However you need to be aware there is a "bug" in Excel, it wrongly considers 1900 to be a leap year, i.e. it counts the date 1900-02-29 as a valid date. This bug was intentionally added to Excel so it would reproduce the wrong behaviour of Lotus, that was the most popular Windows spreadsheet software at the time Excel was created. So assuming you do not care about dates before 1900-03-01, then you could instead assume the number is a count of the number of days since 1899-12-30.

    # Some Corrected PAR is Inf because LIGHT_AMB==0, which must be incorrect
    # Replace Corrected PAR infinity values with NA
    dplyr::mutate(`Corrected PAR` = dplyr::na_if(`Corrected PAR`, Inf)) %>%
    dplyr::select(
      -c(LIGHT_AMB, LIGHT_UW)
    ) %>%
    dplyr::rename(sampleDepth = DEPTH,
                  stationDepth = STATION_DEPTH,
                  QAcomment= NARS_COMMENT) %>%
    tidyr::pivot_longer(c(TRANS, CONDUCTIVITY:TEMPERATURE, `Corrected PAR`), names_to = "ANALYTE", values_to = "RESULT") %>%
    # Remove NAs after pivoting - gets rid of bad CPAR and other missing data
    dplyr::filter(!is.na(RESULT)) %>%
    # Only relevant remaining comment is 'Typo - L uw? No field form available.' But this only catches one case of where UW > AMB light. There are other similar cases as well, so these won't be removed in flagsMap. Will add flag in final joined data for all cpar>100%
    # Other remaining comment (Cast and depth provided but no measurement data; measurement depth > station depth (6.6m)) will be removed in flagsMap, or will be removed because it's for light transmission, which is an analyte we are not keeping currently
    dplyr::select(-DATE_COL) %>%
    dplyr::mutate(
      UNITS = dplyr::case_when(
      # These were take from hydro 2015 metadata file
      ANALYTE == "DO" ~ "mgl",
      ANALYTE == "TEMPERATURE" ~ "c",
      ANALYTE == "CONDUCTIVITY" ~ "uscm",
      ANALYTE == "Corrected PAR" ~ "percent",
      ANALYTE == "TRANS" ~ "percent",
      ANALYTE == "PH" ~ "unitless",
    )) %>%
    dplyr::left_join(renamingTable, by = c("ANALYTE", "Study")) %>% # sum(is.na(df$CodeName))
    dplyr::filter(!grepl("remove", CodeName, ignore.case=T))  %>%
    dplyr::left_join(key) %>% # sum(is.na(df$TargetUnits))
    dplyr::rename(ReportedUnits = UNITS) %>%
    dplyr::left_join(conversions) %>%
    # df %>% dplyr::filter(ReportedUnits != TargetUnits) %>% dplyr::reframe(sum(is.na(ConversionFactor)))
    dplyr::mutate(RESULT = ifelse(is.na(ConversionFactor), RESULT, RESULT * ConversionFactor)) %>%
    # add station info
    dplyr::left_join(sites, by = c("UID", "SITE_ID")) %>%
    # Note that there are cases where the stationDepth in the secchi and hydro files do not match the sites file. Sites file stationDepth appears to be more accurate and lines up with original secchi CTB flags, so use this one
    # look <- df %>% dplyr::filter(stationDepth.x !=stationDepth.y) %>% dplyr::select(UID, SITE_ID, stationDepth.x, stationDepth.y) %>% dplyr::arrange(SITE_ID) %>% unique()
    dplyr::select(-stationDepth.x) %>%
    dplyr::rename(stationDepth=stationDepth.y)

  return(df)
}




#' Load and join Secchi data for NCCA 2015 from online csv files
#'
#' @description
#' `.loadNCCAsecchi2015` returns a dataframe of all of the joined Secchi data relating to NCCA 2015
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param NCCAsecchifile2015 a string specifying the filepath of the data
#' @param NCCAsites2015 a string specifying the URL for the site data
#' @param namingFile a string specifying the URL for the analyte naming file
#' @param n_max Number of rows to read in from the data file (this is just for testing purposes)
#'
#' @return dataframe of the fully joined Secchi data from NCCA 2015
.loadNCCAsecchi2015 <- function(NCCAsecchifile2015, NCCAsites2015, namingFile, n_max = Inf) {

  sites <- .loadNCCASite2015(NCCAsites2015)

  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor)) %>%
    dplyr::distinct() # Duplicate rows

  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NCCA_Map", na.strings = c("", "NA")) %>%
    # remove NAs from table to remove ambiguities on joining
    dplyr::mutate(
      ANALYTE = ifelse(is.na(ANALYTE), ANL_CODE, ANALYTE),
      ANL_CODE = ifelse(is.na(ANL_CODE), ANALYTE, ANL_CODE)
    ) %>%
    dplyr::select(-Units) # Should remove Units from these renamingTables so they don't cause confusion with the units parsed/read from the data. Units in renaming tables are prone to human error.



  df <- readr::read_csv(NCCAsecchifile2015, n_max = n_max, show_col_types = FALSE) %>%
    dplyr::filter(
      !grepl("missing for this site", SECCHI_COMMENT, ignore.case = T),
      !grepl("unavailable for this site", SECCHI_COMMENT, ignore.case = T)
    ) %>%
    dplyr::mutate(
      # sampleTimeUTC = round(as.numeric(SECCHI_TIME) * 24), # Time zone unknown so remove
      # GLNS15-1340 visit 1 Taken 6-13-2015 due to lost secchi
      DATE_COL = ifelse(SITE_ID=="GLNS15-1340" & VISIT_NO==1, DATE_COL+1, DATE_COL),
      sampleDate = as.Date(DATE_COL, origin = "1899-12-30"),
    ) %>%
    # In Excel the date is represented as the integer part of the number, and counts the number of days since 1899-12-31. For example 1 is 1900-01-01.
    # However you need to be aware there is a "bug" in Excel, it wrongly considers 1900 to be a leap year, i.e. it counts the date 1900-02-29 as a valid date. This bug was intentionally added to Excel so it would reproduce the wrong behaviour of Lotus, that was the most popular Windows spreadsheet software at the time Excel was created. So assuming you do not care about dates before 1900-03-01, then you could instead assume the number is a count of the number of days since 1899-12-30.
    dplyr::rename(stationDepth=STATION_DEPTH) %>%
    # This may look like we are keeping MEAN_SECCHI_DEPTH to average with the others,
    # However, we filter it out in the mean call
    tidyr::pivot_longer(c(MEAN_SECCHI_DEPTH, DISAPPEARS, REAPPEARS), names_to = "SecchiType", values_to = "RESULT") %>%
    dplyr::filter(SecchiType != "MEAN_SECCHI_DEPTH") %>%
    dplyr::summarize(
      .by = c(UID, sampleDate, SITE_ID, stationDepth, VISIT_NO),
      ANALYTE = "Secchi",
      # Not using MEAN_SECCHI_DEPTH - the estimated column (from Kd)
      # Calculate the actual mean of the disappear/reappear casts
      RESULT = mean(RESULT, na.rm = T),
      # Compress all comments
      QAcomment = toString(unique(SECCHI_COMMENT))
    ) %>%
    tidyr::drop_na(RESULT) %>%
    # KV: NAs in RESULT seem to be cases where DISAPPEAR and REAPPEAR aren't available. Often these are marked as clear to bottom, but sometimes estimated value is inconsistent with this marking.
    # Decided to remove as bad data
    dplyr::mutate(
      Study = "NCCA_secchi_2015",
      UNITS = "m"
    ) %>%
    dplyr::left_join(sites, by = c("UID", "SITE_ID")) %>%
    # Note that there are cases where the stationDepth in the secchi and hydro files do not match the sites file. Sites file stationDepth appears to be more accurate and lines up with original secchi CTB flags, so use this one
    # look <- df %>% dplyr::filter(stationDepth.x !=stationDepth.y) %>% dplyr::select(UID, SITE_ID, stationDepth.x, stationDepth.y, RESULT, QAcode) %>% dplyr::arrange(SITE_ID)
    dplyr::select(-stationDepth.x) %>%
    dplyr::rename(stationDepth=stationDepth.y) %>%
    dplyr::mutate(
      # Check if the measurements are greater than station depth (rather than use CLEAR_TO_BOTTOM column)
      CLEAR_TO_BOTTOM = RESULT >= stationDepth,
      # Looked through and saw none of the QAcomments were relevant
      # So replace with only whether clear to bottom
      QAcomment = NA,
      QAcode = dplyr::case_when(
        CLEAR_TO_BOTTOM == TRUE ~ "CTB"
      ),
      QAcomment = dplyr::case_when(
        CLEAR_TO_BOTTOM == TRUE ~ "Secchi clear to bottom"
      )
    ) %>%
    dplyr::left_join(renamingTable, by = c("ANALYTE", "Study")) %>% # sum(is.na(df$CodeName))
    dplyr::filter(!grepl("remove", CodeName, ignore.case=T))  %>%
    dplyr::left_join(key) %>% # sum(is.na(df$TargetUnits))
    dplyr::rename(ReportedUnits = UNITS) %>%
    dplyr::left_join(conversions) %>%
    # df %>% dplyr::filter(ReportedUnits != TargetUnits) %>% dplyr::reframe(sum(is.na(ConversionFactor)))
    dplyr::mutate(RESULT = ifelse(is.na(ConversionFactor), RESULT, RESULT * ConversionFactor))

  return(df)
}



#' Load and join hydrographic and Secchi data for NCCA 2010 and 2015
#'
#' @description
#' `.loadNCCAhydro` returns a dataframe of all of the hydrographic data relating to NCCA 2010 and 2015
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param NCCAhydrofiles2010 a string specifying the URL for the 2010 hydrographic data
#' @param NCCAsites2010 a string specifying the URL for the 2010 site data
#' @param NCCAhydrofile2015 a string specifying the URL for the 2015 hydrographic data
#' @param NCCAsites2015 a string specifying the URL for the 2015 site data
#' @param NCCAsecchifile2015 a string specifying the URL for the 2015 Secchi data
#' @param namingFile a string specifying the URL for the analyte naming file
#' @param n_max Number of rows to read in from the data file (this is just for testing purposes)
#'
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



##### NOTE KV HAS NOT REVIEWED THE 2020 HYDRO FUNCTION BELOW ####

#' Load and join NCCA 2020 hydrographic data from online csv files
#'
#' @description
#' `.loadNCCAhydro2020` returns a dataframe of all of the hydrographic data relating to NCCA 2010
#'
#' @details
#' THIS FUNCTION HAS NOT BEEN REVIEWED. This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param NCCAhydrofile2020 a string specifying the filepath of the data
#' @param NCCAsites2020 a string specifying the URL for the 2020 site data
#' @param namingFile a string specifying the URL for the analyte naming file
#' @param n_max Number of rows to read in from the data file (this is just for testing purposes)
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

  # missingness/joining checks:
  # sum(is.na(df$CodeName)): 0
  # sum(is.na(df$TargetUnits)): 0
  # df %>% filter(ReportedUnits != TargetUnits) %>% reframe(sum(is.na(ConversionFactor))): 0 cases
  return(df)
}
