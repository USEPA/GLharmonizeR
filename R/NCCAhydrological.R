
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
  sites <- .loadNCCASite2010(NCCAsites2010)
  
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
      ANL_CODE = ifelse(is.na(ANL_CODE), ANALYTE, ANL_CODE)#,
      # Any methods reported in NCCA_Map are a result of not originally filtering the dataset to GL - shouldn't be needed
    )


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
    # Doesn't look like below code to fill in missing units is needed after filtering to only GL
    # sum(is.na(df$UNITS))==0
    # dplyr::mutate(
    #   UNITS  = rev(names(table(UNITS)))[[1]],
    #   .by = ANALYTE
    # ) %>%
    # Note: CAST_COMMENT and CAST_FLAG are all NA
    # Note: Averaging over RESULT is necessary below because sometimes multiple values present at same depth. Looking at data, this looks like due to possible data entry error due to upcasts being mislabeled, but this is a reasonable solution.
    dplyr::reframe(.by = UID:ANALYTE, RESULT = mean(RESULT, na.rm = T), dplyr::across(UNITS:QAcomment, function(x) toString(unique(x))))
    # , values_fill = list("RESULT" = 9999999, NA) - cannot fill in with a number or causes problems below
    
    
    
    # KV: Code is now okay up until this point
    
    
    


  # [x] KV: *** I strongly suggest not doing these data manipulations by pivoting on the whole data frame. It is introducing too many errors. I would suggest instead splitting out the data that you need to do manipulations on (ambient and underwater PAR) and dealing with them separately, them joining them back in. Same comment on the NCCA water chemistry data **********
  # [x] KV: Also the QAcode and QAcomments get lost in the below process- solved removing pivot
  # [x] Secchi units not filled in and are missing in the final returned data - solved removing pivot
  parDf <- df %>% 
    dplyr::filter(ANALYTE %in% c("Ambient PAR", "Underwater PAR")) %>%
    # derive cpar
    dplyr::reframe(
      .by = c(UID, SITE_ID, STATE, DATE_COL, sampleDepth),
      APAR = mean(ifelse(ANALYTE == "Ambient PAR", RESULT, NA), na.rm = T),
      UPAR = mean(ifelse(ANALYTE == "Underwater PAR", RESULT, NA), na.rm = T),
      RESULT = UPAR / APAR,
      ANALYTE = "Corrected PAR",
      stationDepth = mean(stationDepth, na.rm = T),
      UNITS = "",
      QAcode = toString(unique(QAcode)),
      QAcomment = toString(unique(QAcomment)),
    ) %>% 
    dplyr::select(-c(APAR, UPAR))

  df <- df %>% dplyr::filter(! ANALYTE %in% c("Ambient PAR", "Underwater PAR")) %>% 
    dplyr::bind_rows(parDf) %>%
    dplyr::filter(!(is.na(RESULT) & is.na(QAcode))) %>% # Replaced with a statement to remove NA, not 9999999
    dplyr::mutate(sampleDepth = ifelse(ANALYTE == "Mean secchi", NA, sampleDepth)) %>%
    # [ ] KV: Have repeated secchi observations for every depth - won't be a problem if split data out to manipulate cpar separately
    # - not sure if they were repeated or if we would like them to be repeated?

    
    
    #### Code okay after this point ####

    dplyr::mutate(DATE_COL = lubridate::mdy(DATE_COL)) %>%
    dplyr::mutate(
    #  sampleDepth = ifelse(sampleDepth == -9.0, NA, sampleDepth),
      Study = "NCCA_hydro_2010",
      UNITS = ifelse(ANALYTE == "Corrected PAR", "percent", UNITS),
      UNITS = ifelse(ANALYTE == "Conductivity", "uscm", UNITS) # adding because there's a weird special character
      # # make site ids look like site id file
      # SITE_ID = stringr::str_remove_all(SITE_ID, "NCCA10-"),
      # SITE_ID = stringr::str_remove_all(SITE_ID, "NCCAGL10-"),
      # SITE_ID = stringr::str_remove_all(SITE_ID, "GLBA10-"),
    ) %>%

    # add station info
    dplyr::left_join(sites, by = "SITE_ID") %>%
    dplyr::mutate(stationDepth = dplyr::coalesce(stationDepth.x, stationDepth.y)) %>%
    dplyr::select(-c(stationDepth.x, stationDepth.y)) %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    # There is no ANL_CODE
    # dplyr::mutate(
    #   ANALYTE = stringr::str_trim(ifelse(is.na(ANALYTE), ANL_CODE, ANALYTE)),
    # ) %>%
    # convert units
    dplyr::left_join(renamingTable, by = c("ANALYTE", "Study")) %>%
    dplyr::filter(CodeName != "Remove") %>% 
    dplyr::left_join(key) %>%
    dplyr::mutate(
      ReportedUnits = as.character(ReportedUnits),
      ReportedUnits = stringr::str_remove(ReportedUnits, "/"),
      ReportedUnits = tolower(ReportedUnits)) %>% # 
    dplyr::left_join(conversions) %>%
    dplyr::mutate(RESULT = ifelse(is.na(ConversionFactor), RESULT, RESULT * ConversionFactor))

  return(df)
}


###################
## KV left off here
###################



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
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor)) %>%
    dplyr::distinct() # Duplicate rows
  
  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NCCA_Map", na.strings = c("", "NA")) %>%
    # remove nas from table to remove ambiguities on joinging
    dplyr::mutate(
      ANALYTE = ifelse(is.na(ANALYTE), ANL_CODE, ANALYTE),
      ANL_CODE = ifelse(is.na(ANL_CODE), ANALYTE, ANL_CODE)
      # Methods = ifelse(is.na(Methods), Study, Methods) # prefer to not fill in Methods if not available (keep as NA)
    )

  df <- readr::read_csv(NCCAhydrofile2015, n_max = n_max, show_col_types = FALSE) %>%
    # the only comments mention no measurment data or typo
    # [x] Need to remove all NARS_COMMENTs  -- KV: I don't see where this is done
    # KV: Actually, only the 'Cast and depth provided but no measurement data; measurement depth > station depth (6.6m)' will be taken care of by just removing all NA values. And 'Typo - L uw? No field form available.' only catches one case of where UW > AMB light. There are other similar cases as well, so don't remove these.
    # KV changed NCCA_hydro_2015 action in flagsMap to 'Keep' based on above notes
    dplyr::filter(CAST == "DOWNCAST") %>%
    dplyr::mutate(
      `Corrected PAR` = LIGHT_UW / LIGHT_AMB,
      sampleDateTime = as.Date(DATE_COL, origin = "1900-1-1"),
      # [x] KV: Note that time is not imputed here for sampleDateTime. Need a thorough check across datasets and flags added. What happens when it is merged with the rest of the data without a time?
      Study = "NCCA_hydro_2015"
    ) %>%
    # Some Corrected PAR is Inf because LIGHT_AMB==0, which must be incorrect
    # Replace Corrected PAR infinity values with NA
    dplyr::mutate(`Corrected PAR` = dplyr::na_if(`Corrected PAR`, Inf)) %>%
    # dplyr::filter(!is.na(LIGHT_UW) | !is.na(LIGHT_AMB)) %>% # KV: You can't filter in this way or you are removing other data where light is missing but other parameters are measured
    dplyr::select(
      -c(LIGHT_AMB, LIGHT_UW)
    ) %>%
    dplyr::rename(sampleDepth = DEPTH, stationDepth = STATION_DEPTH, QAcomment= NARS_COMMENT) %>%
    # [ ] KV: Added QAcomment=NARS_COMMENT here to hopefully preserve the comments, which didn't seem to be happening. 
    # Note comments at beginning of function that we will not be using these comments to remove any data though. Check that it doesn't break anything
    tidyr::pivot_longer(c(TRANS, CONDUCTIVITY:TEMPERATURE, `Corrected PAR`), names_to = "ANALYTE", values_to = "RESULT") %>%
    # Remove NAs after pivoting - gets rid of bad CPAR and other missing data
    # dplyr::filter((!is.na(RESULT)) & (!is.na(QAcomment))) %>%
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
    dplyr::left_join(renamingTable, by = c("ANALYTE", "Study")) %>%
    dplyr::filter(CodeName != "Remove") %>%
    dplyr::left_join(key) %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    dplyr::left_join(conversions) %>%
    dplyr::mutate(RESULT = ifelse(is.na(ConversionFactor), RESULT, RESULT * ConversionFactor)) %>%
    # add station info
    dplyr::left_join(sites) # KV added this in, but not working properly
    # [x] KV: This is not joining for all sites. If you join by only SITE_ID, then there are multiple sites and stationDepth combos in the sites file, which causes problems due to multiple matches. If you join including stationDepth, then some secchi sites don't get matched because their stationDepth is not included in the sites file. Note: For some reason, site info joins properly for 2015 water chemistry but not for 2015 hydro or secchi. Perhaps this is why the site info was not joined for hydro 2015?? This needs to be investigating.
    # - only missing 5% of lat/lngs now after successful join 
    # [x] KV: Note that time is not imputed here for sampleDateTime. Need a thorough check across datasets. What happens when it is merged with the rest of the data without a time?
  
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
  sites <- .loadNCCASite2015(NCCAsites2015) %>%
    # same sites reported mulitple times, so keep the mean (this applies to lat/lng, depth)
    # - rounded to m so hopeufully it'll match the data
    dplyr::reframe(
      .by = SITE_ID,
      dplyr::across(c(Latitude, Longitude, stationDepth), function(x) round(mean(x, na.rm = T))),
      WTBDY_NM = unique(WTBDY_NM)
    )
  
  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units) %>%
    dplyr::distinct()
  
  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor)) %>% 
    dplyr::distinct() # Duplicate rows
  
  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "NCCA_Map", na.strings = c("", "NA")) %>%
    # remove nas from table to remove ambiguities on joinging
    dplyr::mutate(
      ANALYTE = ifelse(is.na(ANALYTE), ANL_CODE, ANALYTE),
      ANL_CODE = ifelse(is.na(ANL_CODE), ANALYTE, ANL_CODE)
    ) %>% 
    dplyr::distinct()
  
  
  df <- readr::read_csv(NCCAsecchifile2015, n_max = n_max, show_col_types = FALSE) %>%
    dplyr::filter(
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
      sampleDateTime = mean(sampleDateTime, na.rm = T), # KV added this, was missing sampleDateTime otherwise
      stationDepth = mean(STATION_DEPTH, na.rm = T),

      # MEAN_SECCHI_DEPTH is the estimated column (from Kd)
      # So we find the actual mean of the different casts manually
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
      UNITS = "m",
      ANALYTE = "Secchi"
    ) %>%
    dplyr::mutate(.by = SITE_ID, stationDepth = round(mean(stationDepth, na.rm = T))) %>%
    dplyr::left_join(sites, by = c("SITE_ID", "stationDepth")) %>%
    # [x] KV: Site info is not joining for all sites. If you join by only SITE_ID, then there are multiple sites and stationDepth combos in the sites file, which causes problems due to multiple matches. If you join including stationDepth, then some secchi sites don't get matched because their stationDepth is not included in the sites file. Note: For some reason, site info joins properly for 2015 water chemistry but not for 2015 hydro or secchi. Perhaps this is why the site info was not joined for hydro 2015?? This needs to be investigating.
    # - took the mean reported lat/lng, depth for each site to make sure it's unique
    # - only 1% missingness using this method
  
    # KV added below code to properly join tables
    dplyr::left_join(renamingTable, by = c("ANALYTE", "Study")) %>%
    dplyr::filter(CodeName != "Remove") %>%
    dplyr::left_join(key) %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    dplyr::left_join(conversions) %>%
    dplyr::mutate(RESULT = ifelse(is.na(ConversionFactor), RESULT, RESULT * ConversionFactor)) %>% 
    dplyr::filter(!is.na(RESULT))
  
    # KV: NaN's in RESULT seem to be cases where DISAPPEAR and REAPPEAR aren't available. Often these are marked as clear to bottom, but sometimes estimated value is inconsistent with this marking. 
    # Decide to remove as bad data
  
  
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




##### NOTE KV HAS NOT REVIEWED THE 2020 HYDRO FUNCTION BELOW ####

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
