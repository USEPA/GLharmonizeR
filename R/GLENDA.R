#' readPivotGLENDA
#'
#' @description
#' A function to read the full GLENDA csv file and convert it to a more user friendly long format.
#'
#' @details
#' `.readFormatGLENDA` This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset. This function contains some
#' of the filtering functions in order to easily be compatible with the testing schema because
#' when we filtered later on, we lost too many sample ids that would be included in the test data.
#'
#'
#' @param filepath a filepath to the GLENDA csv
#' @param n_max Number of rows to read in from the raw GLENDA data (this is really just for testing purposes)
#' @param sampleIDs a list of sampleIDs to keep in the final dataset
#'
#' @return a dataframe
.readFormatGLENDA <- function(Glenda, n_max = Inf, sampleIDs = NULL) {
  # [x] Make glendaData the argument for load anssemble data function
  df <- Glenda %>%
    {
      if (grepl(tools::file_ext(Glenda), ".csv", ignore.case = TRUE)) {
        readr::read_csv(.,
          col_types = readr::cols(
            YEAR = "i",
            STN_DEPTH_M = "d",
            LATITUDE = "d",
            LONGITUDE = "d",
            SAMPLE_DEPTH_M = "d",
            SAMPLING_DATE = col_datetime(format = "%Y/%m/%d %H:%M"),
            # Skip useless or redundant columns
            Row = "-",
            .default = "c"
          ),
          show_col_types = FALSE,
          n_max = n_max
        )
      } else {
        .
      }
    } %>%
    {
      if (grepl(tools::file_ext(Glenda), ".Rds", ignore.case = TRUE)) {
        readr::read_rds(.) %>%
          # This is so that everything can be pivoted, we change
          # to final datatypes later once it's in long format
          dplyr::mutate(across(everything(), as.character)) %>%
          dplyr::mutate(
            Year = as.integer(YEAR),
            STN_DEPTH_M = as.double(STN_DEPTH_M),
            Latitude = as.double(LATITUDE),
            Longitude = as.double(LONGITUDE),
            SAMPLE_DEPTH_M = as.double(SAMPLE_DEPTH_M),
            SAMPLING_DATE = lubridate::ymd_hm(SAMPLING_DATE)
          ) %>%
          dplyr::select(-Row)
      } else {
        .
      }
    } %>%
    # this if statement is only for saving test data
    {
      if (!is.null(sampleIDs)) {
        dplyr::filter(
          .,
          SAMPLE_ID %in% sampleIDs
        )
      } else {
        .
      }
    } %>%
    # pivot each set of columns (analyte, units, flag, etc.) into a long
    # format while compressing redundant columns into a single column
    # (i.e. f: ANALYTE_1, ANALYTE_2, ..., ANALYTE_k -> ANALYTE)
    tidyr::pivot_longer(
      cols = -c(1:18),
      names_to = c(".value", "Number"),
      names_pattern = "(.*)_(\\d*)$"
    ) %>%
    # there are a lot of empty columns because of the data storage method
    # so once they are pivoted they are appear as missing
    tidyr::drop_na(ANALYTE) %>%
    # Select samples that haven't been combined
    dplyr::filter(
      SAMPLE_TYPE %in% c("Individual", "INSITU_MEAS"),
      QC_TYPE == "routine field sample",
      # If value and remarks are missing, we assume sample was never taken
      !is.na(VALUE) | !is.na(RESULT_REMARK) # ,
      # The only QA Codes worth removing "Invalid" and "Known Contamination".
      # The rest already passed an initial QA screening before being entered
      # !grepl("Invalid", RESULT_REMARK, ignore.case = T),
      # RESULT_REMARK != "Known Contamination"
    ) %>%
    dplyr::filter(!grepl("^integrated", DEPTH_CODE, ignore.case = T)) %>%
    dplyr::mutate(
        # [x] KV: Are you sure this is in always in standard time using 'Canada/Newfoundland', or that it adjusts for daylight saving time? Newfoundland does observe daylights savings, so I'm not sure how this works. Here is an example of how I previously handled EDT, if it's helpful (basically call it EST and subtract an hour from the time): Chl_EDT <- Chl %>% filter(TIME_ZONE=="EDT") %>% mutate(Sample_Date=ymd_hm(SAMPLING_DATE, tz = "EST")-hours(1))
      SAMPLING_DATE = lubridate::as_datetime(ifelse(TIME_ZONE=="EDT", SAMPLING_DATE - lubridate::hours(1), SAMPLING_DATE)),
      # [x] Check if remove RESULTstart - verified it's gone
      TIME_ZONE = dplyr::case_when(
        TIME_ZONE == "EDT" ~ "EST",
        TIME_ZONE == "CDT" ~ "EST",
        .default = TIME_ZONE
      )) %>%
    tidyr::unite(sampleDateTime, SAMPLING_DATE, TIME_ZONE) %>%
    dplyr::mutate(
      sampleDateTime = readr::parse_datetime(sampleDateTime, format = "%Y-%m-%d %H:%M:%S_%Z"),
      sampleDate = lubridate::date(test$sampleDateTime),
      sampleTimeUTC = lubridate::hour(test$sampleDateTime)
      )
  return(df)
}


#' cleanGLENDA
#'
#' @description
#' A function to perform automated QC on the GLENDA data
#'
#' @details
#' `.cleanGLENDA` This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#'
#' @param df GLENDA dataframe in long format
#' @param imputeCoordinates (optional) Boolean specifying whether to impute missing station coordinates,
#' @param siteCoords (optional) filepath to list of site coordinates to fill in missing lats/lons
#' @param namingFile (optional) filepath to a file containing remappings for analyte names
#' @param GLENDAlimitsPath (optional) filepath to a file limits for GLENDA data
#'
#' @return a dataframe
.cleanGLENDA <- function(
    df, namingFile, imputeCoordinates = TRUE, GLENDAsitePath = NULL,
    GLENDAlimitsPath = NULL) {
  
  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "GLENDA_Map", na.strings = c("", "NA")) %>%
    tidyr::separate_wider_delim(Years, "-", names = c("minYear", "maxYear")) %>%
    dplyr::mutate(minYear = as.numeric(minYear), maxYear = as.numeric(maxYear))

  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)


  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor)) %>% 
    dplyr::distinct() # Duplicate rows

  # all rl not mdls
  internalRL <- df %>%
    # [x] try catch, throw error if it doesn't say reporting limit in RESULT_REMARK
    dplyr::filter(grepl("^<", VALUE))

  if (mean(grepl("reporting limit", internalRL$RESULT_REMARK, ignore.case=T)) != 1) {
    stop("At least one of the GLENDA values reported as less than some number 
          isn't also reported as being less than reporting limit in the RESULT_REMARK
          column. Investigate these comments to see how to deal with them.")
  }
  
  internalRL <- internalRL %>%
    dplyr::distinct(YEAR, SEASON, ANALYTE, VALUE, MEDIUM, FRACTION, METHOD, RESULT_REMARK) %>%
    dplyr::left_join(renamingTable, by = c("MEDIUM", "ANALYTE", "FRACTION", "METHOD" = "Methods")) %>%
    dplyr::mutate(
      VALUE = readr::parse_number(VALUE),
      YEAR = as.numeric(YEAR)
    ) %>%
    dplyr::left_join(key, by = "CodeName") %>%
    dplyr::rename(ReportedUnits = Units) %>%
    dplyr::left_join(conversions, by = c("TargetUnits", "ReportedUnits")) %>%
    dplyr::mutate(VALUE = ifelse(is.na(ConversionFactor), VALUE, VALUE * ConversionFactor)) %>%
    # Format similar to other MDLs
    dplyr::distinct(YEAR, SEASON, CodeName, rl = VALUE)
    # [x] unit conversions for the above internal mdls before joining into mdl df
    # [x] Double check that renaming is working properly for the mdl

  # All self reported mdls are identical across all years for a gien analyte
  limitNames <- openxlsx::read.xlsx(namingFile, sheet = "GLENDA_mdl_Map")

  # instead of solving just for values before 1993, fill it in for other years and solve it generally
  fullSeasons <- expand.grid("SEASON" =  c("Spring", "Summer"), "YEAR" = 1983:2012)
  # [x] utilize Mdl from glenda sheet
  Mdls <- readr::read_rds(GLENDAlimitsPath) %>%
    tidyr::separate_wider_regex(`Survey (Units)`, patterns = c(SEASON = "^\\S+", "\\s+", YEAR = "\\S+$")) %>%
    dplyr::mutate(YEAR = as.numeric(YEAR)) %>%
    # Include years that don't differentiate between seasons
    dplyr::full_join(fullSeasons, by = c("SEASON", "YEAR")) %>%
    tidyr::pivot_longer(3:6, names_to = "ANALYTE", values_to = "mdl") %>%
    tidyr::separate_wider_regex(ANALYTE, pattern = c("ANALYTE" = ".*", "\\(", "ReportedUnits" = ".*", "\\)")) %>%
    dplyr::mutate(
      mdl = as.numeric(mdl),
      .by = c(YEAR, ANALYTE)
    ) %>%
    dplyr::mutate(
      ANALYTE = stringr::str_remove(ANALYTE, "\\(.*\\)"),
      ANALYTE = stringr::str_trim(ANALYTE),
      # to match into the data
    ) %>%
    dplyr::left_join(., limitNames, by = c("ANALYTE" = "OldName")) %>%
    dplyr::select(-ANALYTE) %>%
    dplyr::rename(CodeName = ANALYTE.y) %>%
    # Make units match what is expected
    dplyr::mutate(
      ReportedUnits = tolower(stringr::str_remove(ReportedUnits," .*/")),
      ReportedUnits = ifelse(ReportedUnits == "mgl", "mgl", "ugl")
      ) %>%
    dplyr::left_join(key) %>%
    dplyr::left_join(conversions) %>%
    dplyr::mutate(
      mdl = ifelse(!is.na(ConversionFactor), mdl * ConversionFactor, mdl),
      mdl = ifelse(CodeName == "Diss_SiO2", mdl * 2.13918214, mdl)
      ) %>%
    dplyr::select(YEAR, SEASON, CodeName, mdl) %>%
    dplyr::bind_rows(internalRL)

  test <- df %>%
    # Convert daylight saving TZs into standard time TZs
    dplyr::mutate(
      SEASON = ifelse(is.na(SEASON), lubridate::month(SAMPLING_DATE, label = T), SEASON),
    ) %>%
    # Drop analyte number since it doesn't mean anything now
    # These columns are redundant with the "Analyte" columns
    dplyr::select(-Number) %>%
    {
      if (!is.null(GLENDAsitePath)) {
        # grab the missing sites from that file
        dplyr::left_join(., readr::read_rds(GLENDAsitePath), by = c("STATION_ID" = "Station"), suffix = c("", ".x")) %>%
          dplyr::mutate(
            LATITUDE = as.numeric(LATITUDE),
            Latitude = as.numeric(Latitude),
            LONGITUDE = as.numeric(LONGITUDE),
            Longitude = as.numeric(Longitude),
            LATITUDE = dplyr::coalesce(LATITUDE, Latitude),
            LONGITUDE = dplyr::coalesce(LONGITUDE, Longitude)
          ) %>%
          dplyr::select(-c(Latitude, Longitude))
      } else {
        .
      }
    } %>%
    # Impute site coordinates as the mean of that Site's recorded coordinates
    # Not removing this, just deprecating this argument
    {
      if (imputeCoordinates) {
        dplyr::mutate(.,
          LATITUDE = as.numeric(LATITUDE),
          LONGITUDE = as.numeric(LONGITUDE),
          LATITUDE = ifelse(is.na(LATITUDE), mean(LATITUDE, na.rm = T), LATITUDE),
          LONGITUDE = ifelse(is.na(LONGITUDE), mean(LONGITUDE, na.rm = T), LONGITUDE),
          STN_DEPTH_M = ifelse(is.na(STN_DEPTH_M), mean(STN_DEPTH_M, na.rm = T), STN_DEPTH_M),
          .by = STATION_ID
        )
      } else {
        .
      }
    } %>%
    dplyr::mutate(
      Study = "GLENDA",
      # Grab all of the flags reported in the VALUE column
      RESULT_REMARK = ifelse(is.na(as.numeric(VALUE)), paste0(RESULT_REMARK, sep = "; ", VALUE), RESULT_REMARK),
      RESULT = as.numeric(VALUE), Latitude = as.numeric(LATITUDE), Longitude = as.numeric(LONGITUDE)
    ) %>%
    # These columns are all renamed and so no longer needed
    dplyr::select(-c(VALUE, LONGITUDE, LATITUDE)) %>%
    dplyr::rename(UID = SAMPLE_ID, sampleDepth = SAMPLE_DEPTH_M, stationDepth = STN_DEPTH_M, QAcomment = RESULT_REMARK) %>%
    dplyr::mutate(UID = as.character(UID), RESULT = as.numeric(RESULT)) %>%
    # Standardize analyte names
    dplyr::left_join(renamingTable, by = c("Study", "MEDIUM", "ANALYTE", "FRACTION", "METHOD" = "Methods")) %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    dplyr::mutate(
      ReportedUnits = tolower(ReportedUnits),
      ReportedUnits = stringr::str_remove_all(ReportedUnits, "/")
    ) %>%
    dplyr::left_join(key, by = "CodeName") %>%
    dplyr::filter(!grepl("remove", CodeName, ignore.case=T))  %>%


    # [x] Check if we need to impute units- nope all taken care of
    # sum(is.na(df$Units)) == 0
    # [x] KV: Need to recheck this for ReportedUnits=="none" instead of NA. See GitHub comment on PR
    # If so, we will assume on a given year analytes have same units **KV comment: suggest imputing by year-season combo, rather than just by year**
    # PR comment: In checking that the unit conversions mapped correctly below, I noticed that 
    # there are several observations where ReportedUnits is "none" rather than NA, so those aren't
    # being picked up with the check here for NA units. When you look at the GLENDA_Map tab in 
    # Analytes3.xlsx, you can see these analytes that sometimes have 'none' for units. 
    # In these cases, there is only ever one type of unit reported for that analyte for the rest
    # of the dataset, so it should be reasonable to impute these missing units. It would be worth
    # doing this in case there are analytes that need to have their units converted (I actually 
    # don't think there are, but just in case). You could just impute units by Year/Season to make 
    # it general.
    dplyr::mutate(
      ReportedUnits = ifelse(ReportedUnits != "none", ReportedUnits,
      names(sort(table(ReportedUnits), decreasing = TRUE))[1]
      ),
      .by = c(CodeName, YEAR, SEASON)
    ) %>%
    dplyr::mutate(
      TargetUnits = tolower(TargetUnits),
      ReportedUnits = ifelse(ANALYTE == "pH", "unitless", ReportedUnits),
      ReportedUnits = ifelse(ReportedUnits == "%", "percent", ReportedUnits)
      ) %>%
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::filter(!grepl("remove", CodeName, ignore.case = T)) %>%
    # [x] Check this filter is working
    # this was doing the opposite of what we wanted only choosing integrated
    dplyr::mutate(RESULT = dplyr::case_when(
      # convert from silicon to silica which has more mass
      ANALYTE == "Silicon, Elemental" ~ RESULT * 2.13918214,
      ANALYTE == "Silica, Dissolved as Si" ~ RESULT * 2.13918214,
      ReportedUnits == TargetUnits ~ RESULT,
      ReportedUnits != TargetUnits ~ RESULT * ConversionFactor,
      .default = RESULT
    )) %>%
    # add in the detection limits
    dplyr::mutate(YEAR = as.numeric(YEAR)) %>%
    dplyr::left_join(., Mdls, by = c("CodeName", "YEAR", "SEASON")) %>%
    dplyr::select(-c(sampleDate, dplyr::ends_with(".x"), dplyr::ends_with(".y")))

    # [x] Add self reported  detection limits
    # [x] Extract the < values for all datas, join it back to the pdf extracted RL's
    # [x] Join them all together then join back to the data
    # [x] Give priority to the pdf source - not necessary since they are mutually exclusive
    # [x] Code the flags MDL and DL are the same

  return(df)
}
