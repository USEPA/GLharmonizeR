#' readPivotGLENDA
#'
#' @description
#' A function to read the full GLENDA data file downloaded 2023/10/23 and convert to long format.
#'
#' @details
#' `.readFormatGLENDA` This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset.
#'
#'
#' @param filepath a filepath to the GLENDA RDS or CSV file
#' @param n_max Number of rows to read in from the raw GLENDA data (this is just for testing purposes)
#' @param sampleIDs a list of sampleIDs to keep in the final dataset
#'
#' @return a dataframe
.readFormatGLENDA <- function(Glenda, n_max = Inf, sampleIDs = NULL) {

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
            ## *** NOTE: UNSURE IF ABOVE col_datetime() CODE WORKS CORRECTLY BUT CURRENTLY USING RDS FILE SO THIS CODE NOT RUN. IF THERE IS A PROBLEM IF SWITCH TO CSV, CHECK HERE ***
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
          dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
          dplyr::mutate(
            Year = as.integer(YEAR),
            STN_DEPTH_M = as.double(STN_DEPTH_M),
            Latitude = as.double(LATITUDE),
            Longitude = as.double(LONGITUDE),
            SAMPLE_DEPTH_M = as.double(SAMPLE_DEPTH_M),
            # SAMPLING_DATE = lubridate::ymd_hm(SAMPLING_DATE)
            SAMPLING_DATE = as.character(SAMPLING_DATE)

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
    # No preliminary filtering to reduce data size
    dplyr::filter(
      SAMPLE_TYPE %in% c("Individual", "INSITU_MEAS"),
      QC_TYPE == "routine field sample",
      # If value and remarks are missing, we assume sample was never taken
      !is.na(VALUE) | !is.na(RESULT_REMARK),
      !grepl("inv", VALUE, ignore.case = T),
      !grepl("no result", VALUE, ignore.case = T),
      !grepl("nrr", VALUE, ignore.case = T),
      !grepl("lac", VALUE, ignore.case = T),
      # Remove Secchi values with "T" and "W" and no RESULT_REMARK - need to ask what these are
      !VALUE=="T",
      !VALUE=="W",
      # Additional filtering based on RESULT_REMARK is done in joinFullData.R based on flagsMap_withDecisions.xlsx file
    ) %>%
    dplyr::filter(!grepl("^integrated", DEPTH_CODE, ignore.case = T))

  return(df)
}

# Remaining non-numeric VALUES are <RL cases
# NAs <- df %>% dplyr::filter(is.na(as.numeric(VALUE)))
# unique(NAs$VALUE)



#' cleanGLENDA
#'
#' @description
#' A function to perform QC on the GLENDA data
#'
#' @details
#' `.cleanGLENDA` This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#'
#' @param df GLENDA dataframe in long format
#' @param namingFile filepath to a file containing mappings for analyte names
#' @param imputeCoordinates (optional) Boolean specifying whether to impute missing station coordinates,
#' @param GLENDAsitePath filepath to list of site coordinates to fill in missing lats/lons
#' @param GLENDAlimitsPath filepath to a file with detection limits for GLENDA data
#'
#' @return a dataframe
.cleanGLENDA <- function(df, namingFile, imputeCoordinates = TRUE, GLENDAsitePath = NULL,
    GLENDAlimitsPath) {

  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "GLENDA_Map", na.strings = c("", "NA")) %>%
    dplyr::select(-Units) # Should remove Units from these renamingTables so they don't cause confusion with the units parsed/read from the data. Units in renaming tables are prone to human error.

  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor)) %>%
    dplyr::distinct() # Duplicate rows



  # First fix analytes that have "none" units (pH correctly has 'none' for all)
  # Some analytes with no result reported or <RL have 'none' units. Many cases removed already from initial filtering, but manganese <RL still have 'none' units
  # Impute by ANALYTE, MEDIUM, FRACTION, METHOD, YEAR for general solution
  df <- df %>% dplyr::mutate(
    UNITS = ifelse(UNITS != "none", UNITS,
                   names(sort(table(UNITS), decreasing = TRUE))[1]),
      .by = c(ANALYTE, MEDIUM, FRACTION, METHOD, YEAR)
    ) %>%
    # *AND FIX TIME ZONES*
    # Convert all TZ relative to GMT for consistency
    # "EST" "EDT" "CDT" "GMT"
    # "EST" to "Etc/GMT+5" # 5 hr behind GMT using POSIX-style signs for time zones (opposite of UTC standard)
    # "EDT" to "Etc/GMT+4"
    # "CDT" to "Etc/GMT+5"
    dplyr::mutate(
      TIME_ZONE = dplyr::case_when(
        TIME_ZONE == "EST" ~ "Etc/GMT+5",
        TIME_ZONE == "EDT" ~ "Etc/GMT+4",
        TIME_ZONE == "CDT" ~ "Etc/GMT+5",
        .default = TIME_ZONE), # GMT stays as GMT
      sampleDateTime = paste(SAMPLING_DATE, TIME_ZONE, sep = " ")
    ) %>%
    dplyr::mutate(
      sampleDateTime = readr::parse_datetime(sampleDateTime, format = "%Y/%m/%d %H:%M %Z"),
      sampleDate = lubridate::date(sampleDateTime),
      sampleTimeUTC = lubridate::hour(sampleDateTime),
      sampleTimeUTC = ifelse(is.na(sampleTimeUTC), 0, sampleTimeUTC) # NAs are midnight (i.e., 0)
    )



  # Pull out RLs from "<" values. These are all RLs, not MDLs, but throw error in case that changes
  # These are all metals
  internalRL <- df %>%
    dplyr::filter(grepl("^<", VALUE))

  # Throw error if it doesn't say reporting limit in RESULT_REMARK
  if (mean(grepl("reporting limit", internalRL$RESULT_REMARK, ignore.case=T)) != 1) {
    stop(".cleanGLENDA(): At least one of the GLENDA values reported as less than a number
          is not also flagged as being less than reporting limit in the RESULT_REMARK
          column. Investigate these cases to see how to deal with them.")
  }

  # Summarize by season-year for consistency with MDLs and for ease of joining
  # Check whether RLs differ within season-year for given analyte - they are uniform
  # look <- internalRL %>% dplyr::group_by(YEAR, SEASON, ANALYTE, MEDIUM, FRACTION) %>% dplyr::summarize(n_RL = dplyr::n_distinct(VALUE))
  internalRL <- internalRL %>%
    dplyr::distinct(YEAR, SEASON, ANALYTE, VALUE, UNITS, MEDIUM, FRACTION, METHOD, RESULT_REMARK) %>%
    dplyr::left_join(renamingTable, by = c("MEDIUM", "ANALYTE", "FRACTION", "METHOD" = "Methods")) %>%
    dplyr::mutate(
      VALUE = readr::parse_number(VALUE),
      YEAR = as.numeric(YEAR)
    ) %>%
    dplyr::left_join(key, by = "CodeName") %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    dplyr::mutate(
      ReportedUnits = tolower(ReportedUnits),
      ReportedUnits = stringr::str_remove_all(ReportedUnits, "/")
    ) %>%
    dplyr::left_join(conversions, by = c("TargetUnits", "ReportedUnits")) %>%
    dplyr::mutate(VALUE = ifelse(is.na(ConversionFactor), VALUE, VALUE * ConversionFactor)) %>%
    # Format similar to other MDLs
    dplyr::distinct(YEAR, SEASON, CodeName, rl = VALUE)



  # MDLs provided by GLNPO
  limitNames <- openxlsx::read.xlsx(namingFile, sheet = "GLENDA_mdl_Map")
  fullSeasons <- expand.grid("SEASON" =  c("Spring", "Summer"), "YEAR" = 1983:2012)

  # Map MDLs, convert units, and join with RLs above
  Mdls <- readr::read_rds(GLENDAlimitsPath) %>%
    tidyr::separate_wider_regex(`Survey (Units)`, patterns = c(SEASON = "^\\S+", "\\s+", YEAR = "\\S+$")) %>%
    dplyr::mutate(YEAR = as.numeric(YEAR)) %>%
    # Include years that don't differentiate between seasons (KV: I think this actually just adds in missing years 2002-2003, but leaving)
    dplyr::full_join(fullSeasons, by = c("SEASON", "YEAR")) %>%
    tidyr::pivot_longer(3:6, names_to = "ANALYTE", values_to = "mdl") %>%
    tidyr::separate_wider_regex(ANALYTE, pattern = c("ANALYTE" = ".*", "\\(", "ReportedUnits" = ".*", "\\)")) %>%
    dplyr::mutate(
      mdl = as.numeric(mdl),
      mdl = replace(mdl, mdl==0, NA), # Replace 0s with NAs (assuming incorrect)
    ) %>%
    dplyr::mutate(
      ANALYTE = stringr::str_remove(ANALYTE, "\\(.*\\)"),
      ANALYTE = stringr::str_trim(ANALYTE),
      # to match into the data
    ) %>%
    dplyr::left_join(., limitNames, by = c("ANALYTE")) %>%
    # Make units match what is expected
    dplyr::mutate(
      ReportedUnits = tolower(stringr::str_remove(ReportedUnits," .*/")),
      ReportedUnits = ifelse(ReportedUnits == "mgl", "mgl", "ugl") # Forced solution b/c of ug/l
      ) %>%
    dplyr::left_join(key) %>%
    dplyr::left_join(conversions) %>%
    dplyr::mutate(
      mdl = ifelse(!is.na(ConversionFactor), mdl * ConversionFactor, mdl),
      mdl = ifelse(CodeName == "Diss_SiO2", mdl * 2.13918214, mdl)
      ) %>%
    dplyr::select(YEAR, SEASON, CodeName, mdl) %>%
    dplyr::bind_rows(internalRL)


  GLNPO_sites <- readr::read_rds(GLENDAsitePath) %>% dplyr::select(-c("First Year", "Last Year", "Spring Years (n)", "Summer Years (n)"))


  df <- df %>%
    dplyr::mutate(
      SEASON = ifelse(is.na(SEASON), lubridate::month(sampleDateTime, label = T), SEASON),
    ) %>%
    dplyr::select(-Number) %>%
    # attempt join of lat/lons for any missing sites using GLNPO station file
    dplyr::left_join(., GLNPO_sites, by = c("STATION_ID" = "Station"), suffix = c("", ".x")) %>%
    dplyr::mutate(
      LATITUDE = as.numeric(LATITUDE),
      Latitude = as.numeric(Latitude),
      LONGITUDE = as.numeric(LONGITUDE),
      Longitude = as.numeric(Longitude),
      LATITUDE = dplyr::coalesce(LATITUDE, Latitude),
      LONGITUDE = dplyr::coalesce(LONGITUDE, Longitude)
    ) %>%
    dplyr::select(-c(Latitude, Longitude)) %>%
    # sum(is.na(df2$LATITUDE)) # Including above reduced missingness from 52344 to 4465
    # look <- df2 %>% dplyr::filter(is.na(LATITUDE))
    # # Most missing site lat/lons are 1983-1995, but several are missing in 2015
    # unique(look$STATION_ID) # 153 missing

    # Impute site coordinates as the mean of that Site's recorded coordinates
    {
      if (imputeCoordinates) {
        dplyr::mutate(.,
          LATITUDE = ifelse(is.na(LATITUDE), mean(LATITUDE, na.rm = T), LATITUDE),
          LONGITUDE = ifelse(is.na(LONGITUDE), mean(LONGITUDE, na.rm = T), LONGITUDE),
          STN_DEPTH_M = ifelse(is.na(STN_DEPTH_M), mean(STN_DEPTH_M, na.rm = T), STN_DEPTH_M),
          .by = STATION_ID
        )
        # Reduces missingness from 4465 to 3972, and # sites from 153 to 145 missing
        # Verified this imputes the same mean value for each STATION_ID (i.e., the mean doesn't change as missing values are replaced in the column)
      } else {
        .
      }
    } %>%
    dplyr::mutate(
      Study = "GLENDA",
      # Grab all of the flags reported in the VALUE column
      # Note that KV already filtered these out (in .readFormatGLENDA) to help deal with units problems, so there probably aren't any of these flags left that are in the VALUE column, except for < values, but keeping anyway
      RESULT_REMARK = ifelse(is.na(as.numeric(VALUE)), paste0(RESULT_REMARK, sep = "; ", VALUE), RESULT_REMARK),
      RESULT = as.numeric(VALUE),
      Latitude = as.numeric(LATITUDE),
      Longitude = as.numeric(LONGITUDE)
    ) %>%
    # These columns are all renamed and so no longer needed
    dplyr::select(-c(VALUE, LONGITUDE, LATITUDE)) %>%
    dplyr::rename(UID = SAMPLE_ID,
                  sampleDepth = SAMPLE_DEPTH_M,
                  stationDepth = STN_DEPTH_M,
                  QAcomment = RESULT_REMARK) %>%
    # Standardize analyte names
    dplyr::left_join(renamingTable, by = c("Study", "MEDIUM", "ANALYTE", "FRACTION", "METHOD" = "Methods")) %>% # sum(is.na(df2$CodeName))
    dplyr::filter(!grepl("remove", CodeName, ignore.case=T))  %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    dplyr::mutate(
      ReportedUnits = tolower(ReportedUnits),
      ReportedUnits = stringr::str_remove_all(ReportedUnits, "/"),
      ReportedUnits = ifelse(ANALYTE == "pH", "unitless", ReportedUnits),
      ReportedUnits = ifelse(ReportedUnits == "%", "percent", ReportedUnits)
    ) %>%
    dplyr::left_join(key, by = "CodeName") %>% # sum(is.na(df2$TargetUnits))
    dplyr::mutate(
      TargetUnits = tolower(TargetUnits),
      ) %>%
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::mutate(
      RESULT = ifelse(!is.na(ConversionFactor), RESULT * ConversionFactor, RESULT),
      # convert from silicon to silica which has more mass
      RESULT = ifelse(ANALYTE == "Silicon, Elemental", RESULT * 2.13918214, RESULT),
      RESULT = ifelse(ANALYTE == "Silica, Dissolved as Si", RESULT * 2.13918214, RESULT),
    ) %>%
    # add in the detection limits
    dplyr::mutate(YEAR = as.numeric(YEAR)) %>%
    dplyr::left_join(., Mdls, by = c("CodeName", "YEAR", "SEASON"))

  return(df)
}
