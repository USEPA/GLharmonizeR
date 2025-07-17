#' Load and join Great Lakes water quality data from GLNPO, CSMI, NCCA, and NOAA. Data currently limited to Lake Michigan.
#'
#' @description
#' `assembleData` returns a dataframe with harmonized water quality parameter names, units, and quality control flags
#' Great Lakes National Program Office (GLNPO) Great Lakes Environmental Database, [GLENDA](https://cdx.epmeea.gov/)
#' - 1983 - 2023
#' - Seabird CTD 2003 - 2023
#' CSMI
#' - 2015, 2021
#' National Coastal Condition Assessment, [NCCA](https://www.epa.gov/national-aquatic-resource-surveys/ncca)
#' - 2010, 2015
#' National Oceanic and atmospheric Administration, [NOAA](https://www.noaa.gov/)
#' - 2007 - 2022
#' - CTD 2007 - 2022
#'
#' @details
#' This is the main function of GLharmonizeR which assembles water quality data for the Great Lakes (currently only Lake Michigan) across multiple decades and data sources.
#'
#' @param out filepath specifying where to save the assembled data. Note: this should exclude the file extension
#' @param .test (optional) boolean, load a test subset of the data. Speeds up function for developers
#' @param binaryOut (optional) boolean, should saved data be RDS format for efficiency?. If false it is saved as CSV Default is TRUE.
#' @export
#' @returns Harmonized water quality dataset for Lake Michigan. *FILL THIS IN WITH DOCUMENTATION OF COLUMN NAMES, FLAGS, LINKS TO ANALYTE AND FLAG MAPPINGS*
#' @examples
#' df <- assembleData("filepath", binaryOut = FALSE)
#' df <- assembleData("filepath", binaryOut = TRUE)
assembleData <- function(out=NULL, .test = FALSE, binaryOut = TRUE) {

  # To dos:
  # [ ] make argument for source ("ALL", "GLENDA", "CSMI", "NCCA", "NOAA")
  # [ ] water body name argument once other lakes added in

  # Load up the filepaths
  filepaths <- .getFilePaths()
  NCCAhydrofiles2010 <- filepaths["NCCAhydrofiles2010"]
  NCCAhydrofile2015 <- filepaths["NCCAhydrofile2015"]
  NCCAhydrofile2020 <- filepaths["NCCAhydrofile2020"]
  NCCAsecchifile2015 <- filepaths["NCCAsecchifile2015"]
  NCCAsites2010 <- filepaths["NCCAsites2010"]
  NCCAsites2010not <- filepaths["NCCAsites2010not"]
  NCCAsites2015 <- filepaths["NCCAsites2015"]
  NCCAsites2020 <- filepaths["NCCAsites2020"]
  NCCAwq2010 <- filepaths["NCCAwq2010"]
  NCCAqa2010 <- filepaths["NCCAqa2010"]
  NCCAwq2015 <- filepaths["NCCAwq2015"]
  NCCAwq2020 <- filepaths["NCCAwq2020"]
  NCCAwqQA <- filepaths["NCCAwqQA"]
  Glenda <- filepaths["Glenda"]
  GLENDAlimitsPath <- filepaths["GLENDAlimitsPath"]
  GLENDAsitePath <- filepaths["GLENDAsitePath"]
  # csmi2010 <- filepaths["csmi2010"]
  # csmi2010 <- NULL
  csmi2015 <- filepaths["csmi2015"]
  csmi2021 <- filepaths["csmi2021"]
  seaBird <- filepaths["seaBird"]
  namingFile <- filepaths["namingFile"]
  flagsFile <- filepaths["flagsFile"]
  noaaWQ <- filepaths["noaaWQ"]
  noaaWQ2 <- filepaths["noaaWQ2"]
  noaaCTD <- filepaths["noaaCTD"]
  noaaWQSites <- filepaths["noaaWQSites"]


  n_max <- ifelse(.test, 1000, Inf)

  print("Step 1/7: Read and clean NCCA")
  ncca <- .loadNCCA(
    NCCAsites2010, NCCAsites2015, NCCAwq2010,
    NCCAwq2015, NCCAhydrofiles2010,
    NCCAhydrofile2015, NCCAsecchifile2015,
    namingFile,
    Lakes = c("Lake Michigan"),
    n_max = n_max
  )

  print("Step 2/7: Read and clean preprocessed GLNPO Seabird and NOAA CTD files")
  GLNPOseabirdCTD <- .cleanGLNPOSeabirdCTD()
  noaaCTD <- .cleanNOAACTD()

  print("Step 3/7: Read and clean GLENDA")
  GLENDA <- .readFormatGLENDA(Glenda, n_max = n_max) %>%
    .cleanGLENDA(.,
      namingFile = namingFile, imputeCoordinates = TRUE,
      GLENDAsitePath = GLENDAsitePath, GLENDAlimitsPath = GLENDAlimitsPath
    )

  # Use GLNPO GLENDA and Seabird databases to share site info for missing info:

  # Examine match between GLNPOseabirdCTD maxCTDdepth with GLENDA stationDepth
  GLENDA_site_depths <- GLENDA %>%
    tidyr::drop_na(stationDepth) %>%
    dplyr::select(SITE_ID, stationDepth, sampleDate) %>%
    dplyr::distinct() %>%
    dplyr::summarize(
      .by = c(SITE_ID),
      # Note that some sites can have a wide range of values
      # e.g., MI11 depth ranges from 70-132, with mean 126
      # numStationDepth = length(unique(stationDepth)),
      # maxDepth = max(stationDepth),
      # minDepth = min(stationDepth),
      GLENDAstationDepth = mean(stationDepth, na.rm=T)
    )

  GLENDA_site_depthsYR <- GLENDA %>%
    tidyr::drop_na(stationDepth) %>%
    dplyr::select(SITE_ID, stationDepth, sampleDate, YEAR) %>%
    dplyr::distinct() %>%
    dplyr::summarize(
      .by = c(SITE_ID, YEAR),
      # Note that some sites can have a wide range of values
      # e.g., MI11 depth ranges from 70-132, with mean 126
      # numStationDepth = length(unique(stationDepth)),
      # maxDepth = max(stationDepth),
      # minDepth = min(stationDepth),
      GLENDAstationDepthYR = mean(stationDepth, na.rm=T)
    )

  # Per analysis below: use GLENDA stationDepth preferentially over CTD because CTD measurements not fully to bottom. Use year-specific GLENDA stationDepth where possible, then mean GLENDA stationDepth
  # Then use maxCTDdepth as stationDepth estimate when SITE_ID not in GLENDA - add 'D' flag for station depth estimated as max CTD depth
  # Not adding a flag for using GLENDA stationDepth for CTD
  GLNPOseabirdCTD <- GLNPOseabirdCTD %>%
    dplyr::select(-stationDepth) %>% # Note that stationDepth from CTD is all NA
    dplyr::left_join(GLENDA_site_depths) %>%
    dplyr::left_join(GLENDA_site_depthsYR) %>%
    dplyr::mutate(
      stationDepth = dplyr::coalesce(GLENDAstationDepthYR, GLENDAstationDepth)
    ) %>%
    # look <- GLNPOseabirdCTD %>% dplyr::filter(is.na(stationDepth))
    # unique(look$SITE_ID) # Only "MIC" left
    # "MIC" not in GLENDA -- but there are these sites names in GLENDA: "MIC-1n"    "MIC-2n"    "MIC-3n"    "MIC-45n"   "MIC-5n" "MIC-6n"    "MIC-7n"
    # BASE file name is 70647_MIC_17.CNV - unable to infer site name from this path
    # Use maxCTDdepth for remaining site and add flag
    dplyr::mutate(
      QAcode = ifelse(is.na(stationDepth) & !is.na(maxCTDdepth), "D", NA),
      QAcomment = ifelse(is.na(stationDepth) & !is.na(maxCTDdepth), "station Depth estimated as the maximum CTD Depth", NA),
      stationDepth = ifelse(is.na(stationDepth) & !is.na(maxCTDdepth), maxCTDdepth, stationDepth)
    ) %>%
    dplyr::select(-maxCTDdepth, -maxCTDdepthYR, -GLENDAstationDepth, -GLENDAstationDepthYR)

  # Plot relationship between maxCTDdepth (max over all years) and GLENDAstationDepth (mean over all years)
  # plot(maxCTDdepth~GLENDAstationDepth, data=GLNPOseabirdCTD)
  # abline(0,1)
  # maxCTDdepth over all seabird profiles correlates strongly with mean GLENDA stationDepth, but maxCTDdepth is consistently lower than GLENDA stationDepth

  # Plot relationship between maxCTDdepthYR and GLENDAstationDepthYR by YEAR
  # plot(maxCTDdepthYR~GLENDAstationDepthYR, data=GLNPOseabirdCTD)
  # abline(0,1)
  # maxCTDdepth by year correlates strongly with mean GLENDA stationDepth by year, but maxCTDdepth is consistently lower than GLENDA stationDepth, even by year

  # Note that none of the GLENDA sites missing stationDepth are in Seabird for grabbing depth info
  # GLENDA_miss <- GLENDA %>% dplyr::filter(is.na(stationDepth)) %>%
  #   dplyr::select(SITE_ID) %>% unique()
  # sum(GLENDA_miss$SITE_ID %in% GLNPOseabirdCTD$SITE_ID)

  # Similarly, none of the GLENDA sites missing lat/longs are in Seabird
  # GLENDA_missLAT <- GLENDA %>% dplyr::filter(is.na(Latitude)) %>%
  #   dplyr::select(SITE_ID) %>% unique()
  # sum(GLENDA_missLAT$SITE_ID %in% GLNPOseabirdCTD$SITE_ID)

  # *** If add options to ask for specific datasets, note than both GLNPO seabird and GLENDA will need to be loaded to impute station depths, even if they're not ultimately returned ***


  print("Step 4/7: Read and clean CSMI data")
  CSMI <- .loadCSMI(csmi2015, csmi2021, namingFile = namingFile)


  print("Step 5/7: Read and clean NOAA WQ data")
  NOAA <- .loadNOAAwq(noaaWQ, noaaWQ2, namingFile, noaaWQSites)


  print("Step 6/7: Combine and return full data")
  allWQ <- dplyr::bind_rows(ncca, GLENDA, CSMI, NOAA, GLNPOseabirdCTD, noaaCTD) %>%
    dplyr::mutate(
      SITE_ID = dplyr::coalesce(SITE_ID, STATION_ID),
      RL = dplyr::coalesce(LRL, MRL, rl),
      MDL = dplyr::coalesce(MDL, mdl),
    ) %>%
    dplyr::select(
      # time and space
      "UID", "Study", "SITE_ID", "Latitude", "Longitude", "stationDepth",
      "sampleDepth",  "sampleDate", "sampleDateTime", "DEPTH_CODE",
      # Dropping sampleTimeUTC and keeping sampleDateTime instead (in UTC)
      # analyte name
      "CodeName", "ANALYTE", "Category", "LongName", "Explicit_Units",
      # measurement and limits
      "RESULT", "MDL", "RL",
      # unit conversion
      "TargetUnits", "ReportedUnits", "ConversionFactor",
      # QA
     "METHOD", "LAB", dplyr::contains("QAconsiderations"),  "QAcode", "QAcomment"
    ) %>%
    dplyr::rename(sampleDateTimeUTC = sampleDateTime) %>%
    # Look at duplicated observations
    # dupes <- allWQ %>% group_by_all() %>% mutate(duplicated = n() >1) %>% ungroup() %>% filter(duplicated==TRUE) %>% arrange(Study, SITE_ID, sampleDate, sampleDateTimeUTC, sampleDepth, CodeName)
    # unique(dupes$Study)
    # NOAA secchi repeated in a few instances
    # NOAActd has lots of repeated data - perhaps CTD files are in there twice.
    # SeaBird - Lots of SeaBird CTD repeated twice

    # Remove duplicates
    dplyr::distinct() %>%

    # Add in flags to catch limit issues
    dplyr::mutate(

      # Flag if below detection limit
      # Note that this catches a bunch of early GLENDA data that are below MDLs but not flagged, and catches CSMI_2015 and NCCA_WChem_2010
      QAcode = dplyr::case_when(
        is.na(MDL) ~ QAcode,
        RESULT >= MDL ~ QAcode,
        RESULT < MDL ~ paste(QAcode, "MDL", sep = "; "),
        # KV: Above catches NCCA_WChem_2010, GLENDA, CSMI_2015
        .default = QAcode
      ),
      QAcomment = dplyr::case_when(
        is.na(MDL) ~ QAcomment,
        RESULT >= MDL ~ QAcomment,
        RESULT < MDL ~ paste(QAcomment, "MDL", sep = "; "),
        .default = QAcomment
      ),

      ## Flag if below reporting limit
      QAcode = dplyr::case_when(
        is.na(RL) ~ QAcode,
        RESULT >= RL ~ QAcode,
        RESULT < RL ~ paste(QAcode, "RL", sep = "; "),
        # KV: RL flag added to NCCA_WChem_2010 that are estimated (between MDL and RL) but preference given to estimated values over the <RL flag below, so okay
        .default = QAcode
      ),
      QAcomment = dplyr::case_when(
        is.na(RL) ~ QAcomment,
        RESULT >= RL ~ QAcomment,
        RESULT < RL ~ paste(QAcomment, "RL", sep = "; "),
        .default = QAcomment
      ),

      ## Flag if between MDL and RL, inclusive
      QAcode = dplyr::case_when(
        is.na(RL) & is.na (MDL) ~ QAcode,
        (RESULT >= MDL) & (RESULT <= RL) ~ paste(QAcode, "Estimated", sep = "; "),
        .default = QAcode
      ),
      QAcomment = dplyr::case_when(
        is.na(RL) & is.na (MDL) ~ QAcomment,
        (RESULT >= MDL) & (RESULT <= RL) ~ paste(QAcomment, "Estimated", sep = "; "),
        .default = QAcomment
      )
    ) %>%
    # Replace negative CPAR with 0 and add flag for CPAR>100%
    dplyr::mutate(
      RESULT = ifelse(CodeName == "CPAR" & RESULT<0, 0, RESULT),
      QAcode = ifelse(CodeName == "CPAR" & RESULT>100, paste(QAcode, "PAR", sep = "; "), QAcode),
      QAcomment = ifelse(CodeName == "CPAR" & RESULT>100, paste(QAcomment, "Underwater PAR exceeds surface PAR", sep = "; "), QAcomment)
    ) %>%
    # Make "NA" be NA, not character
    dplyr::mutate(
      QAcode = ifelse(QAcode=="NA", NA, QAcode),
      QAcomment = ifelse(QAcomment=="NA", NA, QAcomment)
    )





# round(colMeans(is.na(allWQ)), 3)
  # UID               Study           SITE_ID          Latitude         Longitude
  # 0.000             0.000             0.000             0.011             0.011
  # stationDepth       sampleDepth        sampleDate sampleDateTimeUTC        DEPTH_CODE
  # 0.018                    0.006             0.000             0.180             0.755
  # CodeName           ANALYTE          Category          LongName    Explicit_Units
  # 0.000              0.000             0.000             0.000             0.000
  # RESULT               MDL                RL       TargetUnits     ReportedUnits
  # 0.003              0.923             0.997             0.000             0.000
  # ConversionFactor     METHOD               LAB            QAcode         QAcomment
  # 0.669                 0.751             0.985             0.958             0.940
# Most have times as well as dates






  print("Step 7/7 joining QC flags and suggestions to dataset")

  # fuzzy join solution from
  # https://stackoverflow.com/questions/69574373/joining-two-dataframes-on-a-condition-grepl

  # flag mapping file
  flags <- openxlsx::read.xlsx(flagsFile) %>%
    dplyr::rename(
      QAcode2 = QAcode,
      QAcomment2 = QAcomment) %>%
    # Fill in missing for fuzzy join to work
    dplyr::mutate(
      QAcode2 = ifelse(is.na(QAcode2), "XXXX", QAcode2),
      QAcomment2 = ifelse(is.na(QAcomment2), "XXXX", QAcomment2)
    ) %>%
    dplyr::distinct()

  # data without flags
  notflagged <- allWQ %>%
    dplyr::filter((is.na(QAcode) & is.na(QAcomment)))

  # data with flags
  flagged <- allWQ %>%
    dplyr::filter(!((is.na(QAcode) & is.na(QAcomment)))) %>%
    dplyr::mutate(
      QAcode = stringr::str_remove_all(QAcode, "^NA; "),
      QAcode = stringr::str_replace_all(QAcode, ",", ";"),
      QAcomment = stringr::str_remove_all(QAcomment, "^NA; "),
    ) %>%
    dplyr::mutate(QAcode2=QAcode, QAcomment2 = QAcomment) %>%
    dplyr::mutate(
      # QAcode2 = stringr::str_remove_all(QAcode2, "^NA"),
      # QAcode2 = stringr::str_remove_all(QAcode2, "NA"),
      QAcode2 = stringr::str_remove_all(QAcode2, "[:space:]"),
      QAcode2 = stringr::str_remove_all(QAcode2, "^,"),
      QAcode2 = ifelse(QAcode2 == "", NA, QAcode2),
      QAcode2 = ifelse(is.na(QAcode2), "XXXX", paste0(QAcode2, ";XXXX")),
      QAcode2 = stringr::str_remove_all(QAcode2, "^; "),
      # QAcomment2 = stringr::str_remove_all(QAcomment2, "^NA"),
      QAcomment2 = stringr::str_remove_all(QAcomment2, "^; "),
      QAcomment2 = ifelse(QAcomment2 == "", NA, QAcomment2),
      QAcomment2 = ifelse(is.na(QAcomment2), "XXXX", paste0(QAcomment2, "; XXXX"))
      # **** Need to add a placeholder to both QAcode2 and QAcomment2 so that flags that appear in only QAcode or QAcomment in data will surely be mapped!
      # However, need to add a placeholder that does NOT have letters that appear in any flags on its own (like N) because it will be mapped to that flag (e.g., previously used Study to augment all code and comments, but NCCA studies were mapped to N flag) - now using 'XXXX' - X is not a QAcode across studies, and 'XXXX' does not appear in comments *****
    ) %>%
    fuzzyjoin::fuzzy_join(flags,
      by=c("Study", 'QAcode2', "QAcomment2"),
      mode='left', # left join
      match_fun = list(`==`, stringr::str_detect, stringr::str_detect)
    ) %>%
    # Use QAcode2.x and QAcomment2.x that have all codes/comments
    dplyr::mutate(
      Study = Study.x,
      QAcode2 = QAcode2.x,
      QAcomment2 = QAcomment2.x,
    ) %>%
    dplyr::select(
      -c(Study.x, Study.y, QAcode2.x, QAcode2.y, QAcomment2.x, QAcomment2.y)
    ) %>%
    # Compress instances that were matched multiple times into single observation
    dplyr::reframe(
      # Don't need to retain these temporary QAcode2 and QAcomment2 columns - just for joining
      # QAcode2 = toString(unique(QAcode2)),
      # QAcomment2 = toString(unique(QAcomment2)),
      Definition = toString(unique(Definition)),
      Unified_Flag = toString(unique(Unified_Flag)),
      Unified_Comment = toString(unique(Unified_Comment)),
      Retain = toString(unique(Retain)),
      Action = toString(unique(Action)),
     .by = c(
      UID, Study, SITE_ID, Latitude, Longitude, stationDepth, sampleDepth, sampleDate, sampleDateTimeUTC, DEPTH_CODE, CodeName,
      ANALYTE, Category, LongName, Explicit_Units, RESULT, MDL, RL, TargetUnits, ReportedUnits, ConversionFactor, METHOD, LAB,  QAcode, QAcomment
    ))  %>%
  # Clean up unified flags/comments
  dplyr::mutate(
    Unified_Flag = ifelse(Unified_Flag == "NA", NA, Unified_Flag),
    Unified_Flag = stringr::str_remove_all(Unified_Flag, "NA, "),
    Unified_Flag = stringr::str_remove_all(Unified_Flag, ", NA"),

    Unified_Comment = ifelse(Unified_Comment == "NA", NA, Unified_Comment),
    Unified_Comment = stringr::str_remove_all(Unified_Comment, "NA, "),
    Unified_Comment = stringr::str_remove_all(Unified_Comment, ", NA")
  ) %>%
  # Remove any flagged observations that have 'Remove' in the Retain column
  dplyr::filter(!grepl("Remove", Retain, ignore.case = TRUE)) %>%
  # handle Retain column in order of flag priority
  dplyr::mutate(
    RESULT = dplyr::case_when(
      is.na(Unified_Flag) ~ RESULT,
      # Set non-detects to NA
      grepl("N", Unified_Flag) ~ NA,
      # Keep estimated values
      grepl("E", Unified_Flag) ~ RESULT,
      # Set < reporting limit to NA if not also an estimate
      grepl("R", Unified_Flag) ~ NA,
      # Set all secchi clear to bottom to NA result
      grepl("B", Unified_Flag) ~ NA,
      .default = RESULT
    )
  )
  # Checks
  # look <- flagged %>% dplyr::filter(grepl("N", Unified_Flag)) # All NA
  # look <- flagged %>% dplyr::filter(grepl("E", Unified_Flag)) # All numeric
  # look <- flagged %>% dplyr::filter(Unified_Flag=="R") # All NA
  # look <- flagged %>% dplyr::filter(grepl("B", Unified_Flag)) # All NA



  # Recombine full dataset and convert sampleDate to be EST
  allWQ <- dplyr::bind_rows(flagged, notflagged) %>%
    dplyr::select(
      # Time and space
      UID,
      Study,
      SITE_ID,
      Latitude,
      Longitude,
      stationDepth,
      sampleDate,
      sampleDateTimeUTC,
      sampleDepth,
      DEPTH_CODE,
      # Analyte name
      CodeName,
      LongName,
      Category,
      ANALYTE_Orig_Name=ANALYTE,
      # Measurement and limits
      RESULT,
      MDL,
      RL,
      # Unit conversion
      Units = Explicit_Units,
      ReportedUnits,
      ConversionFactor,
      # QA
      Unified_Flag,
      Unified_Comment,
      METHOD,
      LAB,
      Orig_QAcode = QAcode,
      Orig_QAcomment = QAcomment,
      Orig_QAdefinition=Definition#,
      # Retain_InternalUse=Retain,
      # Action_InternalUse=Action
      ) %>%
    dplyr::mutate(
      sampleDateTimeEST = lubridate::with_tz(sampleDateTimeUTC, "Etc/GMT+5"),
      sampleDateEST = lubridate::date(sampleDateTimeEST),
      sampleDate = dplyr::coalesce(sampleDateEST, sampleDate)
    ) %>%
    dplyr::select(-c(sampleDateTimeEST, sampleDateEST)) %>%
    dplyr::arrange(sampleDate, sampleDateTimeUTC, SITE_ID, sampleDepth, LongName)


  # round(colMeans(is.na(allWQ)), 2)
  # UID             Study           SITE_ID          Latitude         Longitude
  # 0.00              0.00              0.00              0.01              0.01
  # stationDepth        sampleDate sampleDateTimeUTC       sampleDepth        DEPTH_CODE
  #         0.02              0.00              0.18              0.01              0.76
  # CodeName          LongName          Category ANALYTE_Orig_Name            RESULT
  #     0.00              0.00              0.00              0.00              0.01
  #   MDL                RL             Units     ReportedUnits  ConversionFactor
  #  0.92              1.00              0.00              0.00              0.67
  # Unified_Flag   Unified_Comment            METHOD               LAB       Orig_QAcode
  #         0.94              0.94              0.75              0.99              0.96
  # Orig_QAcomment Orig_QAdefinition
  #           0.94              0.94


  # allWQ %>%
  #   dplyr::reframe(s = sum(is.na(RESULT) & is.na(Unified_Flag)), .by = c(Study, CodeName)) %>%
  #   print(n = 300)

  # check missingness
  # allWQ %>%
  #   dplyr::reframe(across(everything(),
  #           function(x) round(mean(is.na(x)), 2)),
  #           .by = Study) %>%
  #   View()

  # allWQ %>%
  #   dplyr::reframe(across(everything(),
  #           function(x) sum(!is.na(x))),
  #           .by = Study) %>%
  #   View()

  # allWQ %>%
  #   dplyr::reframe(dplyr::across(c(Latitude, Longitude),
  #             c(
  #               "max" = function(x) max(x, na.rm = T),
  #               "min" = function(x) min(x, na.rm = T)
  #               )),
  #           .by = Study) %>%
  #   View()




  # Write data
  if (!is.null(out) & binaryOut == TRUE) {
    print(paste0("Writing data to ", out, ".rda"))
    save(allWQ, file = paste0(out, ".rda"))
  }
  if (!is.null(out) & binaryOut == FALSE) {
    print(paste0("Writing data to ", out, ".csv"))
    readr::write_csv(allWQ, file = paste0(out, ".csv"), progress = readr::show_progress())
  }



  return(allWQ)

}


# df <-  assembleData(out= "C:/Users/KVITENSE/OneDrive - Environmental Protection Agency (EPA)/LM_Chla/data/allWQ", binaryOut = TRUE)

