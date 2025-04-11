# NOT FULLY REVIEWED
# This file will also need to be rechecked after fixing issues in the other files

#' Load and join all WQ measured on Lake Michigan from multiple sources. data from 2010, 2015, and 2020/2021 from CSMI, GLENDA, and NCCA
#'
#' @description
#' `assembleData` returns a dataframe with unified reporting conventions
#' The Great Lakes Environmental Database, [GLNPO](https://cdx.epmeea.gov/)
#' - 1983 - 2023
#' - Seabird CTD 2003 - 2023
#' CSMI (hosted locally)
#' - 2015, 2020 (2010 partially, but too much missing info for inclusion)
#' National Coastal Condition Assessment, [NCCA](https://www.epa.gov/national-aquatic-resource-surveys/ncca)
#' - 2010, 2015
#' National Oceanic and atmospheric Administration, [NOAA](https://www.noaa.gov/)
#' - 2007 - 2022
#' - CTD 2007 - 2022
#'
#' @details
#' This is the main function of LMChla which assembles water quality for Lake Michigan
#' across multiple decades. 
#'
#' @param out filepath to save the dataset to. Note: this should exclude the file extension
#' @param .test (optional) boolean, load a test subset of the data. Speeds up function for developers
#' @param binaryOut (optional) boolean, should saved data be RDS format for efficiency?. If false it is saved as csv. Default is TRUE.
#' @export
#' @returns full, harmonized dataset
#' @examples
#' assembleData("filepath", binaryOut = FALSE)
#' assembleData("filepath")
assembleData <- function(out, .test = FALSE, binaryOut = TRUE) {
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
  # [ ] make arguement for source ("ALL", "GLENDA", "CSMI", "NCCA", "NOAA")
  # [ ] Minyear maxyear arguments
  # [ ] water body name argument
  n_max <- ifelse(.test, 1000, Inf)
  # [x] report sample DateTime not just date
  print("Step 1/7: Read and clean NCCA")
  ncca <- .loadNCCA(
    NCCAsites2010, NCCAsites2015, NCCAwq2010,
    NCCAwq2015, NCCAhydrofiles2010,
    NCCAhydrofile2015, NCCAsecchifile2015,
    namingFile,
    Lakes = c("Lake Michigan"),
    n_max = n_max
  )

  print("Step 2/7: Read preprocessed GLNPO Seabird and NOAA CTD files")
  seabirdNnoaaCTD <- .cleanNOAAnSeabirdCTD()

  print("Step 3/7: Read and clean GLENDA")
  GLENDA <- .readFormatGLENDA(Glenda, n_max = n_max) %>%
    .cleanGLENDA(.,
      namingFile = namingFile, imputeCoordinates = TRUE,
      GLENDAsitePath = GLENDAsitePath, GLENDAlimitsPath = GLENDAlimitsPath
    )
      # [x] KV: If we're going to impute stationDepth from max sampleDepth, this should probably only be done for the CTD data, with the assumption that the profile went all the way to the bottom. I don't think we should impute stationDepth using max of chemistry samples, but chemistry and CTD are combined here and so this may happen.
          # CC: Taken care of by separating out SeaBird data
          # Suggest first imputing stationDepth from other site visits
          # Then use Study ID to only impute CTD stationDepth by maxDepth separately, adding a flag
          # Then do another round of imputing stationDepth from other site visits, preserving the flag from other original maxDepth imputation
      # [x] KV: Also need to add formal flag for imputing stationDepth in flagsMap
          # - CC: added "D" and associated comment
      # [x] KV: Also get warning that it's replacing with -Inf for the max argument.
        # CC: solved by separating out CTD
            # 3: There were 113 warnings in `dplyr::mutate()`.
            # The first warning was:
            #   ℹ In argument: `stationDepth = dplyr::case_when(...)`.
            # ℹ In group 24: `STATION_ID = "MI9552n"`.
            # Caused by warning in `max()`:
            #   ! no non-missing arguments to max; returning -Inf
  
  print("Step 4/7: Read and clean CSMI data")
  CSMI <- .loadCSMI(csmi2010, csmi2015, csmi2021, namingFile = namingFile, n_max = n_max)
  # [x] KV: note that dplyr must be loaded or else doesn't find certain functions in CSMI files. This is likely KV's fault for not specifying package

  print("Step 5/7: Read and clean NOAA WQ data")
  NOAA <- .loadNOAAwq(noaaWQ, noaaWQ2, namingFile, noaaWQSites)

  print("Step 6/7: Combine and return full data")
  allWQ <- dplyr::bind_rows(ncca, GLENDA, CSMI, NOAA, seabirdNnoaaCTD) %>%
    dplyr::mutate(
      SITE_ID = dplyr::coalesce(SITE_ID, STATION_ID),
      RL = dplyr::coalesce(LRL, MRL, rl),
      MDL = dplyr::coalesce(MDL, mdl),
    ) %>%
    dplyr::select(
      # time and space
      "UID", "Study", "SITE_ID", "Latitude", "Longitude", "stationDepth", 
      "sampleDepth",  "sampleDate", "sampleTime", "DEPTH_CODE",
      # [x] KV: After decision to split sampleDate and sampleTime, will need to change column names here accordingly
      # analyte name
      "CodeName", "ANALYTE", "Category", "LongName", "Explicit_Units",
      # measurement and limits
      "RESULT", "MDL", "RL",
      # unit conversion
      "TargetUnits", "ReportedUnits", "ConversionFactor",
      # QA
      "QAcode", "QAcomment", "METHOD", "LAB", dplyr::contains("QAconsiderations")
    ) %>%
    # [x] KV: Could add in DEPTH_CODE eventually if decide need it at some point, and could add in any depth codes from CSMI (2015 at least)
    # catching some where units were inferred
    # KV: I don't think there are any cases of inferred units anymore below. From flagsMap, looks like they previously were used for NCCA_hydro_2010, NCCA_hydro_2015, NCCA_secchi_2015, but these unit issues have likely been resolved elsewhere.

    # [x] KV: De-duplicate here after resolving duplicate issues below Step 6 - i.e., add distinct() here
    dplyr::distinct() %>%
    dplyr::mutate(
      QAcode = ifelse(grepl("no reported units", QAcomment, ignore.case = T),
        paste0(QAcode, sep = "; ", "U"), QAcode),
      QAcomment = ifelse((QAcode == "U") & is.na(QAcomment),
        "No reported units, assumed most common units in analyte-year", QAcomment
      )
    ) %>%

    # [x] KV: note that SeaBird and NOAActd are missing some unit information (e.g., TargetUnits) - likely will be fixed when code is updated and table joins are refreshed dynamically

    # Add in flags to catch limit issues
    dplyr::mutate(
      # [X] KV: Split out MDL, RL, and Estimated cases to deal with NAs

      # Flag if below detection limit
      # KV: Note that this catches a bunch of early GLENDA data that are below MDLs but not flagged, and catches CSMI_2015 and NCCA_WChem_2010
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
      # [X] KV: Note that there are some NCCA_WChem_2015 ammonium values between MDL and RL that didn't get flag as estimated -- need to add flag
      # [X] KV: Add Estimated and RL flags for all studies to flagsMap
    )


# > round(colMeans(is.na(allWQ)), 2)
#              UID            Study          SITE_ID         Latitude 
#             0.00             0.00             0.00             0.01
#        Longitude     stationDepth      sampleDepth       sampleDate
#             0.01             0.02             0.01             0.01
#       sampleTime       DEPTH_CODE         CodeName          ANALYTE
#             0.97             0.72             0.00             0.00 
#         Category         LongName   Explicit_Units           RESULT
#             0.00             0.00             0.00             0.02
#              MDL               RL      TargetUnits    ReportedUnits
#             0.91             1.00             0.00             0.24
# ConversionFactor           QAcode        QAcomment           METHOD
#             0.73             0.69             0.64             0.72
#              LAB
#             0.98





  # [x] KV: After confirming below issues are okay or resolved, add distinct() to the above Step 6 where noted to reduce the size of the joined data

  # [x] KV: *** Why are there duplicated rows? This should not be the case. Check before do anything else ***
  # dupes <- allWQ %>% group_by_all() %>% mutate(duplicated = n() >1) %>% ungroup() %>% filter(duplicated==TRUE) %>% arrange(Study, SITE_ID, sampleDate, sampleDepth, CodeName)
  # - this now has length 0
  # NCCA_hydro_2010 - Repeated Secchi values already noted where appropriate. Also has -9 values, which may not be noted or dealt with? Are those CTB? If so, they aren't flagged
  # NOAA_WQ - Secchi repeated in a few instances - makes sense to just do distinct() here
  # NOAActd - Lots of NOAA CTD repeated twice - perhaps CTD files are in there twice. Makes sense to do distinct() here
  # SeaBird - Lots of SeaBird CTD repeated twice, but not consistently for each parameter, like for NOAA. Seems odd???


  print("Step 7/7 joining QC flags and suggestions to dataset")
  # [x] Split into flagged and unflagged values
  # [x] KV: This needs to be done differently below by NOT filtering out by study. For example, I am adding a secchi CTB flag for NOAAwq but would need to also remove NOAAwq below rather than just add a row in flagsMap
  # - made a fix and is only slightly less efficient
  notflagged <- allWQ %>%
    dplyr::filter((is.na(QAcode) & is.na(QAcomment)))


  # join flag explanations
  flags <- openxlsx::read.xlsx(flagsFile) %>%
    # Fill in missing for fuzzy join to work
    dplyr::mutate(
      QAcode = ifelse(is.na(QAcode), Study, QAcode),
      QAcomment = ifelse(is.na(QAcomment), Study, QAcomment)
    )
  # fuzzy join solution from
  # https://stackoverflow.com/questions/69574373/joining-two-dataframes-on-a-condition-grepl


    # [x] KV: This needs to be done differently below by NOT filtering out by study. For example, I am adding a secchi CTB flag for NOAAwq but would need to also remove NOAAwq below rather than just add a row in flagsMap
  flagged <- allWQ %>%
    dplyr::filter(!((is.na(QAcode) & is.na(QAcomment)))) %>%
    dplyr::mutate(
      QAcode = stringr::str_remove_all(QAcode, "^NA;"),
      QAcode = stringr::str_remove_all(QAcode, "[:space:]"),
      QAcode = stringr::str_replace_all(QAcode, ",", ";"),
      QAcode = ifelse(is.na(QAcode), Study, QAcode),
      QAcomment = ifelse(is.na(QAcomment), Study, QAcomment)
    ) %>%
    fuzzyjoin::fuzzy_join(flags,
    by=c("Study", 'QAcode', "QAcomment"),
    mode='left', #use left join
    match_fun = list(`==`, stringr::str_detect, stringr::str_detect)
  ) %>%
    dplyr::mutate(
      # grab values in the mapping file in case we filled out by hand
      Study = dplyr::coalesce(Study.x, Study.y),
      QAcode = dplyr::coalesce(QAcode.x, QAcode.y),
      QAcomment = dplyr::coalesce(QAcomment.x, QAcomment.y),
    ) %>%
    dplyr::select(
      -c(dplyr::ends_with("\\.y"), dplyr::ends_with("\\.x"))
    ) %>%
  # Compress instances that were matched multiple times into sinlge observation
  # for QA comments and codes
  # Checked dimenisons and this preserved the number of observvations from flgagd
  # data
    dplyr::reframe(
      QAcode = toString(unique(QAcode)),
      QAcomment = toString(unique(QAcomment)),
      Definition = toString(unique(Definition)),
      Unified_Flag = toString(unique(Unified_Flag)),
      Unified_Comment = toString(unique(Unified_Comment)),
      Retain = toString(unique(Retain)),
      Action = toString(unique(Action)),
     .by = c(
      UID, Study, SITE_ID, Latitude, Longitude, sampleDepth, stationDepth, sampleDate, sampleTime, CodeName,
      ANALYTE, Category, LongName, ConversionFactor, TargetUnits, ReportedUnits, RESULT,
      MDL, #PQL,
       RL, LAB
    ))  %>%
     # handle Retain column by priority
  # We're joining by QAcode and QAcomment so this only removes based on the ocmment as well
  dplyr::mutate(
    RESULT = dplyr::case_when(
      is.na(Unified_Flag) ~ RESULT,
      grepl("N;|N$", Unified_Flag) ~ NA,
      # These next two are set in deliberate order set priority to the more complex match
      grepl("E;|E$", Unified_Flag) ~ RESULT,
      grepl("R;|R$", Unified_Flag) ~ NA,
      grepl("B;|B$", Unified_Flag) ~ NA,
      .default = RESULT
    ),
    # Cleaning up unified flags
    Unified_Flag = stringr::str_remove_all(Unified_Flag, "NA"),
    Unified_Flag = ifelse(Unified_Flag == "", NA, Unified_Flag),
    Unified_Flag = stringr::str_remove(Unified_Flag, "^,"),
    Unified_Flag = stringr::str_squish(Unified_Flag)
  ) %>%
  # [x] Make sure NA flag doesn't break this - this is exclusively flagged stuff so should work
  dplyr::filter(!grepl("Remove", Retain) | is.na(Retain) | is.na(Unified_Flag))

  # recombine full dataset
  allWQ <- dplyr::bind_rows(flagged, notflagged) %>%
    dplyr::mutate(Units = Explicit_Units) %>%
    # [x] KV: These selections look inconsistent with the original selection of columns in Step 6. Revisit the list below
    # [x] KV: Regardless, Units should probably not be selected below because it's from the Analytes3 spreadsheet and prone to error. Should just use ReportedUnits and TargetUnits
    dplyr::select(
      # time and space
      UID, Study, SITE_ID, Latitude, Longitude, stationDepth, sampleDate, sampleTime,
      sampleDepth, DEPTH_CODE,
      # analyte name
      CodeName, LongName, Category, ANALYTE_Orig_Name=ANALYTE, Units,
      # measurement and limits
      RESULT, MDL, RL,
      # unit conversionReportedUnits,
      Units = TargetUnits, ConversionFactor, Unified_Flag, Unified_Comment,
      METHOD, LAB,
      # QA
      Orig_QAcode=QAcode, Orig_QAcomment=QAcomment,
      Orig_QAdefinition=Definition,  Retain_InternalUse=Retain,
      Action_InternalUse=Action) %>%
    dplyr::arrange(sampleDate, SITE_ID, sampleDepth, LongName) %>%
    # [x] Add flag for all CPAR>100% across datasets but keep in
    dplyr::mutate(
      QAcode = ifelse((CodeName == "CPAR") & (RESULT > 1) ~ paste(QAcode, "Q", sep = "; ")),
      QAcomment = ifelse((CodeName == "CPAR") & (RESULT > 1) ~ paste(QAcode, "QC issue", sep = "; ")),
    )


# > round(colMeans(is.na(allWQ)), 2)
#                UID              Study            SITE_ID           Latitude 
#               0.00               0.00               0.00               0.01
#          Longitude       stationDepth         sampleDate         sampleTime
#               0.01               0.02               0.01               0.97 
#        sampleDepth         DEPTH_CODE           CodeName           LongName
#               0.01               0.75               0.00               0.00
#           Category  ANALYTE_Orig_Name              Units             RESULT
#               0.00               0.00               0.37               0.03
#                MDL                 RL        TargetUnits   ConversionFactor 
#               0.91               1.00               0.00               0.73
#       Unified_Flag    Unified_Comment             METHOD                LAB
#               0.95               0.63               0.75               0.98
#        Orig_QAcode     Orig_QAcomment  Orig_QAdefinition Retain_InternalUse
#               0.63               0.63               0.63               0.63
# Action_InternalUse
#               0.63 




# allWQ %>%
#   filter(is.na(RESULT) & is.na(Unified_Flag)) %>%
#   reframe(s = sum(is.na(RESULT)), .by = c(Study, CodeName)) %>%
#   print(n = 300)
# allWQ %>%
#   reframe(s = mean(is.na(RESULT) & is.na(Unified_Flag)), .by = c(Study, CodeName)) %>%
#   print(n = 300)


  if (!is.null(out) & binaryOut) {
    print(paste0("Writing data to ", out, ".Rds"))
    saveRDS(allWQ, paste0(out, ".Rds"))
  } else {
    print(paste0("Writing data to ", out, ".csv"))
    readr::write_csv(allWQ, file = out, progress = readr::show_progress())
  }

# check missingness
# allWQ %>% 
#   reframe(across(everything(), 
#           function(x) round(mean(is.na(x)), 2)),
#           .by = Study) %>%
#   View()
# allWQ %>% 
#   reframe(across(everything(), 
#           function(x) sum(!is.na(x))),
#           .by = Study) %>%
#   View()
# allWQ %>% 
#   reframe(n = length(unique(UID)),
#           .by = Study) %>%
#   arrange(desc(n))
# allWQ %>% 
#   reframe(across(c(Latitude, Longitude),
#             c(
#               "max" = function(x) max(x, na.rm = T),
#               "min" = function(x) min(x, na.rm = T)
#               )),
#           .by = Study) %>%
#   View()

  return(allWQ)
}

# inspected every max, min and middle for each column for each Study using View(allWQ)
# - [x]  CSMI 2021 CTD lat longs need to be standardized to -86 - -87 and  + 43


# *** KV list ***
# [x] Does package load dplyr? If not, there are several helper functions that need to have dplyr loaded or need to have dplyr:: added (e.g., join_by)
# - No. I added them where needed
# [x] Need to check how UIDs are generated for studies that don't have them - Ideally, do combination of Study, site ID, date, and sampleDepth so that multiple metrics that match these have the same UID (rather than row number)
# - CTD's missing study name... now it's added
# [x] Need flag for station depth imputed from another site visit
# Time imputation issues:
# [x] Did flag for imputing sample time as noon get incorporated universally? Only for GLENDA (T Flag) - others need it?
# - CC: Not imputing anymore that we separated the columns
# [x] Need thorough check for missing times being imputed - not imputed for hydro 2015
# - CC: Not imputing anymore that we separated the columns
# [x] Another option is to just have separate columns for date and time and not impute time and remove flag?
# [x] Time zones not always specified or are specified differently. How does lubridate know time is EST in CSMI 2015? I don't think it does - assumes UTC and will be incorrect. Need to check throughout but for now, probably assume times are not correct throughout dataset
# - CC: Time zones are computed as UTC now 
# [x] CSMI 2021 time zones not dealt with properly
# [ ] Add known issues to documentation
  # unknown if NOAA cond is specific conductivity at 25C
  # Times mostly trusted but might need to be careful about daylight savings instances
  # unable to retrieve dates for small subset of NOAA CTD data
  # There are negatives and zeros in RESULTS column
  # NCCA secchi 2015 has vrey poor spatial resolution
  # Missing station lat/longs (GLENDA, CSMI 2021)
    # Glenda 3.9%
    # CSMI 2021 wq 5.7%