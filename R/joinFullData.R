# NOT FULLY REVIEWED
# This file will also need to be rechecked after fixing issues in the other files

#'
#' @param out (optional) filepath to save the dataset to
#' @param .test (optional) boolean, load a test subset of the data. Speeds up function for developers
#' @param binaryOut (optional) boolean, should saved data be RDS format for efficiency?
#' @export
#' @return full, harmonized dataset
assembleData <- function(out = NULL, .test = FALSE, binaryOut = FALSE) {
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
  ) %>%
  dplyr::mutate(Finalized = as.character(Finalized))
  # [ ] KV: Move this final mutate function to within .loadNCCA() function if still needed

  print("Step 2/7: Read preprocessed GLNPO Seabird files")
  seaBirdDf <- readr::read_rds(seaBird) %>%
    # since pH aren't being converted, need to set the explicit units
    dplyr::mutate(Explicit_Units= ifelse(CodeName == "pH", "unitless", Explicit_Units))
  # [ ] KV: Looks like all of this code went to seaBirdProcessing.R, which is not a package function. I appreciate you moving the code to clean up this script, but we should not move code outside the core functions that would cause an update to Analytes3 to not get incorporated by running the package functions. See extensive comments in both seaBirdProcessing.R and NOAAProcessing.R regarding how to address this comment.
  # [ ] KV: Also the mutate() line editing Explicit_Units should be in the new GLNPO Seabird CTD function you will create, not added here.


  print("Step 3/7: Read and clean GLENDA")
  GLENDA <- .readFormatGLENDA(Glenda, n_max = n_max) %>%
    .cleanGLENDA(.,
      namingFile = namingFile, imputeCoordinates = TRUE,
      GLENDAsitePath = GLENDAsitePath, GLENDAlimitsPath = GLENDAlimitsPath
    ) %>%
    dplyr::bind_rows(seaBirdDf) %>%
    dplyr::mutate(
      # This is mostly intended to fill in missing values for seabird
      QAcomment = dplyr::case_when(
        is.na(stationDepth) & (sum(!is.na(stationDepth)) > 0) ~ paste0(QAcomment, "Station depth imputed from another site visit", sep = "; "),
        is.na(stationDepth) & (sum(!is.na(sampleDepth)) > 0) ~ paste0(QAcomment, "Station depth imputed from maximum sample depth", sep = "; "),
        .default = QAcomment),
      stationDepth = dplyr::case_when(
        is.na(stationDepth) & (sum(!is.na(stationDepth)) > 0) ~ mean(stationDepth, na.rm=TRUE),
        is.na(stationDepth) & (sum(!is.na(sampleDepth)) > 0) ~ max(sampleDepth, na.rm = TRUE),
        .default = stationDepth),
      .by = STATION_ID

      # [ ] KV: If we're going to impute stationDepth from max sampleDepth, this should probably only be done for the CTD data, with the assumption that the profile went all the way to the bottom. I don't think we should impute stationDepth using max of chemistry samples, but chemistry and CTD are combined here and so this may happen.
          # Suggest first imputing stationDepth from other site visits
          # Then use Study ID to only impute CTD stationDepth by maxDepth separately, adding a flag
          # Then do another round of imputing stationDepth from other site visits, preserving the flag from other original maxDepth imputation
      # [ ] KV: Also need to add formal flag for imputing stationDepth in flagsMap
      # [ ] KV: Also get warning that it's replacing with -Inf for the max argument.
            # 3: There were 113 warnings in `dplyr::mutate()`.
            # The first warning was:
            #   ℹ In argument: `stationDepth = dplyr::case_when(...)`.
            # ℹ In group 24: `STATION_ID = "MI9552n"`.
            # Caused by warning in `max()`:
            #   ! no non-missing arguments to max; returning -Inf
    )

  print("Step 4/7: Read and clean CSMI data")
  CSMI <- .loadCSMI(csmi2010, csmi2015, csmi2021, namingFile = namingFile, n_max = n_max)
  # [ ] KV: note that dplyr must be loaded or else doesn't find certain functions in CSMI files. This is likely KV's fault for not specifying package

  print("Step 5/7: Read and clean NOAA data")
  NOAA <- .loadNOAAwq(noaaWQ, noaaWQ2, namingFile, noaaWQSites)
  noaaCTD <- readr::read_rds(noaaCTD)
  # [ ] KV: This will need to be replaced with calling the new NOAA CTD function you will create
  NOAA <- dplyr::bind_rows(NOAA, noaaCTD)


  print("Step 6/7: Combine and return full data")
  allWQ <- dplyr::bind_rows(ncca, GLENDA, CSMI, NOAA) %>%
    dplyr::mutate(
      SITE_ID = dplyr::coalesce(SITE_ID, STATION_ID),
      RL = dplyr::coalesce(LRL, MRL, rl),
      MDL = dplyr::coalesce(MDL, mdl),
    ) %>%
    dplyr::select(
      # time and space
      "UID", "Study", "SITE_ID", "Latitude", "Longitude", "stationDepth", "sampleDepth",  "sampleDateTime",
      # [ ] KV: After decision to split sampleDate and sampleTime, will need to change column names here accordingly
      # analyte name
      "CodeName", "ANALYTE", "Category", "LongName", "Explicit_Units",
      # measurement and limits
      "RESULT", "MDL", "RL",
      # unit conversion
      "TargetUnits", "ReportedUnits", "ConversionFactor",
      # QA
      "QAcode", "QAcomment", "METHOD", "LAB", contains("QAconsiderations")
    ) %>%
    # [ ] KV: Could add in DEPTH_CODE eventually if decide need it at some point, and could add in any depth codes from CSMI (2015 at least)
    # catching some where units were inferred
    # KV: I don't think there are any cases of inferred units anymore below. From flagsMap, looks like they previously were used for NCCA_hydro_2010, NCCA_hydro_2015, NCCA_secchi_2015, but these unit issues have likely been resolved elsewhere.

    # [ ] KV: De-duplicate here after resolving duplicate issues below Step 6 - i.e., add distinct() here

    dplyr::mutate(
      QAcode = ifelse(grepl("no reported units", QAcomment, ignore.case = T),
        paste0(QAcode, sep = "; ", "U"), QAcode),
      QAcomment = ifelse((QAcode == "U") & is.na(QAcomment),
        "No reported units, assumed most common units in analyte-year", QAcomment
      )
    ) %>%

    # [ ] KV: note that SeaBird and NOAActd are missing some unit information (e.g., TargetUnits) - likely will be fixed when code is updated and table joins are refreshed dynamically

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


  # [ ] KV: After confirming below issues are okay or resolved, add distinct() to the above Step 6 where noted to reduce the size of the joined data

  # [ ] KV: *** Why are there duplicated rows? This should not be the case. Check before do anything else ***
  # dupes <- allWQ %>% group_by_all() %>% mutate(duplicated = n() >1) %>% ungroup() %>% filter(duplicated==TRUE) %>% arrange(Study, SITE_ID, sampleDateTime, sampleDepth, CodeName)
  # NCCA_hydro_2010 - Repeated Secchi values already noted where appropriate. Also has -9 values, which may not be noted or dealt with? Are those CTB? If so, they aren't flagged
  # NOAA_WQ - Secchi repeated in a few instances - makes sense to just do distinct() here
  # NOAActd - Lots of NOAA CTD repeated twice - perhaps CTD files are in there twice. Makes sense to do distinct() here
  # SeaBird - Lots of SeaBird CTD repeated twice, but not consistently for each parameter, like for NOAA. Seems odd???




  ### LEFT OFF HERE ###

  print("Step 7/7 joining QC flags and suggestions to dataset")
  # [x] Split into flagged and unflagged values
  # [ ] KV: This needs to be done differently below by NOT filtering out by study. For example, I am adding a secchi CTB flag for NOAAwq but would need to also remove NOAAwq below rather than just add a row in flagsMap
  notflagged <- allWQ %>%
    dplyr::filter((is.na(QAcode) & is.na(QAcomment)) | (grepl("SeaBird|CSMI|NOAAwq", Study))) %>%
    dplyr::distinct()


  # [x] join flag explanations
  flags <- openxlsx::read.xlsx(flagsFile) %>%
    # Fill in missing for fuzzy join to work
    dplyr::mutate(
      QAcode = ifelse(is.na(QAcode), Study, QAcode),
      QAcomment = ifelse(is.na(QAcomment), Study, QAcomment)
    )
  # fuzzy join solution from
  # https://stackoverflow.com/questions/69574373/joining-two-dataframes-on-a-condition-grepl


    # [ ] KV: This needs to be done differently below by NOT filtering out by study. For example, I am adding a secchi CTB flag for NOAAwq but would need to also remove NOAAwq below rather than just add a row in flagsMap
    flagged <- allWQ %>%
    dplyr::filter(!((is.na(QAcode) & is.na(QAcomment)) | (grepl("SeaBird|CSMI|NOAAwq", Study)))) %>%
    dplyr::mutate(
      QAcode = stringr::str_remove_all(QAcode, "^NA;"),
      QAcode = stringr::str_remove_all(QAcode, "[:space:]"),
      QAcode = stringr::str_replace_all(QAcode, ",", ";"),
      QAcode = ifelse(is.na(QAcode), Study, QAcode),
      QAcomment = ifelse(is.na(QAcomment), Study, QAcomment)
    ) %>%
    dplyr::distinct() %>%
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
      UID, Study, SITE_ID, Latitude, Longitude, sampleDepth, stationDepth, sampleDateTime, CodeName,
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
    # [ ] KV: These selections look inconsistent with the original selection of columns in Step 6. Revisit the list below
    # [ ] KV: Regardless, Units should probably not be selected below because it's from the Analytes3 spreadsheet and prone to error. Should just use ReportedUnits and TargetUnits
    dplyr::select(
      UID, Study, SITE_ID, Latitude, Longitude, stationDepth, sampleDateTime, sampleDepth,
      CodeName, LongName, RESULT, Units, MDL, # PQL,
      RL, Unified_Flag, Unified_Comment,
      Category, METHOD, LAB, Orig_QAcode=QAcode, Orig_QAcomment=QAcomment,
      Orig_QAdefinition=Definition, ANALYTE_Orig_Name=ANALYTE, ReportedUnits,
      TargetUnits, ConversionFactor, Retain_InternalUse=Retain,
      Action_InternalUse=Action) %>%
    dplyr::arrange(sampleDateTime, SITE_ID, sampleDepth, LongName)

allWQ %>%
  filter(is.na(RESULT) & is.na(Unified_Flag)) %>%
  reframe(s = sum(is.na(RESULT)), .by = c(Study, CodeName)) %>%
  print(n = 300)
allWQ %>%
  reframe(s = mean(is.na(RESULT) & is.na(Unified_Flag)), .by = c(Study, CodeName)) %>%
  print(n = 300)


  if (!is.null(out) & binaryOut) {
    print(paste0("Writing data to ", out, ".Rds"))
    saveRDS(allWQ, paste0(out, ".Rds"))
  } else {
    out <- paste0(out, ".csv")
    print(paste0("Writing data to ", out, ".csv"))
    readr::write_csv(allWQ, file = out, progress = readr::show_progress())
  }

  return(allWQ)
}

# *** KV list ***
# [ ] Does package load dplyr? If not, there are several helper functions that need to have dplyr loaded or need to have dplyr:: added (e.g., join_by)
# [ ] Need to check how UIDs are generated for studies that don't have them - Ideally, do combination of Study, site ID, date, and sampleDepth so that multiple metrics that match these have the same UID (rather than row number)
# [ ] Add flag for all CPAR>100% across datasets but keep in
# [ ] Need flag for station depth imputed from another site visit
# Time imputation issues:
# [ ] Did flag for imputing sample time as noon get incorporated universally? Only for GLENDA (T Flag) - others need it?
# [ ] Need thorough check for missing times being imputed - not imputed for hydro 2015
# [ ] Another option is to just have separate columns for date and time and not impute time and remove flag?
# [ ] Time zones not always specified or are specified differently. How does lubridate know time is EST in CSMI 2015? I don't think it does - assumes UTC and will be incorrect. Need to check throughout but for now, probably assume times are not correct throughout dataset
# [ ] CSMI 2021 time zones not dealt with properly
# [ ] Add known issues to documentation
  # --Times not to be trusted yet
# [ ] Missing station lat/longs (GLENDA, CSMI 2021)



# Christian comments
# [ ]: Identify all analytes with missing Code Names and add to naming shee
# - If ANALYTE = ANL_CODE repalce ANL_CODE with NA
# - Then fill all missing ANL_CODE with nonmissing
# test <- data  %>%
#   filter(is.na(CodeName)) %>%
#   count(Study, ANALYTE, ANL_CODE, FRACTION, METHOD, MEDIUM)
# [ ] : ID all conversions with na (that aren't identical)
# [ ] Make a table of all flags after all is said and done so we can
# annotate them for end-users
