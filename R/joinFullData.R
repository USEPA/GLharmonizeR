
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
  noaaCTD <- filepaths["noaaCTD"]
  noaaWQSites <- filepaths["noaaWQSites"]
  # [ ] make arguement for source ("ALL", "GLENDA", "CSMI", "NCCA", "NOAA")
  # [ ] Minyear maxyear arguments
  # [ ] water body name argument
  n_max <- ifelse(.test, 50, Inf)
  # [x] report sample DateTime not just date
  print("Step 1/7: Read and clean NCCA")
  ncca <- .loadNCCA(
    NCCAsites2010, NCCAsites2015, NCCAwq2010,
    NCCAwq2015, NCCAhydrofiles2010,
    NCCAhydrofile2015, NCCAsecchifile2015,
    namingFile,
    Lakes = NULL,
    n_max = n_max
  ) %>%
  dplyr::select(-Finalized)

  print("Step 2/7: Read preprocessed Seabird files associated with GLENDA")
  seaBirdDf <- readr::read_rds(seaBird) %>%
    # since pH aren't being converted, need to set the explicit units
    dplyr::mutate(Explicit_Units= ifelse(CodeName == "pH", "unitless", Explicit_Units))

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
    ) %>%
    dplyr::select(-Finalized)


  print("Step 4/7: Read and clean CSMI data")
  CSMI <- .loadCSMI(csmi2010, csmi2015, csmi2021, namingFile = namingFile, n_max = n_max)

  print("Step 5/7: Read and clean NOAA data")
  NOAA <- .loadNOAAwq(noaaWQ, namingFile, noaaWQSites)
  noaaCTD <- readr::read_rds(noaaCTD)
  NOAA <- dplyr::bind_rows(NOAA, noaaCTD)
  print("Step 6/7: Combine and return full data")
  allWQ <- dplyr::bind_rows(ncca, GLENDA, CSMI, NOAA) %>%
  dplyr::mutate(
    SITE_ID = dplyr::coalesce(SITE_ID, STATION_ID)
  ) %>%
  dplyr::select(
    # time and space
    "UID", "Study", "SITE_ID", "Latitude", "Longitude", "sampleDepth", "stationDepth", "sampleDateTime",
    # analyte name
    "CodeName", "ANALYTE", "Category", "LongName", # [x] Add LongName from key tab for the "long name"
    # unit conversion
    "ConversionFactor", "TargetUnits", "Conversion", "ReportedUnits", "Explicit_Units",
    # measurement and limits
    "RESULT", "MDL", "MRL", "PQL", "METHOD",
    # QA
    "QAcode", "QAcomment", "LAB", "LRL", contains("QAconsiderations")
  ) %>%
  # catching some where units were inferred
  dplyr::mutate(
    QAcode = ifelse(grepl("no reported units", QAcomment, ignore.case = T),
      paste0(QAcode, sep = "; ", "U"), QAcode),
    QAcomment = ifelse((QAcode == "U") & is.na(QAcomment),
      "No reported units, assumed most common units in analyte-year", QAcomment
    )
  ) %>%
  dplyr::mutate(
    RL = dplyr::coalesce(LRL, MRL),
    # [x] Flag if below detection limit
    # If MDL is missing, 
    QAcode = dplyr::case_when(
      is.na(MDL) ~ QAcode,
      RESULT >= MDL ~ QAcode, 
      RESULT < MDL ~ paste(QAcode, "MDL", sep = "; "),
      RESULT < MDL ~ paste(QAcode, "MDL", sep = "; "),
      RESULT < RL ~ paste(QAcode, "RL", sep = "; "),
      .default = QAcode
      ),
    QAcomment = dplyr::case_when(
      is.na(MDL) ~ QAcomment,
      RESULT >= MDL ~ QAcomment, 
      RESULT < MDL ~ paste(QAcomment, "MDL", sep = "; "),
      .default = QAcomment
      )
  ) %>%
  dplyr::select(-c(MRL, LRL))

  print("Step 7/7 joining QC flags and suggestions to dataset")
  # [x] Split into flagged and unflagged values
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
      MDL, PQL, RL, LAB
    ))  %>%
     # handle Retain column by priority
  # We're joining by QAcode and QAcomment so this only removes based on the ocmment as well
  dplyr::mutate(
    RESULT2 = dplyr::case_when(
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
    dplyr::select(
      UID, Study, SITE_ID, Latitude, Longitude, stationDepth, sampleDateTime, sampleDepth,
      CodeName, LongName, RESULT, Units, MDL, PQL, RL, Unified_Flag, Unified_Comment, 
      Category, METHOD, LAB, Orig_QAcode=QAcode, Orig_QAcomment=QAcomment, 
      Orig_QAdefinition=Definition, ANALYTE_Orig_Name=ANALYTE, ReportedUnits,
      TargetUnits, ConversionFactor, Conversion_Note=Conversion, Retain_InternalUse=Retain,
      Action_InternalUse=Action) %>%
    dplyr::arrange(sampleDateTime, SITE_ID, sampleDepth, LongName)



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


# [ ]: Identify all analytes with missing Code Names and add to naming shee
# - If ANALYTE = ANL_CODE repalce ANL_CODE with NA
# - Then fill all missing ANL_CODE with nonmissing
# test <- data  %>%
#   filter(is.na(CodeName)) %>%
#   count(Study, ANALYTE, ANL_CODE, FRACTION, METHOD, MEDIUM)
# [ ] : ID all conversions with na (that aren't identical)
# [ ] Make a table of all flags after all is said and done so we can
# annotate them for end-users
