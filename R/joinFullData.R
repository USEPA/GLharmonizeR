#' Load and join all WQ data from 2010, 2015, and 2020/2021 from CSMI, GLENDA, and NCCA
#'
#' @description
#' `.LoadJoinAll` returns a dataframe data from 2010, 2015, and 2020/2021 from CSMI, GLENDA, and NCCA'
#'
#' @details
#' This is a hidden function that should not generally be used by users.
#'
#' @param out (optional) filepath to save the dataset to
#' @param .test (optional) boolean, if testing that data loads and joins, this flag only loads parts of the datasets to test it faster
#' @param binaryOut (optional) boolean, should saved data be RDS format for efficiency?
#' @export
#' @return full, harmonized dataset
assembleData <- function(out = NULL, .test = FALSE, binaryOut = FALSE) {
  # Load up the filepaths
  filepaths <- .getFilePaths()
  NCCAhydrofiles2010 <- filepaths["NCCAhydrofiles2010"]
  NCCAhydrofile2015 <- filepaths["NCCAhydrofile2015"]
  NCCAsecchifile2015 <- filepaths["NCCAsecchifile2015"]
  NCCAsites2010 <- filepaths["NCCAsites2010"]
  NCCAsites2015 <- filepaths["NCCAsites2015"]
  NCCAsites2022 <- filepaths["NCCAsites2022"]
  NCCAwq2010 <- filepaths["NCCAwq2010"]
  NCCAqa2010 <- filepaths["NCCAqa2010"]
  NCCAwq2015 <- filepaths["NCCAwq2015"]
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
  noaaWQ <- filepaths["noaaWQ"]
  # [ ] make arguement for source ("ALl", "GLENDA", "CSMI", "NCCA", "NOAA")
  # [ ] Minyear maxyear arguments
  # [ ] water body name arguement
  n_max <- ifelse(.test, 50, Inf)
  # [x] report sample DateTime not just date
  print("Step 1/8: Load naming and unit conversion files")
  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))

  seaBirdrenamingTable <- openxlsx::read.xlsx(namingFile, sheet = "SeaBird_Map", na.strings = c("", "NA"))

  print("Step 2/8: Read and clean NCCA")
  ncca <- .LoadNCCAfull(
    NCCAsites2010, NCCAsites2015, NCCAwq2010,
    NCCAwq2015, NCCAhydrofiles2010,
    NCCAhydrofile2015, NCCAsecchifile2015,
    Lakes = c("Lake Michigan"),
    n_max = n_max
  ) %>%
    # rename
    dplyr::left_join(openxlsx::read.xlsx(namingFile, sheet = "NCCA_Map", na.strings = c("", "NA")),
      by = c("Study", "ANALYTE", "ANL_CODE", "METHOD" = "Methods"), na_matches = "na"
    ) %>%
    # unit conversions
    dplyr::left_join(key, by = "CodeName") %>%
    # standardize units
    dplyr::mutate(
      ReportedUnits = stringr::str_remove(ReportedUnits, "/"),
      ReportedUnits = stringr::str_remove(ReportedUnits, "\\\\"),
      ReportedUnits = tolower(ReportedUnits),
      ReportedUnits = dplyr::case_when(
        # [x] can we make this more year specific
        # These were take from hdyro 2015 metadata file
        (Year == 2015) & (CodeName == "DO") ~ "mgl",
        (Year == 2015) & (CodeName == "Secchi") ~ "m",
        (Year == 2015) & (CodeName == "Temp") ~ "c",
        (Year == 2015) & (CodeName == "Cond") ~ "uscm",
        (Year == 2015) & (CodeName == "CPAR") ~ "%"
      )
    ) %>%
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::mutate(RESULT = dplyr::case_when(
      ReportedUnits == TargetUnits ~ RESULT,
      is.na(ReportedUnits) ~ RESULT,
      .default = RESULT * ConversionFactor
    )) %>%
    dplyr::select(-Units) %>%
    dplyr::rename(Units = TargetUnits) %>%
    dplyr::filter(CodeName != "Remove") %>%
    dplyr::select(-c(Finalized, Years))
  # [ ] Join to QA flag decisiosn and filter


  print("Step 3/8: Read and clean GLENDA")
  GLENDA <- .readPivotGLENDA(Glenda, n_max = n_max) %>%
    .cleanGLENDA(.,
      namingFile = namingFile, GLENDAflagsPath = NULL, imputeCoordinates = TRUE,
      GLENDAsitePath = GLENDAsitePath, GLENDAlimitsPath = GLENDAlimitsPath
    )


  print("Step 4/8: Read preprocessed Seabird files associated with GLENDA")
  seaBirdDf <- readr::read_rds(seaBird) %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    dplyr::left_join(seaBirdrenamingTable, by = c("Study", "ANALYTE")) %>%
    dplyr::mutate(
      ReportedUnits = tolower(ReportedUnits),
      ReportedUnits = stringr::str_remove_all(ReportedUnits, "/")
    ) %>%
    dplyr::left_join(key, by = "CodeName") %>%
    dplyr::mutate(TargetUnits = tolower(TargetUnits)) %>%
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::filter(!grepl("remove", CodeName, ignore.case = T))

  GLENDA <- dplyr::bind_rows(GLENDA, seaBirdDf) %>%
    select(-c(Finalized))



  print("Step 5/8: Read and clean CSMI data")
  CSMI <- .LoadCSMI(csmi2010, csmi2015, csmi2021, namingFile = namingFile, n_max = n_max) %>%
    # Years is in the date, and ANL_CODE is all NA
    dplyr::select(-c(Years, ANL_CODE, Finalized)) %>%
    dplyr::filter(!grepl("remove", ANALYTE, ignore.case = T))
  # [x] filter "remove" analytes

  print("Step 6/8: Read and clean NOAA data")
  NOAA <- noaaReadClean(noaaWQ, namingFile) %>%
    dplyr::select(-Years)

  print("Step 7/8: Combine and return full data")
  allWQ <- dplyr::bind_rows(
    ncca, GLENDA, CSMI, NOAA
  ) %>%
    dplyr::mutate(
      SITE_ID = dplyr::coalesce(SITE_ID, STATION_ID)
    ) %>%
    # [x] convert to any_of/one_of selection

    dplyr::select(dplyr::any_of(c(
      # time and space
      "UID", "Study", "SITE_ID", "Latitude", "Longitude", "sampleDepth", "stationDepth", "sampleDateTime",
      # analyte name
      "CodeName", "ANALYTE", "Category", "LongName", # [x] Add LongName from key tab for the "long name"
      # unit conversion
      "ConversionFactor", "TargetUnits", "Conversion", "ReportedUnits",
      # measurement and limits
      "RESULT", "MDL", "MRL", "PQL",
      # QA
      "QAcode", "QAcomment", "LAB", "LRL", contains("QAconsiderations"), "Decision", "Action", "FLAG"
    ))) %>%
    # catching some where units were inferred
    dplyr::mutate(
      QAcode = ifelse((is.na(QAcode)) & (grepl("no reported units", QAcomment, ignore.case = T)),
        "U", QAcode
      ),
      QAcomment = ifelse((QAcode == "U") & is.na(QAcomment),
        "No reported units, assumed most common units in analyte-year", QAcomment
      )
    )

  print("Step 8/8 joining QC flags and suggestions to dataset")
  # [x] Split into flagged and unflagged values
  flagged <- allWQ %>% dplyr::filter(!is.na(QAcode) | !is.na(QAcomment))
  notflagged <- allWQ %>% dplyr::filter(is.na(QAcode) & is.na(QAcomment))
  # [x] join flag explanations
  # fuzzy join solution from
  # https://stackoverflow.com/questions/69574373/joining-two-dataframes-on-a-condition-grepl
  flagged <- flagged %>%
    fuzzyjoin::fuzzy_join(
      flags,
      # didn't include sampyear, because codes seem to have stayed the same
      by = c("Study", "QAcode", "QAcomment"),
      match_fun = list(`==`, stringr::str_detect, stringr::str_detect),
      mode = "left"
    ) %>%
    dplyr::rename(
      # fuzzy matching appends x and y onto matching columns
      Study = Study.x, QAcode = QAcode.x, QAcomment = QAcomment.x
    ) %>%
    dplyr::mutate(
      # grab values in the mapping file in case we filled out by hand
      QAcode = dplyr::coalesce(QAcode, QAcode.y),
      QAcomment = dplyr::coalesce(QAcomment, QAcomment.y),
    ) %>%
    dplyr::select(
      -c(Study.y, QAcode.y, QAcomment.y)
    )
  # [x] recombine full dataset
  allWQ <- dplyr::bind_rows(flagged, notflagged)

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
