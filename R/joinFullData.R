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
  print("Step 0/6: Download data folder")
  .downloadData()

  # Load up the filepaths
  filepaths <- .getFilePaths()
  NCCAhydrofiles2010 <- filepaths["NCCAhydrofiles2010"]
  NCCAhydrofile2015 <- filepaths["NCCAhydrofile2015"]
  NCCAsecchifile2015 <- filepaths["NCCAsecchifile2015"]
  NCCAsites2010 <- filepaths["NCCAsites2010"]
  NCCAsites2015 <- filepaths["NCCAsites2015"]
  NCCAwq2010 <- filepaths["NCCAwq2010"]
  NCCAqa2010 <- filepaths["NCCAqa2010"]
  NCCAwq2015 <- filepaths["NCCAwq2015"]
  Glenda <- filepaths["Glenda"]
  csmi2010 <- filepaths["csmi2010"]
  csmi2021 <- filepaths["csmi2021"]
  seaBird <- filepaths["seaBird"]
  namingFile <- filepaths["namingFile"]

  # [ ] make arguement for source ("ALl", "GLENDA", "CSMI", "NCCA", "NOAA")
  # [ ] Minyear maxyear arguments
  # [ ] water body name arguement
  n_max = ifelse(.test, 50, Inf)
  # [x] report sample DateTime not just date
  print("Step 1/6: Load naming and unit conversion files")
  key <- readxl::read_xlsx(namingFile, sheet = "Key", .name_repair = "unique_quiet") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- readxl::read_xlsx(namingFile, sheet = "UnitConversions", .name_repair = "unique_quiet") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))

  seaBirdrenamingTable <- readxl::read_xlsx(namingFile, sheet= "SeaBird_Map", na = c("", "NA"), .name_repair = "unique_quiet") 

  print("Step 2/6: Read and clean NCCA")
  ncca <- .LoadNCCAfull(
    NCCAsites2010, NCCAsites2015, NCCAwq2010,
    NCCAwq2015, NCCAhydrofiles2010,
    NCCAhydrofile2015, NCCAsecchifile2015,
    Lakes=c("Lake Michigan"), namingFile,
    NCCAwqQA = NCCAwqQA, n_max = n_max) %>%
      dplyr::filter(!grepl("remove", CodeName, ignore.case=T)) %>%
      dplyr::select(-Years)

  print("Step 3/6: Read and clean GLENDA")
  GLENDA <- .readPivotGLENDA(Glenda, n_max = n_max) %>%
    .cleanGLENDA(., namingFile = namingFile, GLENDAflagsPath = NULL, imputeCoordinates = TRUE,
    GLENDAsitePath=GLENDAsitePath , GLENDAlimitsPath= GLENDAlimitsPath)
  # [ ] filter "remove" analytes
  
  print("Step 4/6: Read preprocessed Seabird files associated with GLENDA")

  # [ ] Move this out of the main function 
  seaBirdDf <- readRDS(seaBird)  %>%
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

  GLENDA <- dplyr::bind_rows(GLENDA, seaBirdDf)

  print("Step 5/6: Read and clean CSMI data")
  CSMI <- .LoadCSMI(csmi2010, csmi2021, namingFile = namingFile, n_max = n_max) %>%
    dplyr::select(-Years) %>%
    dplyr::filter(!grepl("remove", ANALYTE, ignore.case = T))
  # [x] filter "remove" analytes

  print("Step 5.5/6: Read and clean NOAA data")
  NOAA <- noaaReadClean(noaaWQ, namingFile)

  print("Step 6/6: Combine and return full data")
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
    "CodeName", "ANALYTE", "Category", "LongName", # [ ] Add LongName from key tab for the "long name"
    # unit conversion
    "ConversionFactor", "TargetUnits", "Conversion", "ReportedUnits",
    # measurement and limits
    "RESULT", "MDL", "MRL", "PQL",
    # QA
    "QAcode", "QAcomment", "LAB", "LRL", contains("QAconsiderations"), "Decision", "Action", "FLAG")))


  if (!is.null(out) & binaryOut) {
    print("Writing data to")
    print(paste0(out, ".Rds"))
    saveRDS(allWQ, paste0(out, ".Rds"))
  } else {
    out <- paste0(out, ".csv")
    print("Writing data to")
    print(out)
    readr::write_csv(allWQ, file = out, progress = readr::show_progress())
  }

  print("Clean up: Delete temporary data folder")
  unlink("GL_Data-main", recursive=TRUE)
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