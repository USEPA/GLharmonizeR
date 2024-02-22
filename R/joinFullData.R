
#' Load and join all WQ data from 2010, 2015, and 2020/2021 from CSMI, GLENDA, and NCCA
#'
#' @description
#' `.LoadJoinAll` returns a dataframe data from 2010, 2015, and 2020/2021 from CSMI, GLENDA, and NCCA'
#'
#' @details
#' This is a hidden function that should not generally be used by users. 
#'
#' @param hydrofiles2010 a string specifying the directory containing NCCA hydrographic 2010 data
#' @param hydrofile2015 a string specifying the directory containing NCCA hydrographic 2015 data
#' @param secchifile2015 a string specifying the directory containing NCCA secchi 2015 data
#' @param siteFiles a string specifying the directory containing NCCA Site data 
#' @param preFiles a string specifying the path containing 2010's and earlier WQ data  
#' @param tenFiles a string specifying the directory containing NCCA WQ 2010 data
#' @param fifteenFiles a string specifying the directory containing NCCA wQ 2015 data
#' @param glendaData a string specifying the directory containing GLENDA data
#' @param csmi2010 a string specifying the directory containing CSMI 2010 data
#' @param csmi2015 a string specifying the directory containing CSMI 2015 data
#' @param csmi2021  a string specifying the directory containing CSMI 2021 data
#' @return dataframe of the fully joined water quality data from CSMI, NCCA, and GLENDA over years 2010, 2015, 2021 
.LoadJoinAll <- function(NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015, siteFiles, preFiles, tenFiles, fifteenFiles, glendaData,
                         csmi2010, csmi2015, csmi2021) {
  # Read NCCA hydrographic data 
  ncca <- LoadNCCAfull(siteFiles, preFiles, tenFiles, fifteenFiles, 
                          NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015)

  # READ GLENDA
  GLENDA <- readCleanGLENDA(glendaData) %>%
    dplyr::rename(UID = SAMPLE_ID, sampleDepth = SAMPLE_DEPTH_M, Depth = STN_DEPTH_M, Comment = RESULT_REMARK, RESULT = VALUE, Date = sampleDate) %>%
    dplyr::select(UID, Date, Depth, sampleDepth, ANALYTE, RESULT, FRACTION, Comment, UNITS) %>%
    dplyr::mutate(UID = as.character(UID), RESULT = as.numeric(RESULT),
    STUDY = "GLENDA")

  # READ CSMI
  CSMI <- LoadCSMI(csmi2010, csmi2015, csmi2021) %>%
    dplyr::rename(UID = `STIS#`) %>%
    #dplyr::select(Depth, FRACTION, LATITUDE, LONGITUDE, sampleDepth, ANALYTE, UNITS, RESULT, mdl, Date) %>%
    dplyr::mutate(UID = as.character(UID),
    STUDY = "CSMI")

  # Join data
  allWQ <- dplyr::bind_rows(ncca, CSMI, GLENDA) %>%
    tidyr::drop_na(RESULT)

  return(allWQ)
}


#' Unify the names and units of the fully joined data 
#'
#' @description
#' `.UnifyUnitsNames` returns a dataframe data from 2010, 2015, and 2020/2021 from CSMI, GLENDA, and NCCA'
#'
#' @details
#' This is a hidden function that should not generally be used by users. 
#'
#' @param namingFile a string specifying the file containing naming and units information 
#' @return dataframe with unified names and units 
.UnifyUnitsNames <- function(data, namingFile) {
  renamingTable <- dplyr::bind_rows(
    readxl::read_xlsx(namingFile, sheet = "GLENDA_Map",
    col_types = rep("text", 12)),
    readxl::read_xlsx(namingFile, sheet = "NCCA_Map",
    col_types = rep("text", 10)),
    readxl::read_xlsx(namingFile, sheet = "CSMI_Map", 
    col_types = rep("text", 11))
  ) %>%
    dplyr::distinct(ANALYTE, FRACTION, CodeName)
  Key <- readxl::read_xlsx(namingFile, sheet = "Key") 
  UnitConversions <- readxl::read_xlsx(namingFile, sheet = "UnitConversions")

  # Relabel analytes and remove unwanted ones
  data <- data %>%
    dplyr::left_join(renamingTable, by = c("ANALYTE", "FRACTION")) %>%
    dplyr::arrange(ANALYTE) %>%
    # dplyr::select(-c(ANALYTE, FRACTION, AnalMethod, ANL_CODE,  Comment)) %>%
    #dplyr::filter(CodeName != "Remove") %>%

  # Unit conversions
    dplyr::mutate(UNITS = ifelse(
      is.na(UNITS),
      names(sort(table(UNITS, useNA = "always")))[[1]],
      UNITS
    ), .by = c(CodeName, STUDY)) %>%
    dplyr::mutate(ReportedUnits = stringr::str_remove_all(UNITS, "/")) %>%
    dplyr::left_join(Key, by = "CodeName") %>%
    dplyr::left_join(UnitConversions, by = c("UNITS" = "ReportedUnits")) %>% 
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor),
           RESULT2 = RESULT * ConversionFactor)
  return(data)
}

#' Read and unite water quality data from CSMI, NCCA, and GLENDA across 2010, 2015, and 2020/2021
#'
#' @description
#' `LoadWQdata` is the main function of this package and returns a dataframe data from 2010, 2015,
#' and 2020/2021 from CSMI, GLENDA, and NCCA'
#'
#' @details
#' Loading data from all of these sources required QC on each respective source.
#'
#' @inheritParams .LoadJoinAll
#' @inheritParams .UnifyUnitsNames
#' 
#' @return dataframe with unified names and units for all WQ data
LoadWQdata <- function(NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015, siteFiles, preFiles, tenFiles, fifteenFiles, glendaData,
                         csmi2010, csmi2015, csmi2021,
                         namingFile) {
  df <- .LoadJoinAll(
    NCCAhydrofiles2010 = NCCAhydrofiles2010,
    NCCAhydrofile2015 = NCCAhydrofile2015,
    NCCAsecchifile2015 = NCCAsecchifile2015,
    siteFiles = siteFiles,
    preFiles = preFiles,
    tenFiles = tenFiles,
    fifteenFiles = fifteenFiles,
    glendaData = glendaData,
    csmi2010 = csmi2010,
    csmi2015 = csmi2015,
    csmi2021 = csmi2021)

  df <- .UnifyUnitsNames(data = df, namingFile = namingFile) 
  return(df)
}