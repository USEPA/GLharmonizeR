
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
.LoadAll <- function(NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015, ncca2010sites, ncca2015sites, tenFiles, tenQAfile, fifteenFiles, glendaData,
                         csmi2010, csmi2015, csmi2021) {
  # Read NCCA hydrographic data 
  ncca <- LoadNCCAfull(ncca2010sites, ncca2015sites, tenFiles, tenQAfile, fifteenFiles, 
                         NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015,
                         greatLakes=TRUE, Lakes=c("Lake Michigan"))


  # READ GLENDA
  GLENDA <- readCleanGLENDA(glendaData) %>%
    dplyr::rename(UID = SAMPLE_ID, sampleDepth = SAMPLE_DEPTH_M, stationDepth = STN_DEPTH_M, QAcomment = RESULT_REMARK, RESULT = VALUE) %>%
    dplyr::mutate(UID = as.character(UID), RESULT = as.numeric(RESULT))

  CSMI <- LoadCSMI(csmi2010, csmi2015, csmi2021) %>%
    dplyr::rename(UID = STIS) %>%
    dplyr::mutate(UID = as.character(UID))

  allWQ <- dplyr::bind_rows(
    ncca, GLENDA, CSMI
  ) %>% 
  dplyr::mutate(
    SITE_ID = dplyr::coalesce(SITE_ID, STATION_ID, SITE)
  ) %>%
  dplyr::select(
    -c(CLEAR_TO_BOTTOM, numerator, denominator, MONTH, SEASON, CRUISE_ID, VISIT_ID, DEPTH_CODE, SAMPLE_TYPE,
    QC_TYPE, PROJECT, `blk/dup other`, LATITUDE, LONGITUDE, STATION_ID, SITE, STATION, QA_CODE)
  )
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
  # compile the list of names for each data source
  renamingTable <- purrr::map2(
    list("GLENDA_Map", "NCCA_Map", "CSMI_Map"),
    list(13, 11, 12),
    .f = \(x,y)
    readxl::read_xlsx(
      namingFile, 
      sheet = x,
      col_types = rep("text", y))) %>%
  dplyr::bind_rows() %>%
  dplyr::select(-Units) %>%
  # remove empty rows from excel cells
  janitor::remove_empty(which = c("rows", "cols")) %>%
  dplyr::select(Study, ANALYTE, ANL_CODE, MEDIUM, FRACTION, Methods, CodeName) %>%
    #dplyr::mutate(
    #  ANL_CODE = ifelse(ANALYTE == ANL_CODE, NA, ANL_CODE),
    #  ANL_CODE = ifelse(is.na(ANL_CODE), mode(ANL_CODE), ANL_CODE),
    #  ANL_CODE = ifelse(ANL_CODE == "character", NA, ANL_CODE),
    #  .by = c(Study, ANALYTE, MEDIUM, FRACTION, Methods)
    #) %>%
    dplyr::rename(METHOD = Methods) %>%
    dplyr::distinct(Study, ANALYTE, ANL_CODE, FRACTION, MEDIUM, METHOD, .keep_all = TRUE)



  data <- data %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    # Make Analyte anl_code more consistent
    # dplyr::mutate(
    #   ANL_CODE = ifelse(ANALYTE == ANL_CODE, NA, ANL_CODE),
    #   ANL_CODE = ifelse(is.na(ANL_CODE), mode(ANL_CODE), ANL_CODE),
    #   ANL_CODE = ifelse(ANL_CODE == "character", NA, ANL_CODE),
    #   .by = c(Study, ANALYTE, MEDIUM, FRACTION, METHOD)
    # ) %>%
    # match nw to old names
    dplyr::left_join(renamingTable, 
      by = c("Study", "ANALYTE", "ANL_CODE", "FRACTION", "METHOD", "MEDIUM")) %>%
    # match desired units
    dplyr::left_join(
      readxl::read_xlsx(namingFile, sheet = "Key") %>%
        dplyr::rename(TargetUnits = Units),
      by = "CodeName") %>%
    dplyr::filter(CodeName != "Remove")

# TODO: Identify all analytes with missing Code Names and add to naming shee
# - If ANALYTE = ANL_CODE repalce ANL_CODE with NA
# - Then fill all missing ANL_CODE with nonmissing
# test <- data  %>% 
#   filter(is.na(CodeName)) %>%
#   count(Study, ANALYTE, ANL_CODE, FRACTION, METHOD, MEDIUM)

# TODO: ID all conversions with na (that aren't identical) 

  #UnitConversions <- readxl::read_xlsx(namingFile, sheet = "UnitConversions")

  # Relabel analytes and remove unwanted ones

  # Unit conversions
  #  dplyr::mutate(UNITS = ifelse(
  #    is.na(UNITS),
  #    names(sort(table(UNITS, useNA = "always")))[[1]],
  #    UNITS
  #  ), .by = c(CodeName, Study)) %>%
  #  dplyr::mutate(ReportedUnits = stringr::str_remove_all(UNITS, "/")) %>%
  #  dplyr::left_join(Key, by = "CodeName") %>%
  #  dplyr::left_join(UnitConversions, by = c("UNITS" = "ReportedUnits")) %>% 
  #  dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor),
  #         RESULT2 = RESULT * ConversionFactor)
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
#' @inheritParams .LoadAll
#' @inheritParams .UnifyUnitsNames
#' 
#' @return dataframe with unified names and units for all WQ data
LoadWQdata <- function(NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015, ncca2010sites, ncca2015sites, 
      tenFiles, tenQAfile, fifteenFiles, glendaData, csmi2010, csmi2015, csmi2021, namingFile) {
  
  df <- .LoadAll(NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015, ncca2010sites, ncca2015sites,
   tenFiles, tenQAfile, fifteenFiles, glendaData, csmi2010, csmi2015, csmi2021)


  df <- .UnifyUnitsNames(data = df, namingFile = namingFile) 
  return(df)
}