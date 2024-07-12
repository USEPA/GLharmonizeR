#' Unify the names and units of the fully joined data 
#'
#' @description
#' NOT USED
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
    openxlsx::read.xlsx(
      namingFile, 
      sheet = x,
      ) %>%
  dplyr::mutate(dplyr::mutate_all(as.character)) %>%
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
    # match nw to old names
    dplyr::left_join(renamingTable, 
      by = c("Study", "ANALYTE", "ANL_CODE", "FRACTION", "METHOD", "MEDIUM")) %>%
    # match desired units
    dplyr::left_join(
      openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
        dplyr::rename(TargetUnits = Units),
      by = "CodeName") %>%
    dplyr::filter(CodeName != "Remove")


  #UnitConversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions")

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



