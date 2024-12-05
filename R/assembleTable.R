#' imputeNwiden
#'
#' @description
#' impute measurements, remove duplicates, pivot wider
#'
#' @details
#' impute measurements based off of MDL, and PQL as specified by the user, remove duplicates specified by users
#' desired spatial resolution (averaging over duplicates), then pivot to a wide format with the spatial variables
#' (lat/lon/depth) and time followed by the analyte measurements as their own columns
#'
#' @param df GLENDA dataframe in long format
#' @param latlonDigits (optional) default=4. number of digits to round Lat and Lons to,
#' @param imputeMethod (optional) default=NULL, one of 'halfMDL' which imputes missing values as exactly mdl/2, 'uniform' which samples 
#' the measuerment from Uniform(0, mdl), NULL doesn't perform imputation. If MDL isn't provided then the same process is followed for the PQL.
#'
#' @return a dataframe of wide format
imputeNwiden <- function(df, latlonDigits = 4, imputeMethod = NULL){
  # [x] Removed duplicates - have user input for Lat/Lon res for now
  if (imputeMethod == "halfMDL"){
    imputeFunction = function(mdl) mdl/2
  } else if (imputeMethod == "uniform") {
    imputeFunction = function(mdl) runif(n = 1, min = 0, max=mdl)
  } else {
    # don't do anything
    imputeFunction = function(mdl) mdl
  }

  removedDup <- df %>%  # removes 10% 
    dplyr::mutate(
      Lat = round(Latitude, digits = latlonDigits),
      Lng = round(Longitude, digits = latlonDigits),
    ) %>%
    dplyr::reframe(
      Result = mean(RESULT, na.rm =T), 
      MDL = mean(MDL, na.rm =T), 
      PQL = mean(PQL, na.rm =T), 
      RL = mean(RL, na.rm =T), 
      Study = toString(unique(Study)),
      Unified_Flag = toString(unique(Unified_Flag)),
      Unified_Comment = toString(unique(Unified_Comment)),
      Study = toString(unique(Study)),
      .by= c(Lat, Lng, sampleDateTime, CodeName, sampleDepth)) %>%
    # get yera for imputing
    dplyr::mutate(Year = lubridate::year(sampleDateTime)) %>%
    dplyr::mutate(
      # impute using DLs
      # https://19january2017snapshot.epa.gov/sites/production/files/2015-06/documents/whatthel.pdf
      # MDL < PQL couldn't fin how to incorporate RL
      # NOTE keep those reported as below MDL because their estimate is still likely better
      Result = dplyr::case_when(
        !is.na(Result) ~ Result,
        grepl("N", Unified_Flag) & (!is.na(MDL) | !is.na(PQL) | sum(!is.na(Result)) > 1) ~ imputeFunction(min(MDL, PQL, Result, na.rm=T)),
        .default = Result
      ),
      # make sure to manage different mdls over studies, times, and analytes
      .by = c(Study, Year, CodeName)
    )

  # pivot into wide format
  tabledData <- removedDup %>%
    pivot_wider(id_cols = c(Lat, Lng, sampleDateTime, sampleDepth), names_from = CodeName, values_from = Result)
  return(tabledData)
}