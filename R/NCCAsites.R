#' Read in NCCA site data 
#'
#' @description
#' `.readNCCASite2010` returns spatial data relating to study sites
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the filepath of the data, This is usually a url
#' @return dataframe
.readNCCASite2010 <- function(filepath) {
# all depths are reported in meters
  readr::read_csv(filepath, show_col_types=FALSE) %>%
    # cutdown number of lats and longs
    # [x] how to choose the best projection here (DD vs DD83) is this constient across sources?
    # This only applies to targetted sites, actual coordinates are unique and settles this problem
    dplyr::rename(
      # No missingness, so no need to coalesce
      Latitude = ALAT_DD,
      Longitude = ALON_DD,
      stationDepth = STATION_DEPTH,
      NCCRreg = NCCR_REG
    ) %>%
    dplyr::select(SITE_ID, Latitude, Longitude, stationDepth, WTBDY_NM, NCCRreg) %>%
    # file 3 has a bunch of empty rows at the end
    # file 2 has missing lat/lons for some reason
    tidyr::drop_na()
}

#' Read in NCCA site data 
#'
#' @description
#' `.readNCCASite2015` returns spatial data relating to study sites
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the directory of the data
#' @return dataframe
.readNCCASite2015 <- function(filepath) {
# all depths are reported in meters
  readr::read_csv(filepath, show_col_types=FALSE) %>%
  # This column is all NAs
  dplyr::select(-WTBDY_NM) %>%
    # cutdown number of lats and longs
    dplyr::rename(
    # [x] how to choose the best projection here (DD vs DD83) is this constient across sources?
    # This only applies to targetted sites, actual coordinates are unique and settles this problem
      # No missingness, so no need to coalesce
      Latitude = LAT_DD,
      Longitude = LON_DD,
      stationDepth = STATION_DEPTH,
      NCCRreg = NCCA_REG,
      WTBDY_NM = GREAT_LAKE
    ) %>%
    dplyr::select(SITE_ID, Latitude, Longitude, stationDepth, WTBDY_NM, NCCRreg) %>%

    tidyr::drop_na()
}


#' Read in all NCCA site data
#' 
#' @description
#' `.readNCCASites` returns spatial data relating to study sites
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param filepath a string specifying the directory of the data
#' @return dataframe
.readNCCASites <- function(ncca2010sites, ncca2015sites) {
  df <- .readNCCASite2010(ncca2010sites)
  df2 <- .readNCCASite2015(ncca2015sites) 

  return(dplyr::bind_rows(df, df2) %>% dplyr::distinct())

}