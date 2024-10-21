#' Read in NCCA site data
#'
#' @description
#' `.readNCCASite2010` returns spatial data relating to study sites
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param NCCAsites2010 a string specifying the filepath of the data, This is usually a url
#' @return dataframe
.readNCCASite2010 <- function(NCCAsites2010) {
  # all depths are reported in meters
  sites <- readr::read_csv(NCCAsites2010, show_col_types = FALSE) %>%
    dplyr::filter(NCCR_REG == "Great Lakes") %>%
    # cutdown number of lats and longs
    # [x] how to choose the best projection here (DD vs DD83) is this constient across sources?
    # This only applies to targetted sites, actual coordinates are unique and settles this problem
    dplyr::select(
      SITE_ID,
      # No missingness, so no need to coalesce
      Latitude = ALAT_DD,
      Longitude = ALON_DD,
      stationDepth = STATION_DEPTH,
      WTBDY_NM
    )  %>% 
    # file 3 has a bunch of empty rows at the end
    # file 2 has missing lat/lons for some reason
    tidyr::drop_na()

    return(sites)
}

#' Read in NCCA site data
#'
#' @description
#' `.readNCCASite2015` returns spatial data relating to study sites
#'
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param NCCAsites2015 a string specifying the directory of the data
#' @return dataframe
.readNCCASite2015 <- function(NCCAsites2015) {
  # all depths are reported in meters
  sites <- readr::read_csv(NCCAsites2015, show_col_types = FALSE) %>%
    # cutdown number of lats and longs
    dplyr::select(
      SITE_ID,
      # [x] how to choose the best projection here (DD vs DD83) is this constient across sources?
      # This only applies to targetted sites, actual coordinates are unique and settles this problem
      # No missingness, so no need to coalesce
      Latitude = LAT_DD83,
      Longitude = LON_DD83,
      stationDepth = STATION_DEPTH,
      WTBDY_NM = GREAT_LAKE
    ) %>%
    tidyr::drop_na()
  
  return(sites)
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
.readNCCASites <- function(NCCAsites2010, NCCAsites2015) {
  df <- .readNCCASite2010(NCCAsites2010)
  df2 <- .readNCCASite2015(NCCAsites2015)

  return(dplyr::bind_rows(df, df2) %>% dplyr::distinct())
}
