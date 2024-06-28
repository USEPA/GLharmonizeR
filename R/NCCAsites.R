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
  readr::read_csv(NCCAsites2010, show_col_types=FALSE) %>%
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
#' @param NCCAsites2015 a string specifying the directory of the data
#' @return dataframe
.readNCCASite2015 <- function(NCCAsites2015) {
# all depths are reported in meters
  readr::read_csv(NCCAsites2015, show_col_types=FALSE) %>%
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


#' Read in NCCA site data 
#'
#' @description
#' `.readNCCASite2020` returns spatial data relating to study sites
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param NCCAsites2020 a string specifying the directory of the data
#' @return dataframe
.readNCCASite2020 <- function(NCCAsites2020) {
# all depths are reported in meters
  test <- readr::read_csv(NCCAsites2022, show_col_types=FALSE)

  test %>%
    dplyr::filter(MAJ_BAS_NM == "Great Lakes Region") %>%glimpse()
  test %>% select(US_L3NAME, US_L4NAME) %>% distinct() %>% filter(grepl("lake", ., ignore.case=T)) %>% filter(grepl("michigan", ., ignore.case=T))
  test %>% distinct(HUC8_NM)US_L4NAME, US_L3NAME
  readr::read_csv(NCCAsites2020, show_col_types=FALSE) %>%
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
.readNCCASites <- function(NCCAsites2010, NCCAsites2015) {
  df <- .readNCCASite2010(NCCAsites2010)
  df2 <- .readNCCASite2015(NCCAsites2015) 

  return(dplyr::bind_rows(df, df2) %>% dplyr::distinct())

}