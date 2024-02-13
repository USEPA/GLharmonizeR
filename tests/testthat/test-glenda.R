valid_url <- function(url_in,t=1.5){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  is.null(check)
}

# NCCA 2015 hydrographic and secchi
test_that("NCCA 2015 hydrographic files are accessible", {
  hydrofile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_hydrographic_profile_great_lakes-data.csv"
  hydrofile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_hydrographic_profile_great_lakes-data.csv"
  secchifile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_secchi_great_lakes-data.csv" 

  nccafiles <- c(hydrofile2015, hydrofile2015, secchifile2015)
  good_connections <- sum(sapply(nccafiles, valid_url))
  testthat::expect_equal(length(nccafiles), good_connections)
})

test_that("NCCA Water quality files are accessible", {
  preFiles <- "Data/Raw/NCCA/nca_waterchemdata.csv"
  tenFiles<- c("Data/Raw/NCCA/assessed_ncca2010_waterchem.csv", "Data/Raw/NCCA/nassessedWaterChem2010.csv") 
  fifteenFiles <- "Data/Raw/NCCA/ncca_2015_water_chemistry_great_lakes-data.csv"

  nccafiles <- c(preFiles, tenFiles, fifteenFiles)
  good_connections <- sum(sapply(nccafiles, file.exists))
  testthat::expect_equal(length(nccafiles), good_connections)
})

test_that("NCCA hydrological data can be read and cleaned", {
  NCCAhydro <- .readNCCAhydro(
    hydrofiles2010 = c("https://www.epa.gov/sites/default/files/2016-01/assessed_ncca2010_hydrolab.csv", 
  "https://www.epa.gov/sites/default/files/2016-01/not_assessed_ncca2010_hydrolab.csv"),
    hydrofile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_hydrographic_profile_great_lakes-data.csv",
    secchifile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_secchi_great_lakes-data.csv" 
    ) %>%
    select(UID, DATE_COL, SAMPLE_DEPTH_M, ANALYTE, RESULT, UNITS, STATION_DEPTH_M) %>%
    rename(Date=  DATE_COL, sampleDepth = SAMPLE_DEPTH_M, Depth = STATION_DEPTH_M) %>%
    mutate(UID = as.character(UID),
            STUDY = "NCCA")
  testthat::expect_s3_class(NCCAhydro, "data.frame")
}) 

test_that("NCCA water quality data read and cleaned", {
  siteFiles <- "Data/Raw/NCCA"
  preFiles <- c("Data/Raw/NCCA/nca_waterchemdata.csv")
  tenFiles<- c("Data/Raw/NCCA/assessed_ncca2010_waterchem.csv", "Data/Raw/NCCA/nassessedWaterChem2010.csv") 
  fifteenFiles <- c("Data/Raw/NCCA/ncca_2015_water_chemistry_great_lakes-data.csv")
  nccaWQ <- readNCCA(siteFiles = siteFiles, preFiles = NULL, tenFiles = tenFiles, fifteenFiles = fifteenFiles)  %>%
    filter(between(LONGITUDE, -88, -84.9),
           between(LATITUDE, 41.7, 46)) %>%
    select(UID, Date, DEPTH, ANALYTE, RESULT, UNITS, QAComment) %>%
    rename(sampleDepth = DEPTH, Comment = QAComment) %>%
    mutate(UID = as.character(UID), 
            STUDY = "NCCA")
  testthat::expect_s3_class(nccaWQ, "data.frame")
})
