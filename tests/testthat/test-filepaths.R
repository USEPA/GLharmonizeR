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


test_that("CSMI data files can be found", {
  early <- file.path("L:", "Priv", "Great lakes Coastal", "2002-2010 Water Quality", "2010")
  t15 <- file.path("L:", "Priv", "Great lakes Coastal", "2015 CSMI Lake Michigan",
    "WQ data and database", "CSMI data & database", "CSMI_LkMich2015_Database_working_minsRivMouths.accdb")
  t21 <- file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)",
    "Lake Michign ML - General", "Raw_data", "CSMI", "2021")

  files <- c(early, t15, t21)
  goodFiles <- sum(sapply(files, file.exists))
  gooddirs<- sum(sapply(files, dir.exists))
  goodTotal <- goodFiles + gooddirs

  testthat::expect_equal(length(files), goodTotal)
})


test_that("GLENDA data file can be found", {
  testthat::expect_true(file.exists("Data/Raw/GLENDA/GLENDA.csv"))
})
