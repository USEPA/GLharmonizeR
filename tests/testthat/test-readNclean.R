
#%% CSMI
test_that("CSMI data files can be found, read, cleaned, and joined.", {
  csmi2010 <- file.path(here::here(), "Data", "CSMI", "2010")
  csmi2015 <- file.path(here::here(), "Data", "CSMI", "CSMI2015_newQuery.accdb")
  csmi2021 <- file.path(here::here(), "Data", "CSMI", "2021")

  files <- c(csmi2010, csmi2015, csmi2021)
  goodFiles <- sum(sapply(files, file.exists))

  # files exist
  testthat::expect_equal(length(files), goodFiles)

  # Test input datatypes
  ## 2010
  testdata <- readxl::read_xlsx(file.path(csmi2010, "GL2010db.xlsx"), skip = 8, n_max = 4)
  test <- unname(sapply(testdata, typeof))
  csmi2010Types <- c("double", "character", "character", "character", "character", "character",
        "character", "double", "character", "double", "double", "double",
        "double", "character", "double", "double", "character", "double",
        "double", "double", "character", "double", "double", "double",
        "double", "character", "double", "character", "character", "double",
        "double", "double", "double", "character", "double", "double",
        "double", "double", "double", "logical", "double", "double")
  testthat::expect_equal(sum(test == csmi2010Types), length(test))

  ## 2015
  test <-   file.path(csmi2021, "Chem2021_FinalShare.xlsx") %>%
    readxl::read_xlsx(sheet = "DetLimitCorr", n_max = 4) 
  test <- unname(sapply(test, typeof))
  csmi2021Types <- c("double", "character", "character", "character", "character", "logical",
    "character", "double", "character", "double", "logical", "double",
    "character", "character", "double", "double", "double", "double",
    "double", "double", "double", "double", "double", "double",
    "double", "double", "double", "double", "double", "logical",
    "logical", "logical", "logical", "logical", "logical", "logical",
    "logical")
  testthat::expect_equal(sum(test == csmi2021Types), length(test))


  CSMI <- LoadCSMI(csmi2010, csmi2015, csmi2021)
  testthat::expect_s3_class(CSMI, "data.frame")
})


#%% GLENDA
test_that("GLENDA data file can be found, read, cleaned, and pivoted", {
  filepath <- "Data/GLENDA.csv"
  testthat::expect_true(file.exists(filepath))

  # Test column data types
  test <- read_csv(filepath, n_max = 4)
  test <- unname(sapply(test, typeof))
  glendaTypes <-  c("double", "double", "character", "character", "character", "character",
    "character", "character", "double", "double", "double", "character",
    "character", "double", "character", "character", "character", "character",
    "character", "character", "character", "double", "character", "character",
    "character", "character", "character", "character", "double", "character",
    "character", "character", "character", "character", "character", "double",
    "character", "character", "character", "logical", "character", "character",
    "double", "character", "character", "character", "character", "character",
    "character", "double", "character", "character", "character", "logical",
    "character", "character", "double", "character", "character", "character",
    "logical", "character", "character", "double", "character", "character",
    "character", "logical", "character", "character", "double", "character",
    "character", "character", "character", "character", "character", "double",
    "character", "character", "character", "logical", "character", "character",
    "double", "character", "character", "character", "character", "logical",
    "logical", "logical", "logical", "logical", "logical", "logical",
    "logical", "logical", "logical", "logical", "logical", "logical",
    "logical", "logical", "logical", "logical", "logical", "logical"  ,
    "logical", "logical", "logical", "logical", "logical", "logical",
    "logical", "logical", "logical", "logical", "logical", "logical",
    "logical", "logical", "logical", "logical", "logical", "logical",
    "logical", "logical", "logical", "logical", "logical", "logical",
    "logical", "logical", "logical", "logical", "logical", "logical",
    "logical", "logical", "logical", "logical", "logical", "logical"  ,
    "logical", "logical", "logical", "logical", "logical", "logical",
    "logical", "logical", "logical", "logical", "logical", "logical",
    "logical", "logical", "logical", "logical", "logical", "logical",
    "logical", "logical", "logical", "logical", "logical", "logical",
    "logical", "logical", "logical", "logical", "logical", "logical"  ,
    "logical", "logical", "logical", "logical", "logical", "logical",
    "logical", "logical", "logical", "logical", "logical", "logical",
    "logical", "logical", "logical", "logical", "logical", "logical",
    "logical", "logical", "logical", "logical", "logical", "logical",
    "logical", "logical", "logical", "logical", "logical", "logical"  ,
    "logical", "logical", "logical", "logical", "logical")
  testthat::expect_equal(sum(test == glendaTypes), length(test))

  GLENDA <- readCleanGLENDA(filepath)
  testthat::expect_s3_class(GLENDA, "data.frame")
})

#%% NCCA
test_that("NCCA hydrological data can be found, read, and cleaned", {
  hydrofile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_hydrographic_profile_great_lakes-data.csv"
  hydrofile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_hydrographic_profile_great_lakes-data.csv"
  secchifile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_secchi_great_lakes-data.csv" 

  nccafiles <- c(hydrofile2015, hydrofile2015, secchifile2015)
  good_connections <- sum(sapply(nccafiles, valid_url))
  testthat::expect_equal(length(nccafiles), good_connections)

  NCCAhydro <- .readNCCAhydro(
    hydrofiles2010 = c("https://www.epa.gov/sites/default/files/2016-01/assessed_ncca2010_hydrolab.csv", 
  "https://www.epa.gov/sites/default/files/2016-01/not_assessed_ncca2010_hydrolab.csv"),
    hydrofile2015 = hydrofile2015,
    secchifile2015 = secchifile2015 
    )
  testthat::expect_s3_class(NCCAhydro, "data.frame")
}) 


test_that("NCCA water quality data read and cleaned", {
  siteFiles <- file.path(here::here(), "Data", "NCCA")
  preFiles <- file.path(here::here(), "Data", "NCCA", "nca_waterchemdata.csv")
  tenFiles<- c(file.path(here::here(), "Data", "NCCA", "assessed_ncca2010_waterchem.csv"), file.path("Data", "NCCA", "nassessedWaterChem2010.csv"))
  fifteenFiles <- file.path(here::here(), "Data", "NCCA", "ncca_2015_water_chemistry_great_lakes-data.csv")
  nccaWQ <- .readNCCA(siteFiles = siteFiles, preFiles = NULL, tenFiles = tenFiles, fifteenFiles = fifteenFiles)
  testthat::expect_s3_class(nccaWQ, "data.frame")
})


