
#%% CSMI
test_that("CSMI data files can be found, read, cleaned, and joined.", {
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
  CSMI <- LoadCSMI(early, t15, t21)
  testthat::expect_s3_class(CSMI, "data.frame")
})


#%% GLENDA
test_that("GLENDA data file can be found, read, cleaned, and pivoted", {
  filepath <- "Data/Raw/GLENDA/GLENDA.csv"
  testthat::expect_true(file.exists(filepath))
  GLENDA <- readCleanGLENDA(filepath) %>%
    rename(UID = SAMPLE_ID, sampleDepth = SAMPLE_DEPTH_M, Depth = STN_DEPTH_M, Comment = RESULT_REMARK, RESULT = VALUE, Date = sampleDate) %>%
    select(UID, Date, Depth, sampleDepth, ANALYTE, RESULT, FRACTION, Comment, UNITS) %>%
    mutate(UID = as.character(UID), RESULT = as.numeric(RESULT),
    STUDY = "GLENDA")
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


