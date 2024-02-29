test_that("Full GLENDA transformation on selected samples, (2001-2023)", {
  # Test if lists have same elements. 
  # taken from https://stackoverflow.com/questions/39338394/check-if-list-contains-another-list-in-r
  VectorIntersect <- function(v,z) {
    sapply(unique(v[v%in%z]), function(x) rep(x,min(sum(v==x),sum(z==x))))
  }
  is.contained <- function(v,z) {length(VectorIntersect(v,z))==length(unique(v))}

  # Once data is verified load it in right here.
  # testData <- readRDS("tests/testthat/fixtures/GLENDAfullTest.csv")
  testIDs <- readRDS("tests/testthat/fixtures/GLENDAtestIDs.Rds")
  df <- readCleanGLENDA("Data/GLENDA.csv", sampleIDs = testIDs)

  expect_s3_class(df, "data.frame")
  expect_true(is.contained(c("LATITUDE", "LONGITUDE", "sampleDate", 
   "SAMPLE_DEPTH_M", "ANALYTE", "VALUE", "UNITS", "FRACTION", "RESULT_REMARK", "STUDY", "STN_DEPTH_M", "METHOD"),
   colnames(df)))
  # no dates out of bounds as a simple check they parsed correctly
  expect_equal(sum(df$sampleDate < ymd_hms("2030-01-01 01:01:01")), length(df$sampleDate))
  expect_equal(sum(df$sampleDate > ymd_hms("1950-01-01 01:01:01")), length(df$sampleDate))
  expect_true(is.contained(df$sample_type, c("individuals", "insitu_meas"))) 
  expect_equal(unique(df$QC_TYPE), "routine field sample")
  expect_true(is.contained(df$SAMPLE_ID, testIDs))
})