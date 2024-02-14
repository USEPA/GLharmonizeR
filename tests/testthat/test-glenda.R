test_that("GLENDA data file can be found", {
  testthat::expect_true(file.exists("Data/Raw/GLENDA/GLENDA.csv"))
})



test_that("GLENDA data can be read, cleaned and pivoted", {
  GLENDA <- readCleanGLENDA("Data/Raw/GLENDA/GLENDA.csv") %>%
    rename(UID = SAMPLE_ID, sampleDepth = SAMPLE_DEPTH_M, Depth = STN_DEPTH_M, Comment = RESULT_REMARK, RESULT = VALUE, Date = sampleDate) %>%
    select(UID, Date, Depth, sampleDepth, ANALYTE, RESULT, FRACTION, Comment, UNITS) %>%
    mutate(UID = as.character(UID), RESULT = as.numeric(RESULT),
    STUDY = "GLENDA")
  testthat::expect_s3_class(GLENDA, "data.frame")
})
