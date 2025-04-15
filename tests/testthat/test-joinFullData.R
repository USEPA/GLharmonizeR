# Input full dataset
# - Has necessary column names : Study, Flag, QAcomment, QACode

# output: full dataset + unified flag strategy
# having:
# - coverage for all flags present in data
# - has column names: Flag, QAcomment, QACode
# - Missing values inserted appropriately
test_that("all data files can be found, read, cleaned, and joined. Expected column names and
          datatypes are present. Amount of missingness is expected, approximate size of data 
          sources are expected.", {
  options( readr.show_col_types = FALSE, readr.num_columns = 0, readr.show_progress = FALSE,
    readxl.show_progress = FALSE
  )
  df <- assembleData(.test = T, out = "fullData", binaryOut = TRUE)

  # df <- .UnifyUnitsNames(df, namingFile = namingFile)

  expect_s3_class(df, "data.frame")

  # Test observations per data set
  studyCounts <- df %>% distinct(UID, Study) %>% dplyr::count(Study) %>% dplyr::arrange(dplyr::desc(n)) %>% pull(n)
  testthat::expect_lt(max(studyCounts), 10000)
  testthat::expect_gt(min(studyCounts), 10)
  
  # Test column names
  colNames <- c("UID", "Study", "SITE_ID", "Latitude",
    "Longitude", "stationDepth", "sampleDepth", "sampleDate",
    "sampleTime", "DEPTH_CODE", "CodeName", "ANALYTE",
    "Category", "LongName", "Explicit_Units", "RESULT",
    "MDL", "RL", "TargetUnits", "ReportedUnits",
    "ConversionFactor", "QAcode", "QAcomment", "METHOD", "LAB")
  expect_equal(names(df), colNames)

  # Test datatypes
  colTypes <- c(
    "UID" = "character",
    "Study" = "character",
    "SITE_ID" = "character",
    "Latitude" = "double",
    "Longitude" = "double",
    "stationDepth" = "double",
    "sampleDepth" = "double",
    "sampleDate" = "double",
    "sampleTime" = "integer",
    "DEPTH_CODE" = "character",
    "CodeName" = "character",
    "ANALYTE" = "character",
    "Category" = "character",
    "LongName" = "character",
    "Explicit_Units" = "character",
    "RESULT" = "double",
    "MDL" = "double",
    "RL"  = "double",
    "TargetUnits" = "character",
    "ReportedUnits" = "character",
    "ConversionFactor" = "double",
    "QAcode" = "character",
    "QAcomment" = "character",
    "METHOD" = "character",
    "LAB" = "character"
  )
  expect_equal(colTypes, sapply(df, typeof))

  # Test column missingness
  colMissing <- c(
    "UID" = 0,
    "Study" = 0,
    "SITE_ID" = 0,
    "Latitude" = 0.012251017,
    "Longitude" = 0.012251017,
    "stationDepth" = 0.021785026,
    "sampleDepth" = 0.007184163,
    "sampleDate" = 0.006709910,
    "sampleTime" = 0.970244470,
    "DEPTH_CODE" = 0.710534529,
    "CodeName" = 0,
    "ANALYTE" = 0,
    "Category" = 0,
    "LongName" = 0,
    "Explicit_Units" = 0,
    "RESULT" = 0.016556008,
    "MDL" = 0.914304072,
    "RL" = 0.996903589,
    "TargetUnits" = 0,
    "ReportedUnits" = 0.249019368,
    "ConversionFactor" = 0.728390907,
    "QAcode" = 0.700847535,
    "QAcomment" = 0.643083560,
    "METHOD" = 0.706171404,
    "LAB" = 0.985460331
  )
  expect_equal(colMissing, colMeans(is.na(df)), tolerance = 0.05)
     

  # Test no duplicates
  expect_equal(anyDuplicated(df), 0)

})
