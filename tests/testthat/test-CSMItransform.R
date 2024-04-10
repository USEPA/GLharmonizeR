test_that("csmi 2010 loads", {
  hasExpectedNames <- function(df) {
    c("Stn Depth (m)", "Notes", "STIS#","FRACTION","LATITUDE", "LONGITUDE", "ANALYTE", "UNITS",
 "RESULT", "mdl", "Date") %in% names(df)
  }
 
  df <- .LoadCSMI2010(csmi2010)

  expect_s3_class(df, "data.frame")
  expect_equal(sum(hasExpectedNames(df)), 11)

})

test_that("csmi 2015 loads", {
  hasExpectedNames <- function(df) {
    c("STIS#","SAMPLE_DEPTH", "ANALYTE", "RESULT",
      "UNITS", "mdl", "AnalMethod", "ANL_CODE",
      "LATITUDE", "LONGITUDE", "Date", "STATION_DEPTH") %in% names(df)
  }
 
  df <- .LoadCSMI2015(csmi2015)

  expect_s3_class(df, "data.frame")
  expect_equal(sum(hasExpectedNames(df)), 12)
})


test_that("csmi 2021 loads", {
  hasExpectedNames <- function(df) {
   c("SITE_ID", "Date", "Latitude", "Longitude", "SAMPLE_DEPTH",
     "ANALYTE", "UNITS", "RESULT", "mdl", "QA_CODE") %in% names(df)
  }
 
  df <- .LoadCSMI2021(csmi2021)

  expect_s3_class(df, "data.frame")
  expect_equal(sum(hasExpectedNames(df)), 10)
})

test_that("csmi loads", {
  hasExpectedNames <- function(df) {
   c("Stn Depth (m)","STIS#","FRACTION", "LATITUDE",
     "LONGITUDE", "ANALYTE", "UNITS", "RESULT",
     "mdl", "Date", "SAMPLE_DEPTH", "AnalMethod",
     "ANL_CODE", "STATION_DEPTH", "SITE_ID", "Latitude",
     "Longitude", "STUDY") %in% names(df)
  }

  df <- LoadCSMI(csmi2010, csmi2015, csmi2021, namingFile)

  expect_s3_class(df, "data.frame")
  expect_equal(sum(hasExpectedNames(df)), 18)
})

test <- readxl::read_xlsx(file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)", "Lake Michigan ML - General", "Results", "Analytes3.xlsx"), sheet =  "CSMI_Map")

df %>% 
  drop_na(RESULT) %>%
  mutate(FRACTION = NA) %>%
  filter(year(Date) == 2021) %>%
  filter(!ANALYTE %in% test$ANALYTE) %>%
  reframe(n = n(), .by= c(ANALYTE, FRACTION, UNITS)) %>%
  arrange(ANALYTE) %>%
  write_csv("tempCSMI2021.csv")

