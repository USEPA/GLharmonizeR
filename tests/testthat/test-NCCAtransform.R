test_that("NCCA Site data reads in and joins to itself"), {
  hasExpectedNames <- function(file) {
    c("SITE_ID", "LATITUDE", "LONGITUDE", "STATION_DEPTH", "WTBDY_NM", "NCCR_REG") %in% names(.readSite(file))
  }
  # Test that the individual files have expected column names
  siteFiles <- dir(path = siteDirectory, pattern = "site.*.csv", all.files =T, full.names=T, ignore.case = T)
  # Early studies don't report region and lake so won't be able to filter those yet
  expect_equal(sum(sapply(siteFiles, hasExpectedNames)), 33)


  nccaSites <- .readNCCASites(siteDirectory)
  expect_s3_class(nccaSites, "data.frame")
}


test_that("Full NCCA data can be loaded and joined", {
  ncca <- LoadNCCAfull(siteDirectory, preFiles, tenFiles, fifteenFiles, greatLakes=TRUE, Lakes=NULL,
                            NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015)

  expect_s3_class(ncca, "data.frame")
  expect_equal(class(ncca$Date), "Date")
  expect_equal(class(ncca$RESULT), "numeric")
  # Tests for missingness of space and time variables

})

ncca %>% 
filter(year(Date) == 2010, NCCR_REG=="Great Lakes") %>%
distinct(ANALYTE) %>%
arrange(ANALYTE)

