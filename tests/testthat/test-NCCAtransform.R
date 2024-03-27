test_that("NCCA Site data reads in, joins to itself, and has proper expected column names", {
  hasExpectedNames <- function(file) {
    c("SITE_ID", "LATITUDE", "LONGITUDE", "STATION_DEPTH", "WTBDY_NM", "NCCR_REG") %in% names(.readSite(file))
  }
  # Test that the individual files have expected column names
  siteFiles <- dir(path = siteDirectory, pattern = "site.*.csv", all.files =T, full.names=T, ignore.case = T)
  # Early studies don't report region and lake so won't be able to filter those yet
  expect_equal(sum(sapply(siteFiles, hasExpectedNames)), 34)


  nccaSites <- .readNCCASites(siteDirectory)
  expect_s3_class(nccaSites, "data.frame")
})


test_that("Full NCCA data can be loaded and joined", {
  ncca <- LoadNCCAfull(
    siteFiles = siteFiles, preFiles=preFiles, tenFiles=tenFiles, tenQAfile = tenQAfile, fifteenFiles=fifteenFiles,
    greatLakes=FALSE, Lakes=NULL, NCCAhydrofiles2010 = NCCAhydrofiles2010, NCCAhydrofile2015 = NCCAhydrofile2015,
    NCCAsecchifile2015 = NCCAsecchifile2015)


  expect_s3_class(ncca, "data.frame")
  expect_equal(class(ncca$Date), "Date")
  expect_equal(class(ncca$RESULT), "numeric")
  # Tests for missingness of space and time variables

})

ncca %>% 
  separate_longer_delim(QAcode, ",") %>%
  mutate(QAcode = stringr::str_remove_all(QAcode, " ")) %>%
  reframe(n = n(),
          Region = toString(unique(NCCRreg)),
          SAMPYEAR = toString(unique(SAMPYEAR)),
          .by = c(ANALYTE, QAcode, Definition, QAcomment)) %>%
  filter(!is.na(QAcode)) %>%
  filter(QAcode != "NA") %>%
  mutate(Definition = ifelse(is.na(Definition), toString(unique(Definition)), Definition)) 
  write_csv("NCCA2010QAcounts.csv")

