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

test_that("CSMI data files can loaded, cleaned, and joined", {
  CSMI <- LoadCSMI(
    file.path("L:", "Priv", "Great lakes Coastal", "2002-2010 Water Quality", "2010"),
    file.path("L:", "Priv", "Great lakes Coastal", "2015 CSMI Lake Michigan",
      "WQ data and database", "CSMI data & database", "CSMI_LkMich2015_Database_working_minsRivMouths.accdb"),
    file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)",
      "Lake Michign ML - General", "Raw_data", "CSMI", "2021"))
  testthat::expect_s3_class(CSMI, "data.frame")
})