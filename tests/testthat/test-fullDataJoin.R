
#%% CSMI
test_that("CSMI data files can be found, read, cleaned, and joined.", {
  df <- .LoadAll(NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015, ncca2010sites, ncca2015sites, tenFiles, tenQAfile, fifteenFiles, glendaData,
                           csmi2010, csmi2015, csmi2021)

  df <- .UnifyUnitsNames(df, namingFile = namingFile)

  expect_s3_class(df, "data.frame")
})