
#%% CSMI
test_that("all data files can be found, read, cleaned, and joined.", {
  options(readr.show_col_types=FALSE, readr.num_columns = 0, readr.show_progress = FALSE,
          readxl.show_progress= FALSE)
  df <- assembleData(NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015, ncca2010sites, ncca2015sites, tenFiles, tenQAfile, fifteenFiles, glendaData,
                           csmi2010, csmi2015, csmi2021, seaBirdFiles, namingFile, 
                           test = FALSE, out = "Data/fullData", binaryOut = TRUE)

  #df <- .UnifyUnitsNames(df, namingFile = namingFile)

  expect_s3_class(df, "data.frame")
})