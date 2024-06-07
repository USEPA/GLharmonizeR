
#%% CSMI
test_that("all data files can be found, read, cleaned, and joined.", {
  options(readr.show_col_types=FALSE, readr.num_columns = 0, readr.show_progress = FALSE,
          readxl.show_progress= FALSE)
  df <- assembleData(NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015, NCCAsites2010, NCCAsites2015, 
    NCCAwq2010, NCCAqa2010, NCCAwq2015, Glenda,
    csmi2010, csmi2015, csmi2021, seaBird, namingFile, 
                     test = F, out = "data/fullData.Rds", binaryOut = TRUE)

  #df <- .UnifyUnitsNames(df, namingFile = namingFile)

  expect_s3_class(df, "data.frame")
})