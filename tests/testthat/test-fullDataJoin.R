
# Input full dataset
# - Has necessary column names : Study, Flag, QAcomment, QACode

# output: full dataset + unified flag strategy
# having:
# - coverage for all flags present in data
# - has column names: Flag, QAcomment, QACode
# - Missing values inserted appropriately
test_that("all data files can be found, read, cleaned, and joined. Expected column names and 
          datatypes are present.", {
  options(readr.show_col_types=FALSE, readr.num_columns = 0, readr.show_progress = FALSE,
          readxl.show_progress= FALSE)
  df <- assembleData(NCCAhydrofiles2010, NCCAhydrofile2015, NCCAsecchifile2015, NCCAsites2010, NCCAsites2015, 
    NCCAwq2010, NCCAqa2010, NCCAwq2015, Glenda,
    csmi2010, csmi2021, seaBird, namingFile, 
                     test = T, out = "fullData", binaryOut = TRUE)

  #df <- .UnifyUnitsNames(df, namingFile = namingFile)

  expect_s3_class(df, "data.frame")

  # TODOS
  # [ ] Test column names
  # [ ] Test datatypes



  # Read flagMapper file

  # Join flagMapper to data

  # Drop old columns

})