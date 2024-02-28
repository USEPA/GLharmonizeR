library(tidyverse)
library(devtools)
load_all()
### GLENDA
glendaFile <- "Data/GLENDA.csv" 
df <- .readPivotGLENDA(glendaFile)
testIDs <- df %>%
  # For a reasonable sample size keeping this to newer years
  filter(YEAR > 2000) %>%
  # Every distinct we don't care about distinct values, but rather distinct
  # ways they are reported 
  distinct(YEAR, SAMPLE_ID, is.na(as.numeric(VALUE)), RESULT_REMARK) %>%
  # Grab the samples from each year with the most unique observations 
  filter(
    SAMPLE_ID == names(sort(table(SAMPLE_ID), decreasing = T))[[1]],
    .by = YEAR
    ) %>%
  # Make sure only one per year
  distinct(YEAR, .keep_all = T) %>%
  distinct(SAMPLE_ID) %>%
  pull(SAMPLE_ID)
saveRDS(testIDs, file = "tests/testthat/fixtures/GLENDAtestIDs.Rds")

testIDs <- readRDS("tests/testthat/fixtures/GLENDAtestIDs.Rds")

read_csv("Data/GLENDA.csv") %>%
  filter(SAMPLE_ID %in% testIDs) %>%
  select(-c(1,3,4,5,6,7,8,9,10,11,12,13,14,15,)) %>%
  write_csv("tests/testthat/fixtures/GLENDAtestIn.csv")
readCleanGLENDA("Data/GLENDA.csv", sampleIDs = testIDs) %>% 
  # only checking columns that had stuff done
  dplyr::select(YEAR, MEDIUM, SAMPLE_TYPE, QC_TYPE, SAMPLE_ID, ANALYTE, FRACTION, VALUE, UNITS, RESULT_REMARK) %>%
  write_csv("tests/testthat/fixtures/GLENDAtestOut.csv")

######################
### Checked manually and this sample contains:
# - VALUE: NA, numeric, <number, "not reported"
# - RESULT_REMARK: Nearly every REMARK
 df %>%
   filter(SAMPLE_ID %in% testIDs) %>% 
   mutate(VALUE = ifelse(is.na(as.numeric(VALUE)), VALUE, "Number")) %>%
   #distinct(RESULT_REMARK)
   distinct(VALUE)
#######################