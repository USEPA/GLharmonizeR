library(devtools)
library(tidyverse)
load_all()
source(file.path("tests", "testthat", "helper-filepaths.R"))

newSurvey <- file.path("Data", "GLENDA", "2023Survey.csv")
df <- .readPivotGLENDA(newSurvey)

df <- .cleanGLENDA(df, namingFile, GLENDAflagsPath= GLENDAflagsPath, imputeCoordinates = FALSE, GLENDAsitePath = GLENDAsitePath, 
  GLENDAlimitsPath = GLENDAlimitsPath)
glimpse(df)

df %>%
  select(UID, YEAR.x, Latitude, Longitude, stationDepth, sampleDate, sampleDepth, QAcomment, sampleDateTime, CodeName, RESULT, Units, mdl) %>%
  write_csv("Ryans2023GLENDAlook.csv")