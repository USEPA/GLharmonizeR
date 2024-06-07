library(devtools)
library(tidyverse)
load_all()
source(file.path("tests", "testthat", "helper-filepaths.R"))

newSurvey <- file.path("Data", "GLENDA", "2023Survey.csv")

rawData <- read_csv(newSurvey)

rawData %>% 
  select(contains("ANALYTE")) %>%
  pivot_longer(everything()) %>%
  distinct(value)

df <- .readPivotGLENDA(newSurvey)

df <- .cleanGLENDA(df, namingFile, GLENDAflagsPath= GLENDAflagsPath, imputeCoordinates = FALSE, GLENDAsitePath = GLENDAsitePath, 
  GLENDAlimitsPath = GLENDAlimitsPath)
glimpse(df)

outputData <- df %>%
  select(UID, YEAR.x, Latitude, Longitude, stationDepth, sampleDepth, QAcomment, sampleDateTime, CodeName, RESULT, Units, mdl)

glimpse(outputData)



outputData %>% 
  ggplot(aes(x = RESULT)) +
  geom_density() +
  facet_wrap(~CodeName, scales = "free")

outputData %>%
  filter(CodeName == "Cond")




outputData %>% 
  write_csv("Ryans2023GLENDAlook.csv")


