source("Code/dataCleaning/GLENDA/readCleanGLENDA.R")
library(tidyverse)

names <- readCleanGLENDA("Data/Raw/GLENDA/GLENDA.csv") %>%
  distinct(ANALYTE) %>% 
  arrange() %>%
  filter(! (ANALYTE %in% WQXnames$Name))
  pull(ANALYTE)


# For package, can directly link it to the web data
# https://cdx.epa.gov/wqx/download/DomainValues/Characteristic.CSV
WQXnames <- read_csv("Data/Meta/Characteristic.csv")

WQXnames %>%
  filter(! (Name %in% names))



