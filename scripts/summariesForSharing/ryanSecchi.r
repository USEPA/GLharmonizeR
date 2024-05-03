source("Code/dataCleaning/readCleanGLENDA.r")
library(sf)
library(tidyverse)
library(kableExtra)

df <- read_csv("secchi.csv") %>%
  drop_na(VALUE_1) %>%
  select(SAMPLE_ID, YEAR, SEASON, MONTH,  SAMPLING_DATE, TIME_ZONE, LAKE, STN_DEPTH_M, LATITUDE, LONGITUDE, SAMPLE_DEPTH_M, DEPTH_CODE, SAMPLE_TYPE, QC_TYPE, VALUE_1) %>%
  rename(Secchi = VALUE_1) %>% 
  arrange(YEAR, SEASON) %>%
  write_csv("Secchi2017-2023.csv")
  