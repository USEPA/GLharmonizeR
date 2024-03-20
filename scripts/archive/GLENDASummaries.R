library(tidyverse)
library(gtsummary)
library(gt)
library(devtools)
load_all()
readCleanGLENDA("Data/Raw/GLENDA/GLENDA.csv") %>% 
  select(YEAR, LAKE, SAMPLE_DEPTH_M, MEDIUM, ANALYTE) %>%
  tbl_summary() %>%
  as_gt() %>%
  tab_options(table.width = 180, table.font.size = 8) %>%
  gtsave(filename = "docs/figsTables/GLENDA_summary.png")
qaCodes <- readxl::read_xlsx("DRAFT_NCCA_QA_Codes_05.30.2017.xlsx", sheet= 1) %>%
  select(-...3) %>%
  drop_na(`Unique Qualifier Code`)

siteFiles <- "Data/Raw/NCCA"
preFiles <- c("Data/Raw/NCCA/nca_waterchemdata.csv")
tenFiles<- c("Data/Raw/NCCA/assessedWaterChem2010.csv", "Data/Raw/NCCA/nassessedWaterChem2010.csv") 
fifteenFiles <- c("Data/Raw/NCCA/ncca_2015_water_chemistry_great_lakes-data.csv")
readNCCA(siteFiles = siteFiles, preFiles = preFiles, tenFiles = tenFiles, fifteenFiles = fifteenFiles) %>%
  left_join(qaCodes, by = c("QACODE" = "Unique Qualifier Code")) %>%
  filter(between(LON, -88, -84.9),
         between(LAT, 41.7, 46)) %>%
  select(SAMPYEAR, DEPTH, ANALYTE) %>%
  tbl_summary() %>%
  as_gt()  %>%
  tab_options(table.width = 180, table.font.size = 8) %>%
  gtsave(filename = "docs/figsTables/NCCAsummary.png")
