source("Code/dataCleaning/readCleanGLENDA.R")
library(gtsummary)
library(gt)
df <- readCleanGLENDA("Data/Raw/GLENDA/GLENDA.csv")

test <- df %>% 
  select(YEAR, LAKE, SAMPLE_DEPTH_M, MEDIUM, ANALYTE) %>%
  tbl_summary() %>%
  as_gt() %>%
  gtsave(filename = "Results/GLENDA_summary.png")
   
 

