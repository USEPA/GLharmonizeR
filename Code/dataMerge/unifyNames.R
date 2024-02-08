source("Code/dataCleaning/GLENDA/readCleanGLENDA.R")
source("Code/dataCleaning/CSMI/csmiJoinAll.r")
source("Code/dataCleaning/NCCA/waterQuality.r")
source("Code/dataCleaning/NCCA/hydrological.r")

library(tidyverse)
namingFile <- file.path(
  "C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)", "Lake Michigan ML - General", "Results", "Analytes3.xlsx"
)
renamingTable <- bind_rows(
  readxl::read_xlsx(namingFile, sheet = "GLENDA_Map",
  col_types = rep("text", 12)),
  readxl::read_xlsx(namingFile, sheet = "NCCA_Map",
  col_types = rep("text", 10)) ,
  readxl::read_xlsx(namingFile, sheet = "CSMI_Map", 
  col_types = rep("text", 10))
) %>%
  distinct(ANALYTE, FRACTION, CodeName)


NCCAhydro <- .readNCCAhydro(
  hydrofiles2010 = c("https://www.epa.gov/sites/default/files/2016-01/assessed_ncca2010_hydrolab.csv", 
"https://www.epa.gov/sites/default/files/2016-01/not_assessed_ncca2010_hydrolab.csv"),
  hydrofile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_hydrographic_profile_great_lakes-data.csv",
  secchifile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_secchi_great_lakes-data.csv" 
  ) %>%
  select(UID, DATE_COL, SAMPLE_DEPTH_M, ANALYTE, RESULT, UNITS, STATION_DEPTH_M) %>%
  rename(Date=  DATE_COL, sampleDepth = SAMPLE_DEPTH_M, Depth = STATION_DEPTH_M) %>%
  mutate(UID = as.character(UID),
          STUDY = "NCCA")

# READ NCCA WQ 
siteFiles <- "Data/Raw/NCCA"
preFiles <- c("Data/Raw/NCCA/nca_waterchemdata.csv")
tenFiles<- c("Data/Raw/NCCA/assessed_ncca2010_waterchem.csv", "Data/Raw/NCCA/nassessedWaterChem2010.csv") 
fifteenFiles <- c("Data/Raw/NCCA/ncca_2015_water_chemistry_great_lakes-data.csv")
nccaWQ <- readNCCA(siteFiles = siteFiles, preFiles = NULL, tenFiles = tenFiles, fifteenFiles = fifteenFiles)  %>%
  filter(between(LONGITUDE, -88, -84.9),
         between(LATITUDE, 41.7, 46)) %>%
  select(UID, Date, DEPTH, ANALYTE, RESULT, UNITS, QAComment) %>%
  rename(sampleDepth = DEPTH, Comment = QAComment) %>%
  mutate(UID = as.character(UID), 
          STUDY = "NCCA")


# READ GLENDA
GLENDA <- readCleanGLENDA("Data/Raw/GLENDA/GLENDA.csv") %>%
  rename(UID = SAMPLE_ID, sampleDepth = SAMPLE_DEPTH_M, Depth = STN_DEPTH_M, Comment = RESULT_REMARK, RESULT = VALUE, Date = sampleDate) %>%
  select(UID, Date, Depth, sampleDepth, ANALYTE, RESULT, FRACTION, Comment) %>%
  mutate(UID = as.character(UID), RESULT = as.numeric(RESULT),
  STUDY = "GLENDA")

# READ CSMI
CSMI <- LoadCSMI(
  file.path("L:", "Priv", "Great lakes Coastal", "2002-2010 Water Quality", "2010"),
  file.path("L:", "Priv", "Great lakes Coastal", "2015 CSMI Lake Michigan",
    "WQ data and database", "CSMI data & database", "CSMI_LkMich2015_Database_working_minsRivMouths.accdb"),
  file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)",
    "Lake Michign ML - General", "Raw_data", "CSMI", "2021")
) %>%
  rename(UID = `STIS#`) %>%
  select(`STIS#`, Depth, FRACTION, LATITUDE, LONGITUDE, sampleDepth, ANALYTE, UNITS, RESULT, mdl, Date) %>%
  mutate(UID = as.character(UID),
  STUDY = "CSMI") 

# Join data
allWQ <- bind_rows(NCCAhydro, nccaWQ, CSMI, GLENDA) %>%
  drop_na(RESULT) %>%
  mutate(FRACTION = case_when(
    FRACTION == "F" ~ "Filtrate",
    FRACTION == "U" ~ "Total/Bulk",
    FRACTION == "A" ~ "Filtrate",
    FRACTION == "M" ~ "Filtrate",
    FRACTION == "D" ~ "Filtrate",
    FRACTION == "V" ~ "Total/Bulk",
    FRACTION == "PCN" ~ "Residue",
    FRACTION == "Not applicable" ~ NA,
    .default = FRACTION
  )) %>%
# CSMI fraction labels
# From "L:\Priv\Great lakes Coastal\2010 MED Lake Michigan\2010\LMich10forms.xls"
# Sheet "flow_charts"

# Relabel analytes and remove unwanted ones
  left_join(renamingTable, by = c("ANALYTE", "FRACTION")) %>%
  arrange(ANALYTE) %>%
  select(-c(ANALYTE, UNITS, `STIS#`, FRACTION, AnalMethod, ANL_CODE, QA_CODE, Comment, Notes)) %>%
  filter(CodeName != "Remove")




# Analytes / study/ year look consistent
allWQ %>% 
  ggplot(aes(x = RESULT, fill = factor(year(Date)), group= factor(year(Date)))) +
  geom_histogram(alpha = 0.33) + 
  ggh4x::facet_grid2(rows = vars(CodeName), cols = vars(STUDY), 
  scales = "free", independent = "all", render_empty = FALSE) +
  theme_void() +
  theme(legend.position="none") +
  scale_x_log10()

allWQ %>% 
  ggplot(aes(x = RESULT, fill = factor(STUDY), group= factor(STUDY))) +
  geom_histogram(aes(y = ..density..), alpha = 0.33, position = "identity" ) +
  facet_wrap(~CodeName, scales = "free")

# Look fishy
# Diss NH4 not comparable between studies
# Diss NOx 
# Diss_Si
# Diss_Na
# Diss_NH3
# Secchi
# SRP
# Temp  (Is it just near vs offshore?)
# Tot_P
# Diss_Mg
