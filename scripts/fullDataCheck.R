library(tidyverse)
# NCCA hydro FILES  
NCCAhydrofiles2010 = c("https://www.epa.gov/sites/default/files/2016-01/assessed_ncca2010_hydrolab.csv", 
"https://www.epa.gov/sites/default/files/2016-01/not_assessed_ncca2010_hydrolab.csv")
NCCAhydrofile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_hydrographic_profile_great_lakes-data.csv"
NCCAsecchifile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_secchi_great_lakes-data.csv" 

#NCCA WQ files
siteFiles <- file.path("Data", "NCCA")
preFiles <- file.path("Data", "NCCA", "nca_waterchemdata.csv")
tenFiles<- c(file.path("Data", "NCCA", "assessed_ncca2010_waterchem.csv"),
            file.path("Data", "NCCA", "nassessedWaterChem2010.csv"))
fifteenFiles <- file.path("Data", "NCCA", "ncca_2015_water_chemistry_great_lakes-data.csv")

glendaData <- file.path("Data", "GLENDA.csv")

# L drive data
# csmi2010 <- file.path("L:", "Priv", "Great lakes Coastal", "2002-2010 Water Quality", "2010")
# csmi2015 <- file.path("L:", "Priv", "Great lakes Coastal", "2015 CSMI Lake Michigan",
#   "WQ data and database", "CSMI data & database", "CSMI_LkMich2015_Database_working_minsRivMouths.accdb")
# csmi2021 <- file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)",
#   "Lake Michign ML - General", "Raw_data", "CSMI", "2021")
# namingFile <- file.path(
#   "C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)", "Lake Michigan ML - General", "Results", "Analytes3.xlsx"
# )
csmi2010 <- file.path("Data", "CSMI", "2010")
csmi2015 <- file.path("Data", "CSMI", "CSMI2015_newQuery.accdb")
csmi2021 <- file.path("Data", "CSMI", "2021")

namingFile <- file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)", "Lake Michigan ML - General", "Results", "Analytes3.xlsx")

allWQ <- LoadWQdata(
  hydrofiles2010 = NCCAhydrofiles2010,
  hydrofile2015 = NCCAhydrofile2015,
  secchifile2015 = NCCAsecchifile2015,
  siteFiles = siteFiles,
  preFiles = preFiles,
  tenFiles = tenFiles,
  fifteenFiles = fifteenFiles,
  glendaData = glendaData,
  csmi2010 = csmi2010,
  csmi2015 = csmi2015,
  csmi2021 = csmi2021,
  namingFile = namingFile
) %>%
mutate(
  SAMPLE_DEPTH = coalesce(sampleDepth, SAMPLE_DEPTH),
  ) %>%
select(-c(sampleDepth, `Stn Depth (m)`))

# Analytes / study/ year look consistent
# Check with and without log scale
allWQ %>% 
  ggplot(aes(x = RESULT, fill = factor(year(Date)), group= factor(year(Date)))) +
  geom_histogram(alpha = 0.33) + 
  ggh4x::facet_grid2(rows = vars(CodeName), cols = vars(STUDY), 
  scales = "free", independent = "all", render_empty = FALSE) +
  theme_void() +
  theme(legend.position="none") +
  scale_x_log10()

# Pooled across years comparing studies
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

############ TEMP 
# Generate csmi names

CSMInames <- allWQ %>% 
  filter(STUDY == "CSMI") %>%
  drop_na(RESULT) %>% 
  reframe(
    Units = toString(unique(Units)),
    Method = toString(unique(AnalMethod)),
    Years = max(year(Date), na.rm = T),
    n = n(), 
    .by = c(ANALYTE, FRACTION, ANL_CODE, CodeName, `Lepak input`, Comment))  %>%
    arrange(Years, ANALYTE)
CSMIkey <- file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)", "Lake Michigan ML - General", "Results", "Analytes3.xlsx") %>%
  readxl::read_xlsx(path = ., sheet = "CSMI_Map") %>%
  dplyr::mutate(FRACTION = dplyr::case_when(
    FRACTION == "F" ~ "Filtrate",
    FRACTION == "U" ~ "Total/Bulk",
    FRACTION == "A" ~ "Filtrate",
    FRACTION == "M" ~ "Filtrate",
    FRACTION == "D" ~ "Filtrate",
    FRACTION == "V" ~ "Total/Bulk",
    FRACTION == "PCN" ~ "Residue",
    FRACTION == "Not applicable" ~ NA,
    .default = FRACTION
  ))


CSMInames %>%
  full_join(
    CSMIkey, 
    by = c("ANALYTE", "FRACTION", "ANL_CODE", "CodeName", "Units", "Years")) %>%
    select(ANALYTE, FRACTION, ANL_CODE, Methods, Years, Units, CodeName, `RL Agree?`, `Original comment/observation`, `Resolution Comment`) %>%
    distinct(ANALYTE, FRACTION, ANL_CODE, Methods, Years, Units, CodeName, `RL Agree?`, `Original comment/observation`, `Resolution Comment`) %>%
    write_csv("CSMInames.csv", na = "")

