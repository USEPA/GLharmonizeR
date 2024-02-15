library(tidyverse)
# NCCA hydro FILES  
NCCAhydrofiles2010 = c("https://www.epa.gov/sites/default/files/2016-01/assessed_ncca2010_hydrolab.csv", 
"https://www.epa.gov/sites/default/files/2016-01/not_assessed_ncca2010_hydrolab.csv")
NCCAhydrofile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_hydrographic_profile_great_lakes-data.csv"
NCCAsecchifile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_secchi_great_lakes-data.csv" 

#NCCA WQ files
siteFiles <- "Data/Raw/NCCA"
preFiles <- c("Data/Raw/NCCA/nca_waterchemdata.csv")
tenFiles<- c("Data/Raw/NCCA/assessed_ncca2010_waterchem.csv", "Data/Raw/NCCA/nassessedWaterChem2010.csv") 
fifteenFiles <- c("Data/Raw/NCCA/ncca_2015_water_chemistry_great_lakes-data.csv")

glendaData <- "Data/Raw/GLENDA/GLENDA.csv"

# L drive data
# csmi2010 <- file.path("L:", "Priv", "Great lakes Coastal", "2002-2010 Water Quality", "2010")
# csmi2015 <- file.path("L:", "Priv", "Great lakes Coastal", "2015 CSMI Lake Michigan",
#   "WQ data and database", "CSMI data & database", "CSMI_LkMich2015_Database_working_minsRivMouths.accdb")
# csmi2021 <- file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)",
#   "Lake Michign ML - General", "Raw_data", "CSMI", "2021")
# namingFile <- file.path(
#   "C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)", "Lake Michigan ML - General", "Results", "Analytes3.xlsx"
# )
csmi2010 <- file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)", "Lake Michigan ML - General", "Raw_data", "CSMI", "2010")
csmi2015 <- file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)", "Lake Michigan ML - General", "Raw_data", "CSMI", "CSMI2015_newQuery.accdb")
csmi2021 <- file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)", "Lake Michigan ML - General", "Raw_data", "CSMI", "2021")

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
  STATION_DEPTH = coalesce(`Stn Depth (m)`, STATION_DEPTH),
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

