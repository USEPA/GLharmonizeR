
# NCCA
# pre doesn't have any
#df <- read_csv("Data/Raw/NCCA/nca_waterchemdata.csv")
# 2010
qa10 <- c("Data/Raw/NCCA/nAssessedWaterChem2010.csv", "Data/Raw/NCCA/AssessedWaterChem2010.csv") %>%
  map_dfr(read_csv) %>%
  distinct(QACODE) %>%
  separate_longer_delim(QACODE, delim = ",") %>%
  drop_na() %>%
  mutate(QACODE = str_remove_all(QACODE, " ")) %>%
  distinct(QACODE) %>%
  arrange(QACODE) %>%
  filter(QACODE != "") %>%
  pull(QACODE)

# 2015
qa15 <- read_csv("Data/Raw/NCCA/ncca_2015_water_chemistry_great_lakes-data.csv") %>%
  distinct(NARS_FLAG, NARS_COMMENT) %>%
  mutate(NARS_FLAG = str_replace_all(NARS_FLAG, ",", ";")) %>%
  separate_longer_delim(cols = c(NARS_FLAG, NARS_COMMENT), delim= ";")
  drop_na(NARS_FLAG) %>%
  mutate(NARS_FLAG = str_remove_all(NARS_FLAG, " ")) %>%
  distinct(QACODE) %>%
  arrange(QACODE) %>%
  filter(QACODE != "") %>%
  pull(QACODE)

qaCodes <- readxl::read_xlsx("DRAFT_NCCA_QA_Codes_05.30.2017.xlsx", sheet= 1) %>%
  select(-...3) %>%
  drop_na()


  


