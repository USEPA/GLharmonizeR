library(tidyverse)

# Water chemistry  copied from 
# L:\Priv\Great lakes Coastal\2021 CSMI Lake Michigan\Data\Water chem
# Contact is Annie Fosso
directoryPath<- file.path(
  "C:",
  "Users",
  "ccoffman",
  "Environmental Protection Agency (EPA)",
  "Lake Michigan ML - General",
  "Raw_data",
  "CSMI",
  "2021"
)

DL <- file.path(directoryPath, "Chem2021_detection limits.xlsx") %>%
  # The detection limit file contains MDLs and the values used to impute results <MDL.
  readxl::read_xlsx(sheet = "detection limits") %>%
  select(23:38) %>%
  mutate(Limit = coalesce(...23, ...24)) %>%
  select(-c(...23, ...24)) %>%
  pivot_longer(-Limit, values_to = "RESULT", names_to = "ANALYTE") %>%
  pivot_wider(id_cols = ANALYTE, names_from = Limit, values_from = RESULT, values_fn = mean) %>%
  select(-`NA`) 

WQ <- file.path(directoryPath, "Chem2021_FinalShare.xlsx") %>%
  readxl::read_xlsx(sheet = "DetLimitCorr") %>%
  select(-contains("...")) %>%
  mutate(across(ends_with("L"), ~ as.numeric(.))) %>%
  pivot_longer(15:29, names_to = "ANALYTE", values_to = "RESULT") %>%
  left_join(DL, by = "ANALYTE") %>%
  mutate(QA_CODE = case_when(
    # If a value is equal to 1/2 the respective MDL, either replace it with NA or flag as nondetect with imputed value (or whatever you need to do to ensure consistency across datasets)
    RESULT < `method detection limit` ~ "nondetect"
    ))

# CTD
# \Lake Michigan ML - General\Raw_data\CSMI\2021\2020 LM CSMI LEII CTD combined_Fluoro_LISST_12.13.21.xlsx
# Contact is James Gerads

## bin averaged over 1 meter depth intervals
## -9.99E-29 is NA
## There are already processed, formatted ready to use files Should we use that?
## 
CTD <- file.path(directoryPath, "2020 LM CSMI LEII CTD combined_Fluoro_LISST_12.13.21.xlsx") %>%
  readxl::read_xlsx(sheet = "Lake Michigan 2020 CSMI Data", skip = 1, na = c("", -9.99e-29)) %>% 
  rename(Transect = ...1, Station = ...2, Date = ...3) 

# Any other QC'ing necessary such as number of scans per bin, time elapsed
CTD <- map2(list(first = CTD[,1:24],  second = CTD[,c(1:3, 25:35)], third = CTD[,c(1:3, 36:43)]),
     list(5:24, 5:14, 5:11),
     \(df, cols) pivot_longer(
      data = df, 
      cols = cols, names_sep = " \\[", names_to = c("ANALYTE", "UNITS"), values_to = "RESULT")) %>%
     bind_rows() %>%
     mutate(UNITS = str_remove_all(UNITS, "\\]"),
            Depth = coalesce(`Depth [fresh water, m]`, `Depth [m]`)) %>%
     select(-c(`Depth [fresh water, m]`, `Depth [m]`))

# Join the data

CSMI2021 <- left_join(WQ, CTD, by = c("Station", "Date"))



# Appears there are no chl-a measurements for the Gaurdian data, but USGS collected chl-a data at some of the same sites within a week or so. Need to confirm with Ryan/Aabir.
# Lat-longs are missing but probably can be found in profile data below TRUE