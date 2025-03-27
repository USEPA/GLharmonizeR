library(devtools)
library(tidyverse)
library(oce)
load_all()

filepaths <- .getFilePaths()
seaBird <- filepaths["seaBird"]
n_max <- Inf

# KV: Per comment below, any code in a script that uses the below Analytes3 tables needs to be a core package function that can accommodate changes to these tables
key <- openxlsx::read.xlsx(filepaths["namingFile"], sheet = "Key") %>%
  dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
  dplyr::rename(TargetUnits = Units)

conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
  dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))

seaBirdrenamingTable <- openxlsx::read.xlsx(namingFile, sheet = "SeaBird_Map", na.strings = c("", "NA"))

# [ ] KV: Not a high priority, but note that the CTD file paths here are just synced files from Sharepoint and could probably be read in directly from Sharepoint, rather than having a user-specific path here. These paths can be found if you go to the files on Teams and hit 'Copy link'.teamsFolder <- file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)", "Lake Michigan ML - General")
seaBird <- list.files(path = file.path(teamsFolder, "Raw_data", "Seabird"),
  pattern = , ".cnv$", full.names=T) 
seaBird <- seaBird[grepl("_MI", seaBird, ignore.case = t)]
# [ ] remove _BAD$ files
test <- F
if (test) {
   seaBird <- seaBird[c(1:5, (length(seaBird) - 5): length(seaBird))]
}
seaBirdDf <- seaBird %>%
  purrr::map(\(x)
    .oce2df(suppressWarnings(oce::read.oce(x)), studyName = "SeaBird", bin = TRUE, downcast = TRUE) %>% 
      mutate(
        UID=x,
        # This only works for normal names like MIXX
        # But we don't konw how to deal with the rest anyway right now
        UID = tools::file_path_sans_ext(stringr::str_split_i(UID, "_", i = -1 ))
        ), .progress = TRUE) %>%
  dplyr::bind_rows() %>% 
  
  # Note: The above .oce2df() function is from ctdProcessing.R 
  
  # [ ] KV: Stop this function here and write out seabird.Rds data (see note below)
  
  # [ ] KV: Any parts of processing scripts that use the naming/conversions file (Analytes3) need to be moved to its own package function so that any updates to Analytes3 results in the data being updated dynamically when the core package functions are run. As it stands now, these CTD files and names are static and are inconsistent with the rest of the package and the idea behind the tables in Analytes3
  
  ########## Everything below here needs to be moved to a NEW GLNPO Seabird CTD package function that reads in seabird.Rds and does the remaining mapping and harmonization ########################
  
  
  dplyr::mutate(Study = "SeaBird") %>%
  mutate(
    STATION_ID = basename(STATION_ID),
    STATION_ID = gsub("^\\d+", "", STATION_ID),
    STATION_ID = stringr::str_remove_all(STATION_ID, ".CNV"),
    STATION_ID = stringr::str_remove_all(STATION_ID, ".BIN"),
    STATION_ID = stringr::str_remove_all(STATION_ID, ".cnv"),
    STATION_ID = stringr::str_remove_all(STATION_ID, ".bin"),
    UID = paste0(STATION_ID, "_", lubridate::date(sampleDateTime)),
    UNITS = dplyr::case_when(
      ANALYTE == "cpar" ~ "percent",
      ANALYTE == "pH" ~ "unitless"
    )
  )

seaBirdDf <- seaBirdDf %>%
  # Catch all for 2, 3, and fourth casts
  filter(!grepl("CAST", STATION_ID, ignore.case = T)) %>%
  # Not sure what this is
  filter(!grepl("DERIVE", STATION_ID, ignore.case = T)) %>%
  # Not sure what these are 
  filter(!grepl("(911)", STATION_ID, ignore.case = T)) %>%
  drop_na(RESULT) %>%
  dplyr::rename(ReportedUnits = UNITS) %>%
  # [ ] KV: In the remaining steps below, ensure you are CAREFULLY checking that each join is working as expected using the tests that were previously outlined.
  dplyr::left_join(seaBirdrenamingTable, by = c("Study", "ANALYTE")) %>%
  dplyr::filter(!grepl("remove", CodeName, ignore.case = T)) %>%
  dplyr::mutate(
    ReportedUnits = tolower(ReportedUnits),
    ReportedUnits = stringr::str_remove_all(ReportedUnits, "/")
  ) %>%
  # [ ] KV: Check that ReportedUnits are all formatted correctly for appropriate joining of the conversions table
  dplyr::left_join(key, by = "CodeName") %>%
  dplyr::mutate(TargetUnits = tolower(TargetUnits)) %>%
  dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits"))
  # Need to include code to actually do the conversions, even if there weren't any when you originally ran this. Everything in Analytes3 is subject to change and the code needs to be robust to changes

############# End of code that needs to be moved to a NEW GLNPO Seabird CTD package function for appropriate mapping, etc. ################################

# Save the cleaned seabird Files
saveRDS(seaBirdDf, "../GL_Data/GLENDA/seabird.Rds")


# Look at data
seaBirdDf %>% 
  distinct(STATION_ID) %>%
  print(n=399)
seaBirdDf %>% 
  distinct(CodeName)

