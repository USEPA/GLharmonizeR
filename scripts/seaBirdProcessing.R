library(devtools)
library(tidyverse)
library(oce)
load_all()
teamsFolder <- file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)", "Lake Michigan ML - General")
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
    oce2df(suppressWarnings(oce::read.oce(x)), studyName = "SeaBird", bin = TRUE, downcast = TRUE) %>% 
      mutate(
        UID=x,
        # This only works for normal names like MIXX
        # But we don't konw how to deal with the rest anyway right now
        UID = tools::file_path_sans_ext(stringr::str_split_i(UID, "_", i = -1 ))
        ), .progress = TRUE) %>%
  dplyr::bind_rows() %>% 
  dplyr::mutate(Study = "SeaBird") %>%
  mutate(
    STATION_ID = basename(STATION_ID),
    STATION_ID = gsub("^\\d+", "", STATION_ID),
    STATION_ID = stringr::str_remove_all(STATION_ID, ".CNV"),
    STATION_ID = stringr::str_remove_all(STATION_ID, ".BIN"),
    STATION_ID = stringr::str_remove_all(STATION_ID, ".cnv"),
    STATION_ID = stringr::str_remove_all(STATION_ID, ".bin"),
    UID = paste0(STATION_ID, "_", lubridate::date(sampleDateTime))
  )

seaBirdDf <- seaBirdDf %>%
  # Catch all for 2, 3, and fourth casts
  filter(!grepl("CAST", STATION_ID, ignore.case = T)) %>%
  # Not sure what this is
  filter(!grepl("DERIVE", STATION_ID, ignore.case = T)) %>%
  # Not sure what these are 
  filter(!grepl("(911)", STATION_ID, ignore.case = T)) %>%
  drop_na(RESULT)


# Save the cleaned seabird Files
saveRDS(seaBirdDf, "../GL_Data/GLENDA/seabird.Rds")


# Look at data
seaBirdDf %>% 
  distinct(STATION_ID) %>%
  print(n=399)

