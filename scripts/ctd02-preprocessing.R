# SECOND NOAA CTD PROCESSING FILE TO RUN
# - 1m depth bins
# - standardizes names via oce
# - saves compressed output to rds


library(devtools)
library(tidyverse)
library(oce)
options(insertCalculatedDataCTD=TRUE)
source("scripts/ctd-functions.R")

teamsFolder <- file.path(fs::path_home(), "Environmental Protection Agency (EPA)", "Lake Michigan ML - General")
filepaths <- .getFilePaths()
seaBird <- filepaths["seaBird"]
noaaWQ <- filepaths["noaaWQ"]
n_max <- Inf

# https://usepa.sharepoint.com/:u:/r/sites/LakeMichiganML/Shared%20Documents/General/Raw_data/Seabird/16305_MI18M.cnv.cnv?csf=1&web=1&e=wkesQN
# [ ] KV: Not a high priority, but note that the CTD file paths here are just synced files from Sharepoint and could probably be read in directly from Sharepoint, rather than having a user-specific path here. These paths can be found if you go to the files on Teams and hit 'Copy link'.teamsFolder <- file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)", "Lake Michigan ML - General")
# - this will require some html work which might take a while so I'll wait on this
# - for now though, only EPA folk who have access to the teams directory can run this
# - I've broadened so it doen'st use my specific file path
teamsFolder <- file.path(fs::path_home(), "Environmental Protection Agency (EPA)", "Lake Michigan ML - General")
seaBird <- list.files(path = file.path(teamsFolder, "Raw_data", "Seabird"),
  pattern = , ".cnv$", full.names=T) 
seaBird <- seaBird[grepl("_MI", seaBird, ignore.case = t)]

test <- F
if (test) {
   seaBird <- seaBird[c(1:5, (length(seaBird) - 5): length(seaBird))]
}

seaBirdDf <- seaBird %>%
  purrr::map(\(x)
    # Note: .oce2df() function is from ctdProcessing.R 
    .oce2df(suppressWarnings(oce::read.oce(x)), studyName = "SeaBird", bin = TRUE, downcast = TRUE) %>% 
      mutate(
        UID=x,
        # This only works for normal names like MIXX
        # But we don't konw how to deal with the rest anyway right now
        UID = tools::file_path_sans_ext(stringr::str_split_i(UID, "_", i = -1 ))
      ), .progress = TRUE) %>%
   dplyr::bind_rows() %>%
 # [x] KV: Stop this function here and write out seabird.Rds data (see note below)
 saveRDS(file.path("..", "GL_Data", "GLENDA", "seabird.Rds"))


noaaFiles <- readRDS(url("https://github.com/kvitense/GL_Data/raw/refs/heads/main/NOAA/ctdFileMetaData.Rds", "rb")) %>% # KV: This file is output from NOAActdNameParsing.R
  # remove files where we don't know the station
  drop_na(SITE_ID) %>%
  select(ctdFiles, SITE_ID, sampleDateTime, stationDepth, Latitude, Longitude) %>%
  mutate(
    ctdFiles = file.path("/Users", "ccoffman", "Environmental Protection Agency (EPA)",
      "Lake Michigan ML - General", "Raw_data", "NOAA", "CTD 2007-2022", ctdFiles))

noaaCTDdf <- noaaFiles %>%
  mutate(
    data = purrr::map(ctdFiles, \(x)
      tryCatch({
      .oce2df(suppressWarnings(oce::read.oce(x)), studyName = "NOAActd", bin = TRUE, downcast = TRUE) %>%
        # Note: The above .oce2df() function is from ctdProcessing.R

        # take the station information froma the filepath
        dplyr::rename(Latitude.ctd = Latitude, Longitude.ctd  = Longitude, sampleDateTime.ctd = sampleDateTime, stationDepth.ctd = stationDepth)},
      error = function (e) {
        message(paste("Error reading file", basename(x)))
        NULL
      }),
    .progress = TRUE)
  ) %>%
  tidyr::unnest(cols = data) %>%
  # [ ] Should these be save in the GL_data package?
  saveRDS(file.path("..", "GL_Data", "NOAA", "noaaCTD.Rds"))
