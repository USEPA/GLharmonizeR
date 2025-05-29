# COMPILE AND PROCESS NOAA AND GLNPO SEABIRD CTD AND SAVE FOR FURTHER PROCESSING WITH PACKAGE FUNCTIONS
# - uses .oce2df() in ctd03-functions.R
# - 1m depth bins - fixed in ctd03-functions.R (.oce2df() function, starts at 0.5 m, bins in 1m increments)
# - standardizes names via oce
# - saves compressed output to rds in GL_Data repo

# ** Using oce 1.8-4 **
# ** Note: need to be careful about which version of oce you use to process files. For example, par changed to uppercase PAR in 1.8-3 or thereabouts and broke the code


library(devtools)
library(tidyverse)
library(oce) # 1.8-3
options(insertCalculatedDataCTD=TRUE)
source("scripts/ctd_scripts/ctd03-functions.R")


teamsFolder <- file.path(fs::path_home(), "Environmental Protection Agency (EPA)", "Lake Michigan ML - General")
seaBird <- list.files(path = file.path(teamsFolder, "Raw_data", "Seabird"),
  pattern = , ".cnv$", full.names=T)
seaBird <- seaBird[grepl("_MI", seaBird, ignore.case = t)]

# seaBird2 <- seaBird[grepl("_MB", seaBird, ignore.case = t)] # No sites with MB


test <- F
if (test) {
   seaBird <- seaBird[c(1:5, (length(seaBird) - 5): length(seaBird))]
}

seaBirdDf <- seaBird %>%
  purrr::map(\(x)
    # Note: .oce2df() function is from ctd03-functions.R
    .oce2df(suppressWarnings(oce::read.oce(x, requireSalinity=FALSE)),  bin = TRUE, downcast = TRUE) %>%
      mutate(
        UID=x,
        # This only works for normal names like MIXX
        # But we don't know how to deal with the rest anyway right now
        UID = tools::file_path_sans_ext(stringr::str_split_i(UID, "_", i = -1 ))
      ), .progress = TRUE) %>%
   dplyr::bind_rows() %>%
 saveRDS(file.path("..", "GL_Data", "GLENDA", "seabird.Rds"))
 # NOTE: GL_Data is a GitHub repository and changes to this file need to be pushed to the repo

# Note only pH data in 2022


# Make sure push any changes to ctdFileMetaData.Rds to GL_Data repo before running this
noaaFiles <- readRDS(url("https://github.com/USEPA/GL_Data/raw/refs/heads/main/NOAA/ctdFileMetaData.Rds", "rb")) %>% # KV: This file is output from ctd01-NOAAstationFromFileName.R
  # remove files where we don't know the station
  drop_na(SITE_ID) %>%
  select(ctdFiles, SITE_ID, sampleDateTime, stationDepth, Latitude, Longitude) %>%
  mutate(
    ctdFiles = file.path(teamsFolder, "Raw_data", "NOAA", "CTD 2007-2022", ctdFiles))


# Need to add requireSalinity=FALSE or it will throw error if conductivity missing (there are some files that only contain temperature)
noaaCTDdf <- noaaFiles %>%
  mutate(
    data = purrr::map(ctdFiles, \(x)
      tryCatch({
        # Note: .oce2df() function is from ctd-functions.R
      .oce2df(suppressWarnings(oce::read.oce(x, requireSalinity=FALSE)), bin = TRUE, downcast = TRUE) %>%
        # take the station information from the filepath
        dplyr::rename(Latitude.ctd = Latitude, Longitude.ctd  = Longitude, sampleDateTime.ctd = sampleDateTime, stationDepth.ctd = stationDepth)},
      error = function (e) {
        message(paste("Error reading file", basename(x)))
        NULL
      }),
    .progress = TRUE)
  ) %>%
  tidyr::unnest(cols = data) %>%
  saveRDS(file.path("..", "GL_Data", "NOAA", "noaaCTD.Rds"))
# NOTE: GL_Data is a GitHub repository and changes to this file need to be pushed to the repo
