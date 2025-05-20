# COMPILE AND PROCESS NOAA AND GLNPO SEABIRD CTD AND SAVE AS DATA TABLES
# - 1m depth bins - fixed in ctd-functions.R (.oce2df() function, starts at 0.5 m, bins in 1m increments)
# - standardizes names via oce
# - saves compressed output to rds in GL_Data repo

# ** Using oce 1.8-3 **
# *** Note that ctdFileMetaData.Rds was updated so this script needs to be rerun!!! ****


# **** Try installing latest version and rerun to see if will include binned files. ****




library(devtools)
library(tidyverse)
library(oce) # 1.8-3
options(insertCalculatedDataCTD=TRUE)
source("scripts/ctd_scripts/ctd03-functions.R")

# filepaths <- .getFilePaths()
# seaBird <- filepaths["seaBird"]
# noaaWQ <- filepaths["noaaWQ"]
# n_max <- Inf


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
    # Note: .oce2df() function is from ctd03-functions.R
    .oce2df(suppressWarnings(oce::read.oce(x)),  bin = TRUE, downcast = TRUE) %>%
      mutate(
        UID=x,
        # This only works for normal names like MIXX
        # But we don't know how to deal with the rest anyway right now
        UID = tools::file_path_sans_ext(stringr::str_split_i(UID, "_", i = -1 ))
      ), .progress = TRUE) %>%
   dplyr::bind_rows() %>%
 # [x] KV: Stop this function here and write out seabird.Rds data (see note below)
 saveRDS(file.path("..", "GL_Data", "GLENDA", "seabird_KV.Rds"))
 # NOTE: GL_Data is a GitHub repository and changes to this file need to be pushed to the repo


# Make sure push any changes to ctdFileMetaData.Rds to GL_Data repo before running this
noaaFiles <- readRDS(url("https://github.com/USEPA/GL_Data/raw/refs/heads/main/NOAA/ctdFileMetaData.Rds", "rb")) %>% # KV: This file is output from ctd01-NOAAstationFromFileName.R
  # remove files where we don't know the station
  drop_na(SITE_ID) %>%
  select(ctdFiles, SITE_ID, sampleDateTime, stationDepth, Latitude, Longitude) %>%
  mutate(
    ctdFiles = file.path(teamsFolder, "Raw_data", "NOAA", "CTD 2007-2022", ctdFiles))



noaaCTDdf <- noaaFiles %>%
  mutate(
    data = purrr::map(ctdFiles, \(x)
      tryCatch({
        # Note: .oce2df() function is from ctd-functions.R
      .oce2df(suppressWarnings(oce::read.oce(x)), bin = TRUE, downcast = TRUE) %>%
        # take the station information from the filepath
        dplyr::rename(Latitude.ctd = Latitude, Longitude.ctd  = Longitude, sampleDateTime.ctd = sampleDateTime, stationDepth.ctd = stationDepth)},
      error = function (e) {
        message(paste("Error reading file", basename(x)))
        NULL
      }),
    .progress = TRUE)
  ) %>%
  tidyr::unnest(cols = data) %>%
  saveRDS(file.path("..", "GL_Data", "NOAA", "noaaCTD_KV.Rds"))
# NOTE: GL_Data is a GitHub repository and changes to this file need to be pushed to the repo


##### DELETE ALL OF THIS BELOW ########


noaaCTDdf_old <- readRDS(file.path("..", "GL_Data", "NOAA", "noaaCTD.Rds"))
noaaCTDdf <- readRDS(file.path("..", "GL_Data", "NOAA", "noaaCTD_KV.Rds"))

noaaCTDdf_nopar <- noaaCTDdf %>% filter(!ANALYTE=="par")
noaaCTDdf_old_filt <- noaaCTDdf_old %>% filter(!SITE_ID %in% c("gvsubuoy"))

unique(noaaCTDdf$SITE_ID)
unique(noaaCTDdf_old$SITE_ID)

unique(noaaCTDdf$ANALYTE)
unique(noaaCTDdf_old_filt$ANALYTE)

sum(!noaaCTDdf$sampleDateTime %in% noaaCTDdf_old_filt$sampleDateTime)

look <- noaaCTDdf_nopar[!noaaCTDdf_nopar$sampleDateTime %in% noaaCTDdf_old_filt$sampleDateTime,]
look2 <- noaaCTDdf_nopar[grepl("raw00", noaaCTDdf_nopar$ctdFiles, ignore.case = F, fixed=TRUE),]

sum(is.na(noaaCTDdf_old_filt$sampleDateTime))

names(noaaCTDdf_nopar)
noaaCTDdf_newcheck <- noaaCTDdf_nopar %>% dplyr::select(-ctdFiles, -STATION_ID)
noaaCTDdf_oldcheck <- noaaCTDdf_old_filt %>% dplyr::select(-ctdFiles, -STATION_ID)


table(noaaCTDdf_newcheck$ANALYTE)
table(noaaCTDdf_oldcheck$ANALYTE)

# Difference is the temperature - new has 12k fewer obs

newtemp <- noaaCTDdf_nopar %>% filter(ANALYTE=="temperature")
oldtemp <- noaaCTDdf_old_filt %>% filter(ANALYTE=="temperature")

table(newtemp$SITE_ID)
table(oldtemp$SITE_ID)

newtemp_gh100 <- newtemp %>% filter(SITE_ID == "gh100") %>% arrange(sampleDateTime, sampleDepth)
oldtemp_gh100 <- oldtemp %>% filter(SITE_ID == "gh100") %>% arrange(sampleDateTime, sampleDepth)


# Difference seems to be that old version has temp for .bin.cnv files but new version does not
# However, not all .cnv are unbinned - some are binned but aren't .bin.cnv???
