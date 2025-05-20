# Second exploratory script to get the set of all analyte names in the CTD file (both GLNPO Seabird and NOAA) as well as the metadata (lat/lon, date, time) in these files.
# Should be run after ctd01-NOAAstationFromFileName.R
# This script was used to inform the names that go into ctd03-functions.R

# ** Using oce 1.8-3 **

library(devtools)
devtools::load_all()
library(tidyverse)
filepaths <- .getFilePaths()
# filepaths["seaBird"]


teamsFolder <- file.path(fs::path_home(), "Environmental Protection Agency (EPA)", "Lake Michigan ML - General")
seaBird <- list.files(path = file.path(teamsFolder, "Raw_data", "Seabird"),
                      pattern = , ".cnv$", full.names=T)
seaBird <- seaBird[grepl("_MI", seaBird, ignore.case = t)]


# Check to see if names are same across all seabird files
.getCTDNames <- function(file) {
  try(oce::read.oce(file) |>
    _@data |>
    names())
}
.getCTDMeta <- function(file) {
  try(data <- oce::read.oce(file))
  #meta <- data@metadata
  meta <- data.frame("latitude" = NA, "longitude" = NA, "station" = NA, "date" = NA, "startTime" = NA, "waterDepth" = NA)
  try(meta$latitude <- data@metadata$latitude)
  try(meta$longitude <- data@metadata$longitude)
  try(meta$station <- data@metadata$station)
  try(meta$startTime <- data@metadata$startTime)
  try(meta$recoveryTime <- data@metadata$recoveryTime)
  try(meta$waterDepth <- data@metadata$waterDepth)
  try(meta$date <- data@metadata$date)
  return(meta)
}



# extract the list of names from each Seabird file, flatten that list
# and count the number of occurences of each analyte name.
# across the files in the C drive

# Extract all of the analyte names across all Seabird CTD files
NAMES <- sapply(seaBird, FUN = .getCTDNames)

# Summarize all of the names (remember these are the names transformed by oce package)
table(unlist(NAMES))
# altimeter     beamAttenuation    beamTransmission        conductivity       conductivity2
# 46                 411                 411                 411                  19
# depth                flag        fluorescence       fluorescence2              oxygen
# 411                 411                 411                   8                 411
# oxygen2           oxygenRaw          oxygenRaw2                 par                  pH
# 19                 365                  19                 411                   8
# pressure         pressurePSI pressureTemperature            salinity                spar
# 411                  46                  46                 411                 403
# specificConductance         temperature        temperature2               upoly              upoly2
# 411                 411                  19                 403                  17
# upoly3
# 17

# depth, oxygen, par, spar, temperature, conductivity, specificConductance, pH


# repeat the process for noaa CTD
# get list of noaa CTD filepaths
noaaFiles <- readRDS("../GL_Data/NOAA/ctdFileMetaData.rds") %>%
  mutate(ctdFiles = file.path(fs::path_home(), "Environmental Protection Agency (EPA)",
  "Lake Michigan ML - General", "Raw_data", "NOAA", "CTD 2007-2022", ctdFiles))



# extract all names from NOAA CTD files (used pb apply to get a progress bar (pb) otherwise it was hard to know how long it would take)

NAMES_NOAA <- pbapply::pbsapply(noaaFiles$ctdFiles, FUN = .getCTDNames)
noaaFiles$ctdFiles[1]
.getCTDNames(noaaFiles$ctdFiles[1])

# table(unlist(NAMES_NOAA)
c(unique(unlist(NAMES_NOAA)))
# [1] "depth"
# [2] "temperature"
# [3] "conductivity"
# [4] "fluorescence"
# [5] "par"
# [6] "oxygen"
# [7] "beamTransmission"
# [8] "flag"
# [9] "nbin"
# [10] "salinity"
# [11] "pressure"
# [12] "Error in .nextMethod(x = x, i = i) : \n  the object's data slot lacks \"salinity\", and it cannot be calculated since \"conductivity\" is also missing\n"
# [13] "beamAttenuation"
# [14] "depth2"
# [15] "specificConductance"
# [16] "phycyflTC0"
# [17] "cdomflTC0"
# [18] "v2"
# [19] "v3"
# [20] "Error in read.table(file, skip = iline - 1L, header = FALSE, encoding = encoding) : \n  no lines available in input\n"
# [21] "v0"
# [22] "v1"
# [23] "v4"
# [24] "oxygen2"
# [25] "v5"
# [26] "v7"
# [27] "fluorescence2"

# depth, oxygen, par,  temperature, conductivity (no specificConductance?)
# no pH, no spar


#%% evaluate naming of conductivity and PAR

# ignore: conductivity2 doesn't necessarily mean spec cond just that its the second occurrence of that type of measurement on a given cast
# cond2 <- pbapply::pbsapply(NAMES_NOAA, function(x) "conductivity" %in% x)
# parFiles <- pbapply::pbsapply(NAMES_NOAA, function(x) "par" %in% x)
# cond2Files <- noaaFiles %>% filter(parFiles) %>% pull(ctdFiles)
# v0 - v5, v7 are just voltage






#%% Evaluate CTD META DATA coverage
# Does each ctd have lat/lon date and time available. No but a good portion do
META <- pbapply::pbsapply(noaaFiles$ctdFiles, FUN = .getCTDMeta)
parFiles <- pbapply::pbsapply(META, function(x) "par" %in% x)
test <- bind_rows(META)
