# Exploratory script to describe the set of all analyte names in the CTD file (both Seabird and NOAA) as well as 
# the metadata (lat/lon, date, time) in these files.

# [x] KV: The description is still not clear enough at the top of this file. Is this just for getting names for the mapping file? For both Seabird and NOAA? Are these functions ever used anywhere else? Please be explicit because it's hard to follow.

# [x] KV: Please add comments at the top of this document describing what this script does and the order in which the CTD processing files need to be run. For instance, NOAActdNameParsing.R outputs ctdFileMetaData.csv, which is read in below, and would need to be run first.
# [x] KV: Please edit typo in script file name, if it is a typo


# ** Note: KV has not run or carefully checked this code **


library(devtools)
devtools::load_all()
library(tidyverse)
filepaths <- .getFilePaths()
filepaths["seaBird"]

# [x] KV: Are the two functions below used anywhere else in the scripts or package functions or are they just used here? If just used in this sole script, please add a comment explaining that.

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

# [x] KV: Please describe what the code below is doing and why

# [ ] KV: But what is the below code used for? Nothing is written out. Was it just used to manually create the mapping tables in Analytes3??

# extract the list of names from each Seabird file, flatten that list
# and count the number of occurences of each analyte name.
# across the files in the C drive

# Extract all of the analyte names across all Seabird CTD files
NAMES <- sapply(seaBird, FUN = .getCTDNames)

# Summarize all of the names (remember these are the names transformed by oce package)
table(unlist(NAMES))


#%% repeat the process for noaa CTD
# get list of noaa CTD filepaths
noaaFiles <- read_csv("../GL_Data/NOAA/ctdFileMetaData.csv") %>%
  mutate(cnvFiles = file.path("~", "Environmental Protection Agency (EPA)",
  "Lake Michigan ML - General", "Raw_data", "NOAA", "CTD 2007-2022", cnvFiles))

# extract all names from NOAA CTD files (used pb apply to get a progress bar (pb) otherwise it was hard to know how long it would take)

NAMES <- pbapply::pbsapply(noaaFiles$cnvFiles, FUN = .getCTDNames)


#%% evaluate naming of conductivity and PAR

# conductivity2 doesn't necessaily mean spec cond just that its the second occurance of that type of measurement on a given cast
cond2 <- pbapply::pbsapply(NAMES, function(x) "conductivity" %in% x)
parFiles <- pbapply::pbsapply(NAMES, function(x) "par" %in% x)
cond2Files <- noaaFiles %>% filter(parFiles) %>% pull(cnvFiles)
# v0 - v5, v7 are just voltage


unique(unlist(NAMES))

table(unlist(NAMES))



#%% Evaluate CTD META DATA coverage
# Does each ctd have lat/lon date and time available. No but a good portion do
META <- pbapply::pbsapply(noaaFiles$cnvFiles, FUN = .getCTDMeta)
parFiles <- pbapply::pbsapply(META, function(x) "par" %in% x)
test <- bind_rows(META)
