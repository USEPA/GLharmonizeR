# Creates a complete list of the analyte names contained within the CTD data files
# [x] KV: Please add comments at the top of this document describing what this script does and the order in which the CTD processing files need to be run. For instance, NOAActdNameParsing.R outputs ctdFileMetaData.csv, which is read in below, and would need to be run first.
# [x] KV: Please edit typo in script file name, if it is a typo

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

# extract the list of names from each Seabird file, flatten that list
# and count the number of occurences of each analyte name.
# across the files in the C drive
NAMES <- sapply(seaBird, FUN = .getCTDNames)
table(unlist(NAMES))
# They are not the same
noaaFiles <- read_csv("../GL_Data/NOAA/ctdFileMetaData.csv") %>%
  mutate(cnvFiles = file.path("~", "Environmental Protection Agency (EPA)",
  "Lake Michigan ML - General", "Raw_data", "NOAA", "CTD 2007-2022", cnvFiles))

NAMES <- pbapply::pbsapply(noaaFiles$cnvFiles, FUN = .getCTDNames)
# conductivity2 doesn't necessaily mean spec cond just that its the second occurance of that type of measurement on a given cast
cond2 <- pbapply::pbsapply(NAMES, function(x) "conductivity" %in% x)
parFiles <- pbapply::pbsapply(NAMES, function(x) "par" %in% x)
cond2Files <- noaaFiles %>% filter(parFiles) %>% pull(cnvFiles)
# v0 - v5, v7 are just voltage


unique(unlist(NAMES))

table(unlist(NAMES))



#%% META DATA
META <- pbapply::pbsapply(noaaFiles$cnvFiles, FUN = .getCTDMeta)
parFiles <- pbapply::pbsapply(META, function(x) "par" %in% x)
test <- bind_rows(META) 