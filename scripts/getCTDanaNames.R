# NOT YET REVIEWED

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

NAMES <- sapply(seaBird, FUN = .getCTDNames)
table(unlist(NAMES))
# They are not the same
noaaFiles <- read_csv("../GL_Data/NOAA/ctdFileMetaData.csv") %>%
  mutate(cnvFiles = file.path("~", "Environmental Protection Agency (EPA)",
  "Lake Michigan ML - General", "Raw_data", "NOAA", "CTD 2007-2022", cnvFiles))

NAMES <- pbapply::pbsapply(noaaFiles$cnvFiles, FUN = .getCTDNames)
# [ ] check if specific conductivity (maybe conductivity2 label?)
cond2 <- pbapply::pbsapply(NAMES, function(x) "conductivity" %in% x)
parFiles <- pbapply::pbsapply(NAMES, function(x) "par" %in% x)
cond2Files <- noaaFiles %>% filter(parFiles) %>% pull(cnvFiles)
# v0 - v5, v7 are just voltage
# [ ] Double check for PAR and SPAR


unique(unlist(NAMES))

table(unlist(NAMES))



#%% META DATA
META <- pbapply::pbsapply(noaaFiles$cnvFiles, FUN = .getCTDMeta)
parFiles <- pbapply::pbsapply(META, function(x) "par" %in% x)
test <- bind_rows(META) 