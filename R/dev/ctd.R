# Check to see if names are same across all seabird files
.getSeaBirdNames <- function(file) {
  try(oce::read.oce(file) |>
    _@data |>
    names())
}
getSeaBirdMeta <- function(file) {
  data <- oce::read.oce(file)
  meta <- data.frame("latitude" = NA, "longitude" = NA, "station" = NA, "startTime" = NA, "waterDepth" = NA)
  try(meta$latitude <- data@metadata$latitude)
  try(meta$longitude <- data@metadata$longitude)
  try(meta$station <- data@metadata$station)
  try(meta$startTime <- data@metadata$startTime)
  try(meta$waterDepth <- data@metadata$waterDepth)
  meta
}

NAMES <- sapply(seaBird, FUN = .getSeaBirdNames)
table(unlist(NAMES))
# They are not the same