library(tidyverse)
library(oce)



toOce <- function(row, CTD) {
  data(ctd)
  ctd@data <- CTD$data[[row]]
  ctd@metadata$station <- CTD$Site[[row]]
  ctd@metadata$date <- CTD$sampleDate[[row]]
  ctd@metadata$waterDepth <- CTD$stationDepth[[row]]
  ctd@metadata$latitude <- CTD$Latitude[[row]]
  ctd@metadata$longitude <- CTD$Longitude[[row]]
  return(ctd)
}


oces <- lapply(1:nrow(CTD), toOce, CTD=CTD)
