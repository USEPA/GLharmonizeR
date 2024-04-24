
# Check to see if names are same across all seabird files
.getSeaBirdNames <- function(file) {
  oce::read.oce(file) |>
    _@data |>
    names()
}
# NAMES <- apply(files, .getSeaBirdNames)
# table(unlist(NAMES))
# They are the same


seabird2df <- function(filepath) {
  # load data as oce object 
  data <- oce::read.oce(filepath)
  # get Date, Lat, Lon, stationDepth # Station Name
  meta <- as.data.frame(data@metadata[c("latitude", "longitude", "station", "startTime", "waterDepth")])

  data <- data %>%
    # QC: apply despike over all columns then grab dataframe
    oce::despike(reference = "median") |>
    _@data |>
    as.data.frame() %>%
    dplyr::mutate(
      # Derivatives
      CPAR = par / spar
    ) %>%

    dplyr::select(
      depth, temperature, conductivity2, CPAR,  oxygen, flag
    ) %>%
    dplyr::rename(
      sampleDepth = depth,
      DO = oxygen
    )

  # Bin data
  # start at 0.5m so that measures will be at integers matching other 
  temp <- oce::binAverage(y = data$temperature, x = data$sampleDepth, xmin = 0.5, xinc = 1)
  df <- data.frame("sampleDepth" = temp$x, "Temperature" = temp$y) %>%
    dplyr::mutate(
      Conductivity = oce::binAverage(y = data$conductivity2, x = data$sampleDepth, xmin = 0.5, xinc = 1)$y,
      DO = oce::binAverage(y = data$DO, x = data$sampleDepth, xmin = 0.5, xinc = 1)$y
    )
  

  df <- df %>% 
    # add meta data
    dplyr::mutate(
      Latitude = meta$latitude,
      Longitude = meta$longitude,
      Station = meta$station,
      Time = meta$startTime,
      stationDepth = meta$waterDepth,
      sampleDate = lubridate::date(Time)
    )


  return(df)
}