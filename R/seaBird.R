
# Check to see if names are same across all seabird files
.getSeaBirdNames <- function(file) {
  try(oce::read.oce(file) |>
    _@data |>
    names())
}

.getSeaBirdMeta <- function(file) {
  data <- oce::read.oce(file)
  meta <- data.frame("latitude" = NA, "longitude" = NA, "station" = NA , "startTime" = NA, "waterDepth" = NA)
  try(meta$latitude <- data@metadata$latitude)
  try(meta$longitude <- data@metadata$longitude)
  try(meta$station <- data@metadata$station)
  try(meta$startTime <- data@metadata$startTime)
  try(meta$waterDepth <- data@metadata$waterDepth)
  meta
}
# NAMES <- sapply(seaBirdFiles, FUN = .getSeaBirdNames)
# table(unlist(NAMES))
# # They are not the same



seabird2df <- function(filepath) {
  # load data as oce object 
  data <- suppressWarnings(oce::read.oce(filepath))
  # get Date, Lat, Lon, stationDepth # Station Name
  meta <- data.frame("latitude" = NA, "longitude" = NA, "station" = NA, "startTime" = NA, "waterDepth" = NA)
  meta$latitude   <- data@metadata$latitude
  meta$longitude  <- data@metadata$longitude
  meta$startTime  <- data@metadata$startTime
  meta$waterDepth <- data@metadata$waterDepth

  meta$station    <- ifelse(!is.null(data@metadata$station), data@metadata$station, 
    # Parse it from the filename
    stringr::str_split(tools::file_path_sans_ext(basename(filepath)), pattern = "_", simplify = T)[2]
    ) 

   
  data <- data %>%
    oce::ctdTrim(method = 'downcast') %>% 
    # QC: apply despike over all columns then grab dataframe
    oce::despike(reference = "median") %>% 
    .@data %>%
    as.data.frame() %>%
    {if (("par" %in% names(data)) & ("spar" %in% names(data))) {
      dplyr::mutate(
      # Derivatives
      CPAR = par / spar
      )} else .} %>%
    dplyr::filter(depth > 0.1)

  # possible names
  possibleNames <- c("temperature2", "conductivity", "conductivity2", "conductivity3", "oxygen", "oxygen2")
  
  # Bin data
  # start at 0.5m so that measures will be at integers matching other 
  temp <- oce::binAverage(y = data$temperature, x = data$depth, xmin = 0.5, xinc = 1)
  df <- data.frame("sampleDepth" = temp$x, "Temperature" = temp$y)

  for (analyte in possibleNames) {
    {if (analyte %in% names(data)) {
      df <- df %>% 
        dplyr::mutate(
          "{analyte}" := oce::binAverage(y = data[[analyte]], x = data[["depth"]], xmin = 0.5, xinc = 1)$y
        )
    }}}

  df <- df %>% 
    # add meta data
    dplyr::mutate(
      Latitude = meta$latitude,
      Longitude = meta$longitude,
      Station = meta$station,
      Time = meta$startTime,
      stationDepth = meta$waterDepth,
      sampleDate = lubridate::date(Time),
      # Make station names similar to how they appear in GLENDA
      Station = stringr::str_remove_all(Station, ","),
      Station = stringr::str_remove_all(Station, " "),
      Station = stringr::str_remove_all(Station, "-"),
      Station = stringr::str_remove_all(Station, "_"),
      Station = toupper(Station)
    )


  return(df)
}