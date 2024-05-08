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


# Convert conductance with the following (suggested by James Gerads)
# [microS/cm]  = (C * 10,000) / (1 + A * [T – 25]) with (C = conductivity (S/m), T = temperature (C), A = thermal coefficient of conductivity
oce2df <- function(data, studyName = NULL, bin = FALSE, downcast = FALSE) {
  # load data as oce object 
  # get Date, Lat, Lon, stationDepth # Station Name
  meta <- data.frame(
    "latitude" = data@metadata$latitude,
    "longitude" = data@metadata$longitude,
    "sampleDate" = data@metadata$date,
    "waterDepth" = data@metadata$waterDepth,
    "station" = data@metadata$station)

# FIXME aaljkldfjaf
  meta$station    <- ifelse(!is.null(data@metadata$station), data@metadata$station, 
    # Parse it from the filename
    #stringr::str_split(tools::file_path_sans_ext(basename(filepath)), pattern = "_", simplify = T)[2]
    "jklj"
  )

   
  data <- data %>%
    {if (downcast) {
      oce::ctdTrim(., method = 'downcast')
    } else . } %>%
    # QC: apply despike over all columns then grab dataframe
    oce::despike(reference = "median") %>%
    # handleFlags is not working
    #oce::handleFlags(object =  , flags = oce::defaultFlags()) %>%
    .@data %>%
    as.data.frame()

  if (("par" %in% names(data)) & ("spar" %in% names(data))) {
    data <- data %>%
      dplyr::mutate(
      # Derivatives
      cpar = par / spar
    )} 

  data <- data %>%
    dplyr::filter(depth > 0.1) %>%
    # Select which sensor for each type of data
    dplyr::select(dplyr::one_of("depth", "temperature", "cpar", "oxygen", "specificConductance")) %>%
    dplyr::mutate(UID = paste0(
      studyName,
      "-", 
      1:nrow(.))
    )
      

  # possible names
  possibleNames <- c("temperature", "spar", "oxygen", "specificConductance", "pH")

  if (bin) {

    # Bin data
    # start at 0.5m so that measures will be at integers matching other 
    temp <- oce::binAverage(y = data$temperature, x = data$depth, xmin = 0.5, xinc = 1)
    df <- data.frame("depth" = temp$x, "temperature" = temp$y)

    for (analyte in possibleNames) {
      {if (analyte %in% names(data)) {
        df <- df %>% 
          dplyr::mutate(
            "{analyte}" := oce::binAverage(y = data[[analyte]], x = data[["depth"]], xmin = 0.5, xinc = 1)$y
          )
      }}}
  }

  df <- df %>% 
    # add meta data
    dplyr::mutate(
      Latitude = meta$latitude,
      Longitude = meta$longitude,
      Station = meta$station,
      sampleDate = meta$sampleDate,
      stationDepth = meta$waterDepth,
      # Make station names similar to how they appear in GLENDA
      Station = stringr::str_remove_all(Station, ","),
      Station = stringr::str_remove_all(Station, " "),
      Station = stringr::str_remove_all(Station, "-"),
      Station = stringr::str_remove_all(Station, "_"),
      Station = toupper(Station)
    ) %>%
    dplyr::rename(sampleDepth = depth) %>%
    tidyr::pivot_longer(-c(sampleDepth, Station, Latitude, Longitude, stationDepth, sampleDate), names_to = "ANALYTE", values_to = "RESULT") %>%
    dplyr::rename(STATION_ID = Station)
  
  unitTable <- data.frame("ANALYTE" = unique(df$ANALYTE))
  unitTable$UNITS <- sapply(unitTable$ANALYTE, function(x) as.character(units[[x]]$unit))
  unitTable <- unitTable %>%
    dplyr::mutate(UNITS = ifelse(grepl("/", UNITS), 
      stringr::str_remove_all(UNITS, pattern = "/"),
      stringr::str_extract(UNITS, pattern = "C$")
    ))

  df <- df %>%
    dplyr::left_join(unitTable, by = "ANALYTE")

  return(df)
}




