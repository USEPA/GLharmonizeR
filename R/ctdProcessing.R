# Check to see if names are same across all seabird files
.getSeaBirdNames <- function(file) {
  try(oce::read.oce(file) |>
    _@data |>
    names())
}

.getSeaBirdMeta <- function(file) {
  data <- oce::read.oce(file)
  meta <- data.frame("latitude" = NA, "longitude" = NA, "station" = NA, "startTime" = NA, "waterDepth" = NA)
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
oce2df <- function(data, studyName = NULL, bin = TRUE, downcast = TRUE) {
  # load data as oce object
  # get Date, Lat, Lon, stationDepth # Station Name
  meta <- data.frame(
    "latitude" = data@metadata$latitude,
    "longitude" = data@metadata$longitude,
    "sampleDateTime" = data@metadata$date,
    "waterDepth" = data@metadata$waterDepth,
    "station" = data@metadata$filename
  )
  units <- data@metadata$units


  df <- data %>%
    {
      if (downcast) {
        oce::ctdTrim(., method = "downcast")
      } else {
        .
      }
    } %>%
    # QC: apply despike over all columns then grab dataframe
    oce::despike(reference = "median") %>%
    # handleFlags we need to find their flag reporting scheme
    # oce::initializeFlagScheme(., name = "argo") %>%
    # oce::handleFlags(.) %>%
    .@data %>%
    as.data.frame()

  if (("par" %in% names(df)) & ("spar" %in% names(df))) {
    df <- df %>%
      dplyr::mutate(
        # Derivatives
        cpar = par / spar
      )
  }

  df <- df %>%
    dplyr::filter(depth > 0.1) %>%
    # Select which sensor for each type of data
    dplyr::select(dplyr::one_of("depth", "temperature", "cpar", "oxygen", "specificConductance"))


  # possible names
  possibleNames <- c("cpar", "oxygen", "specificConductance", "pH")

  if (bin) {
    # Bin data
    # start at 0.5m so that measures will be at integers matching other
    temp <- oce::binAverage(y = df$temperature, x = df$depth, xmin = 0.5, xinc = 1)
    temp <- data.frame("depth" = temp$x, "temperature" = temp$y)

    for (analyte in possibleNames) {{ if (analyte %in% names(df)) {
      temp <- temp %>%
        dplyr::mutate(
          "{analyte}" := oce::binAverage(y = df[[analyte]], x = df[["depth"]], xmin = 0.5, xinc = 1)$y
        )
    } }}
  }

  df <- temp %>%
    # add meta data
    dplyr::mutate(
      Latitude = meta$latitude,
      Longitude = meta$longitude,
      Station = meta$station,
      sampleDateTime = meta$sampleDateTime,
      stationDepth = meta$waterDepth,
      # Make station names similar to how they appear in GLENDA
      Station = stringr::str_remove_all(Station, ","),
      Station = stringr::str_remove_all(Station, " "),
      Station = stringr::str_remove_all(Station, "-"),
      Station = stringr::str_remove_all(Station, "_"),
      Station = toupper(Station)
    ) %>%
    dplyr::rename(sampleDepth = depth) %>%
    tidyr::pivot_longer(-c(sampleDepth, Station, Latitude, Longitude, stationDepth, sampleDateTime), names_to = "ANALYTE", values_to = "RESULT") %>%
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
