library(tidyverse)

df <- readCleanGLENDA("Data/Raw/GLENDA/GLENDA.csv")

# List of site IDs that are missing lat/longs
siteMissing <- df %>%
  mutate(
    latlongNotMissing = !(is.na(LATITUDE) & is.na(LONGITUDE)),
    StationdepthNotMissing = !is.na(SAMPLE_DEPTH_M),
    SampledepthNotMissing = !is.na(STN_DEPTH_M),
   .by = STATION_ID
  ) %>%
  reframe(LatLonNonMissing = sum(latlongNotMissing, na.rm = T),
    STNDepthNonmissing = sum(StationdepthNotMissing, na.rm = T),
    SampleDepthNotMissing = sum(SampledepthNotMissing, na.rm = T),
    minYear = min(YEAR, na.rm = T),
    maxYear = max(YEAR, na.rm = T),
    .by = "STATION_ID"
  ) %>%
  filter(
    (LatLonNonMissing ==0) | 
    (STNDepthNonmissing==0) |
    (SampleDepthNotMissing == 0 ) ) %>%
  unite(Years, minYear, maxYear, sep = "-") %>%
  arrange(LatLonNonMissing, STNDepthNonmissing, SampleDepthNotMissing) %>%
  write_csv("missingCoordinates.csv")

