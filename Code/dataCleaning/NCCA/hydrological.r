
# Hydrological data

readNCCASecchi2015 <- function(filepath) {
  read_csv(filepath) %>%
    mutate(
      # Assume if not reported clear to bottom, it is not clear to bottom
      CLEAR_TO_BOTTOM = ifelse(is.na(CLEAR_TO_BOTTOM), "N", CLEAR_TO_BOTTOM),
      # Either it's clear to bottom, or they took measurements, so average them
      # Only mean or both dissappear and reappear exist at a time, so we can include all three in average without biasing
      Secchi = ifelse(CLEAR_TO_BOTTOM == "Y", STATION_DEPTH, rowMeans(select(., MEAN_SECCHI_DEPTH, DISAPPEARS, REAPPEARS), na.rm = TRUE))
      ) %>% 
      # Average over all reps
      reframe(
        SITE_ID = toString(unique(SITE_ID)),
        DATE_COL = toString(unique(DATE_COL)), 
        ANALYTE = "Secchi",
        RESULT = mean(Secchi, na.rm= T),
        STATION_DEPTH_M = mean(STATION_DEPTH, na.rm=T),
        QA_COMMENT = toString(unique(SECCHI_COMMENT)), 
        .by = UID)
}
secchi2015 <- readNCCASecchi2015("https://www.epa.gov/sites/default/files/2021-04/ncca_2015_secchi_great_lakes-data.csv")

readNCCAhydro2010 <- function(filepaths) {
  filepaths %>%
    purrr::map_dfr(read_csv) %>%
    select(UID, SITE_ID, DATE_COL, SDEPTH, PARAMETER_NAME, RESULT, UNITS, QA_CODE, QA_COMMENT) %>%
    rename(
      SAMPLE_DEPTH_M = SDEPTH,
      ANALYTE = PARAMETER_NAME)
}
hydro2010 <- readNCCAhydro2010(c("https://www.epa.gov/sites/default/files/2016-01/assessed_ncca2010_hydrolab.csv", 
"https://www.epa.gov/sites/default/files/2016-01/not_assessed_ncca2010_hydrolab.csv"))


readNCCAhydro2015 <- function(filepath) {
  read_csv(filepath) %>%
    pivot_longer(c(TRANS, CONDUCTIVITY:TEMPERATURE), names_to = "ANALYTE", values_to = "RESULT") %>%
    # combine measurements for similar depths across cast directions rounded to the nearest meter
    mutate(DEPTH = round(DEPTH, 0)) %>%
    # I'm hesitant to use UID instead of DATE and SITE, just because I haven't verified that it is unique 
    reframe(RESULT = mean(RESULT, na.rm = T),
            STATION_DEPTH = mean(STATION_DEPTH, na.rm = T),
            .by = c(UID, DATE_COL, SITE_ID, DEPTH, ANALYTE))
    # I decided not to select or rename columns until we are at the joining step
}
hydro2015 <- readNCCAhydro2015("https://www.epa.gov/sites/default/files/2021-04/ncca_2015_hydrographic_profile_great_lakes-data.csv")





test %>% 
  ggplot(aes(x = RESULT, fill = UNITS, group= UNITS)) +
  geom_histogram() +
  facet_wrap(~ANALYTE, scale = "free")

