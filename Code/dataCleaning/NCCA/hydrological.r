
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
    pivot_longer(-c(1:12,14,15, 24), names_to = "ANALYTE", values_to = "RESULT") %>%
    select(UID, SITE_ID, DATE_COL, STATION_DEPTH, ANALYTE, RESULT) %>%
    rename(SAMPLE_DEPTH_M = STATION_DEPTH)
}
hydro2015 <- readNCCAhydro2015("https://www.epa.gov/sites/default/files/2021-04/ncca_2015_hydrographic_profile_great_lakes-data.csv")



# Drop DATE_COL temporarliy to join and look at data
hydro2010 <- hydro2010 %>%
  select(-DATE_COL)

hydro2015 <- hydro2015 %>%
  select(-DATE_COL)

secchi2015 <- secchi2015 %>%
  select(-DATE_COL)

test <- bind_rows(hydro2010, hydro2015, secchi2015)


test %>% 
  ggplot(aes(x = RESULT, fill = UNITS, group= UNITS)) +
  geom_histogram() +
  facet_wrap(~ANALYTE, scale = "free")

