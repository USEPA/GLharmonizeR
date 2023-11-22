
parseNCCAData <- function(directory) {
  data.frame("file" = dir(directory)) %>%
    dplyr::mutate(year = readr::parse_number(file), 
           chem = grepl(file, pattern = "chem", ignore.case = T),
           site = grepl(file, pattern = "site", ignore.case = T))
}
# pre 2000's doesn't report depth
# 2010 reports depth units, need to check if they're same
# all depths are reported in meters
readSite <- function(filepath) {
  readr::read_csv(filepath) %>%
    dplyr::select(SITE_ID, contains("LAT_DD"), contains("LON_DD"), contains("DEPTH")) %>%
    dplyr::select(-contains("TLAT_DD"), -contains("TLON_DD"), -contains("83"), -contains("UNITS"))  %>%
    dplyr::rename(SITE_ID = 1, LAT = 2, LON = 3) %>%
    dplyr::mutate_at(vars(one_of('STATION_DEPTH')), as.numeric) %>%
    dplyr::rename_with(~ case_when(
      . == "STATION_DEPTH" ~ "DEPTH",
      TRUE ~ .
    )) %>%
    # file 3 has a bunch of empty rows at the end
    # file 2 has missing lat/lons for some reason
    tidyr::drop_na()
}

#dir <- "Data/Raw/NCCA"
readNCCASites <- function(directory) {
  dir(path = directory, pattern = "site", all.files =T, full.names=T, ignore.case = T) %>%
    purrr::map_dfr(readSite)
}

readNCCA2000s <- function(filepath) {
  readr::read_csv(filepath,
                  col_types = cols(
                    .default = "-",
                    "SITE_ID" = "c",
                    "SAMPYEAR" = "d",
                    "Chla ug/L" = "d",
                    "DIN mg N/L" = "d",
                    "DIP mg P/L" = "d",
                    "DO mg/L" = "d",
                    "Light transmittance (%) @ 1 m" = "d",
                    "Chla Cond" = "d",
                    "DIN Cond" = "d",
                    "DIP Cond" = "d",
                    "DO Cond" = "d",
                    "Trans Cond" = "d",
                  )) %>%
    pivot_longer(-c(SITE_ID, SAMPYEAR), names_to = "ANALYTE", values_to = "RESULT")
}

readNCCA2010 <- function(filepaths) {
  filepaths %>%
    map_dfr(read_csv,
            col_types = cols(
              "DATE_COL" = col_date(format = "%m/%d/%Y"),
              "LAB_SAMPLE_ID" = "-",
              "SAMPLE_ID" = "-",
              "STATE" = "-",
              "BATCH_ID" = "-",
              "DATE_ANALYZED" = "-",
              "HOLDING_TIME" = "-",
            )) %>%
    rename(ANL_CODE = PARAMETER, 
           ANALYTE = PARAMETER_NAME,
           Date = DATE_COL)
}

readNCCA2015 <- function(filepath) {
  read_csv(filepath,
           col_types = cols(
             "UID" = "d",
             "SITE_ID" = "c",
             "DATE_COL"= "c",
             "ANALYTE" = "c",
             "LRL" = "d",
             "MDL" = "d",
             "METHOD" = "c",
             "NARS_FLAG" = "c",
             "NARS_COMMENT" = "c",
             "RESULT" = "d",
             "RESULT_UNITS" = "c",
             .default = "-"
           )) %>%
    rename(Date = DATE_COL,
           QACODE = NARS_FLAG,
           QAComment = NARS_COMMENT,
           UNITS = RESULT_UNITS  
          ) %>%
    mutate(Date = dmy(Date)) 
}

# 2020/2021
# SITE_ID, Date_COL,  LAT/LON_DD (not 83), STATION_DEPTH


readNCCA <- function(siteFiles, preFiles, tenFiles, fifteenFiles){
  sites <- readNCCASites(siteFiles) %>%
    distinct(SITE_ID, .keep_all =T) 
  t0 <- readNCCA2000s(preFiles)
  t10 <- readNCCA2010(tenFiles)
  t15 <- readNCCA2015(fifteenFiles)
  dplyr::bind_rows(list(t0, t10, t15)) %>%
    left_join(sites, by = "SITE_ID") %>%
    # QC filters
    filter(! QACODE %in% c("J01", "Q08", "ND", "Q", "H", "L"))
}
