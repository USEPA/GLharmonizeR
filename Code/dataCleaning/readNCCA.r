
parseNCCAData <- function(directory) {
  data.frame("file" = dir(directory)) %>%
    mutate(year = readr::parse_number(file), 
           chem = grepl(file, pattern = "chem", ignore.case = T),
           site = grepl(file, pattern = "site", ignore.case = T))
    }

readSite <- function(filepath) {
  readr::read_csv(filepath,
    col_types = cols(
      "UID" = "c",
      "SITE_ID" = "c",
      "STATION_DEPTH" = "d",
      "STATION_DEPTH_UNITS" = "c",
      "ALAT_DD" = "d",
      "ALON_DD" = "d" # What are TLAT/TLON? somtimes subbed for alat/lon
    ))
}

readNCCASites <- function(filepaths) {
  file1 <- readSite(filepaths[[1]])
  file2 <- readSite(filepaths[[2]])
  plyr::rbind.fill(file1,file2)
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
              "VISIT_NO" = "-"
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
siteFiles <- c("Data/Raw/NCCA/assessedSiteInfo2010.csv", "Data/Raw/NCCA/nAssessedSiteInfo2010.csv")
preFiles <- c("Data/Raw/NCCA/nca_waterchemdata.csv")
tenFiles<- c("Data/Raw/NCCA/assessedWaterChem2010.csv", "Data/Raw/NCCA/nassessedWaterChem2010.csv") 
fifteenFiles <- c("Data/Raw/NCCA/ncca_2015_water_chemistry_great_lakes-data.csv")
readNCCA <- function(siteFiles, preFiles, tenFiles, fifteenFiles){
  sites <- readNCCASites(siteFiles) %>%
    distinct(SITE_ID, WTBDY_NM, STATION_DEPTH, STATION_DEPTH_UNITS, ALAT_DD, ALON_DD)
  t0 <- readNCCA2000s(preFiles)
  t10 <- readNCCA2010(tenFiles)
  t15 <- readNCCA2015(fifteenFiles)
  plyr::rbind.fill(t0, t10, t15) %>%
    left_join(sites, by = "SITE_ID")
}
