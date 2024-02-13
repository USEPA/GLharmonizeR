
parseNCCAData <- function(directory) {
  data.frame("file" = dir(directory)) %>%
    dplyr::mutate(
      year = as.integer(readr::parse_number(file)), 
      chem = grepl(file, pattern = "chem", ignore.case = TRUE),
      site = grepl(file, pattern = "site", ignore.case = TRUE),
      meta = grepl(file, pattern = "metadata", ignore.case = TRUE),
      qacodes = grepl(file, pattern = "QA_Codes", ignore.case = TRUE)
    ) %>%
    dplyr::arrange(year)
}

# pre 2000's doesn't report depth
# 2010 reports depth units, need to check if they're same
# all depths are reported in meters
readSite <- function(filepath) {
  templateTable <- dplyr::tibble(
    SITE_ID = character(),
    LATITUDE = numeric(),
    LONGITUDE = numeric(),
    DEPTH = numeric()) %>%
    dplyr::rename(STATION_DEPTH = DEPTH)

  readr::read_csv(filepath, show_col_types=FALSE) %>%
    dplyr::select(SITE_ID, contains("LAT_DD"), contains("LON_DD"), contains("DEPTH")) %>%
    dplyr::select(-contains("TLAT_DD"), -contains("TLON_DD"), -contains("83"), -contains("UNITS"))  %>%
    dplyr::rename(SITE_ID = 1, LATITUDE = 2, LONGITUDE = 3) %>%
    dplyr::rename_with(~ case_when(
      . == "STATION_DEPTH" ~ "DEPTH",
      TRUE ~ .
    )) %>%
    # bind to template table to get all of the columns (even if empty)
    dplyr::bind_rows(templateTable) %>%
    dplyr::mutate_at(vars(one_of('DEPTH')), as.numeric) %>%
    # file 3 has a bunch of empty rows at the end
    # file 2 has missing lat/lons for some reason
    drop_na()
}

readNCCASites <- function(directory) {
  dir(path = directory, pattern = "site.*.csv", all.files =T, full.names=T, ignore.case = T) %>%
    purrr::map_dfr(readSite)
}

readNCCA2000s <- function(filepath) {
  readr::read_csv(filepath,
                  col_types = cols(
                    # Doesn't contain date nor time
                    .default = "-",
                    "SITE_ID" = "c",
                    "SAMPYEAR" = "d",
                    "Chla ug/L" = "d",
                    "DIN mg N/L" = "d",
                    "DIP mg P/L" = "d",
                    "DO mg/L" = "d",
                    "Light transmittance (%) @ 1 m" = "d",
                    "Chla Cond" = "-",
                    "DIN Cond" = "-",
                    "DIP Cond" = "-",
                    "DO Cond" = "-",
                    "Trans Cond" = "-",
                  )) %>%
    pivot_longer(-c(SITE_ID, SAMPYEAR), names_to = "ANALYTE", values_to = "RESULT") 
    # Make a missing columns for depths to align with other data sources
    #mutate(DEPTH = NA)
}

readNCCA2010 <- function(filepaths) {
  filepaths %>%
    map_dfr(read_csv,
            col_types = cols(
              # Doesn't contain time
              "DATE_COL" = col_date(format = "%m/%d/%Y"),
              "LAB_SAMPLE_ID" = "-",
              "SAMPLE_ID" = "-",
              "STATE" = "-",
              "BATCH_ID" = "-",
              "DATE_ANALYZED" = "-",
              "HOLDING_TIME" = "-",
            )) %>%
    rename(
      ANL_CODE = PARAMETER,
      ANALYTE = PARAMETER_NAME,
      Date = DATE_COL)
}

readNCCA2015 <- function(filepath) {
  read_csv(filepath,
           col_types = cols(
             "UID" = "d",
             "SITE_ID" = "c",
             # doesn't contain time
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
           UNITS = RESULT_UNITS,
           ANL_CODE = ANALYTE
          ) %>%
    mutate(Date = dmy(Date)) 
}

# 2020/2021
# SITE_ID, Date_COL,  LAT/LON_DD (not 83), STATION_DEPTH


readNCCA <- function(siteFiles, preFiles=NULL, tenFiles=NULL, fifteenFiles=NULL){
  sites <- readNCCASites(siteFiles) %>%
    distinct(SITE_ID, .keep_all =T) 
  dfs <- list()
  if (!is.null(preFiles)) dfs[[1]] <- readNCCA2000s(preFiles) else print("No early data specified")
  if (!is.null(tenFiles)) dfs[[2]] <- readNCCA2010(tenFiles) else print("2010 files not specified")
  if (!is.null(fifteenFiles)) dfs[[3]] <- readNCCA2015(fifteenFiles) else print("2015 files not specified")
  dplyr::bind_rows(dfs) %>%
    left_join(sites, by = "SITE_ID")
    # QC filters
    #filter(! QACODE %in% c("J01", "Q08", "ND", "Q", "H", "L")) 
}

