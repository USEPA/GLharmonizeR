# Harmonize and join NOAA and GLNPO CTD

.cleanGLNPOSeabirdCTD <- function(){
  filepaths <- .getFilePaths()

  # KV: Per comment below, any code in a script that uses the below Analytes3 tables needs to be a core package function that can accommodate changes to these tables
  key <- openxlsx::read.xlsx(filepaths["namingFile"], sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(filepaths["namingFile"], sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor)) %>%
    dplyr::distinct() # Duplicate rows

  seaBirdrenamingTable <- openxlsx::read.xlsx(filepaths["namingFile"], sheet = "SeaBird_Map", na.strings = c("", "NA"))


  glnpo_seabird <- readRDS(url(filepaths["seaBird"])) %>%
    dplyr::mutate(Study = "SeaBird") %>%
    dplyr::mutate(
      STATION_ID = basename(STATION_ID),
      BASE=STATION_ID,
      STATION_ID = stringr::str_remove_all(STATION_ID, stringr::fixed(".cnv", ignore_case = TRUE) ),
      STATION_ID = stringr::str_remove_all(STATION_ID, stringr::fixed(".bin", ignore_case = TRUE)),
      STATION_ID = stringr::str_split_i(STATION_ID, "_", i=2),
      STATION_ID = stringr::str_split_i(STATION_ID, " ", i=1),

      UID = paste0(STATION_ID, "_", lubridate::date(sampleDateTime), "_", sampleDepth),
      UNITS = dplyr::case_when(
        ANALYTE == "cpar" ~ "percent",
        ANALYTE == "pH" ~ "unitless",
        .default = UNITS
      )
    ) %>%
    # Only keep first casts (remove CAST, comma with number, or 'B' if there's also an 'A')
   dplyr::filter(!grepl("CAST", STATION_ID, ignore.case = T))  %>%
   dplyr::filter(!grepl(",", STATION_ID, ignore.case = T))  %>%
    # Note there are some valid sites that end in B that should be kept (above all end in MB and are second casts):
    #  "MI30B" "MI31B" "MI42B"     "MI46B"    "MI48B"     "MI49B"
    # "MI50B"     "MI51B"     "MI52B"     "MI53B"
    dplyr::filter(!STATION_ID %in% c("MI18MB", "MI27M-B", "MI27MB", "MI41M-B", "MI41MB")) %>%
    # Remove A from: "MI18MA", "MI27M-A",  "MI27MA", "MI41M-A" , "MI41MA"
    dplyr::mutate(
      STATION_ID = dplyr::case_when(
        STATION_ID == "MI18MA" ~ "MI18M",
        STATION_ID == "MI27M-A" ~ "MI27M",
        STATION_ID == "MI27MA" ~ "MI27M",
        STATION_ID == "MI41M-A" ~ "MI41M",
        STATION_ID == "MI41MA" ~ "MI41M",
        .default = STATION_ID
      )) %>%
    # What does DERIVED mean in station ame? Unclear but seems to be redundant so removing.
    dplyr::filter(!grepl("DERIVE", STATION_ID, ignore.case = T))  %>%
    # drop NA values
    tidyr::drop_na(RESULT) %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    dplyr::left_join(seaBirdrenamingTable, by = c("Study", "ANALYTE")) %>% # sum(is.na(glnpo_seabird$CodeName)) : 0
    dplyr::filter(!grepl("remove", CodeName, ignore.case = T)) %>%
    dplyr::mutate(
      ReportedUnits = tolower(ReportedUnits),
      ReportedUnits = stringr::str_remove_all(ReportedUnits, "/")
    ) %>%
    dplyr::left_join(key, by = "CodeName") %>% # sum(is.na(glnpo_seabird$TargetUnits)) : 0
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::mutate(RESULT = ifelse(!is.na(ConversionFactor), RESULT*ConversionFactor, RESULT)) %>%
    # Check: if Reported units != Target units, then !is.na(ConversionFactor)
    dplyr::rename(SITE_ID = STATION_ID) %>%

    dplyr::mutate(
      ##### KV: This is not done correctly below and will need to be done instead in the joinFullData.R code so that GLNPO CTD can be filled in from GLENDA station depth
      # Could calculate max depth here for each site and label as such. Then use in the full join function to prioritize which depth to use across both seabird and glenda databases
      # QAcomment = dplyr::case_when(
      #   is.na(stationDepth) & (sum(!is.na(stationDepth)) > 0) ~ "Station depth imputed from another site visit",
      #   is.na(stationDepth) & (sum(!is.na(sampleDepth)) > 0) ~ "Station depth imputed from maximum sample depth",
      #   .default = NA),
      # QAcode = dplyr::case_when(
      #   is.na(stationDepth) & (sum(!is.na(stationDepth)) > 0) ~ "D",
      #   is.na(stationDepth) & (sum(!is.na(sampleDepth)) > 0) ~ "D",
      #   .default = NA),
      # stationDepth = dplyr::case_when(
      #   is.na(stationDepth) & (sum(!is.na(stationDepth)) > 0) ~ mean(stationDepth, na.rm=TRUE),
      #   is.na(stationDepth) & (sum(!is.na(sampleDepth)) > 0) ~ max(sampleDepth, na.rm = TRUE),
      #   .default = stationDepth),
      maxCTDdepth = max(sampleDepth, na.rm = TRUE),
      .by = SITE_ID
    ) %>%
    dplyr::mutate(
      sampleDate = lubridate::date(sampleDateTime),
      sampleTimeUTC = lubridate::hour(sampleDateTime),
      # Note that Seabird is already in UTC
      UID = paste(Study, UID, sep = "_")
    )
  return(glnpo_seabird)
}

# Investigate NA RESULT values (commenting out line that removes NA values above)
# Most are CPAR
# notfinite_glnpo <- glnpo_seabird %>% filter(!is.finite(RESULT))
# table(notfinite_glnpo$CodeName)
# notfinite_glnpo_nocpar <- glnpo_seabird %>% filter(!is.finite(RESULT)) %>% filter(CodeName!="CPAR") # remaining ones are all shallow at 1 or 2 m
# Assume these are bad values and continue to remove


.cleanNOAACTD <- function(){

  filepaths <- .getFilePaths()

  key <- openxlsx::read.xlsx(filepaths["namingFile"], sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(filepaths["namingFile"], sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor)) %>%
    dplyr::distinct() # Duplicate rows

  NOAArenamingTable <- openxlsx::read.xlsx(filepaths["namingFile"], sheet = "NOAA_Map") %>%
    dplyr::filter(Study == "NOAActd") %>%
    dplyr::distinct(Study, ANALYTE, CodeName)


  noaa <- readRDS(url(filepaths["noaaCTD"])) %>%
    # prioritize ctd machine information
    dplyr::mutate(
      Latitude = dplyr::coalesce(Latitude.ctd, Latitude),
      Longitude = dplyr::coalesce(Longitude.ctd, Longitude),
      sampleDateTime = dplyr::coalesce(sampleDateTime.ctd, sampleDateTime),
      # ** Original sampleDateTime is taken from file path and only has date **
      # Where does sampleDateTime.ctd differ from sampleDateTime?
      # time_disagree <- noaa %>% filter(lubridate::date(sampleDateTime.ctd) != sampleDateTime) %>% dplyr::relocate("sampleDateTime.ctd", "sampleDateTime")
      # Looking at disagreements, the CTD date looks to be correct, not the path
      stationDepth = dplyr::coalesce(stationDepth.ctd, stationDepth),
      # Note: There is no stationDepth from CTD for either NOAA or GLNPO Seabird
      # All observations already have stationDepth for NOAA from data provided by Steve Pothoven
      # But doesn't hurt to leave code as is.
      Study = "NOAActd",
    ) %>%
    dplyr::select(-dplyr::ends_with(".ctd")) %>%
    # Note that there are some dates that oce read as 1909 that are actually 2009
    # Replace with correct year
    dplyr::mutate(
      sampleDateTime = dplyr::case_when(
        lubridate::year(sampleDateTime)==1909  ~ sampleDateTime + lubridate::years(100),
        .default = sampleDateTime)
    ) %>%
    dplyr::left_join(NOAArenamingTable, by = c("Study", "ANALYTE")) %>% # sum(is.na(noaa$CodeName))
    dplyr::filter(!grepl("remove", CodeName, ignore.case=  T)) %>%
    dplyr::rename(ReportedUnits = UNITS)  %>%
    dplyr::mutate(ReportedUnits = tolower(ReportedUnits)) %>%
    # [x] KV: Check that ReportedUnits are all formatted correctly for appropriate joining of the conversions table
    dplyr::left_join(key, by =  "CodeName") %>% # sum(is.na(noaa$TargetUnits)) : 0
    dplyr::left_join(conversions, by = c("ReportedUnits", "TargetUnits")) %>%
    dplyr::mutate(
      RESULT = ifelse(!is.na(ConversionFactor), RESULT * ConversionFactor, RESULT)
      ) %>%
    # drop NA values
    tidyr::drop_na(RESULT) %>%
    dplyr::mutate(
      sampleDate = lubridate::date(sampleDateTime),
      sampleTimeUTC = lubridate::hour(sampleDateTime),
    ) %>%
    # Based on maxCTDdepth code below in comparison to stationDepth, found that M45 and M110 were mislabeled/swapped on 2019-12-16. Change Site_ID here
    # sampleDate=="2019-12-16" & grepl("M45", ctdFiles) & SITE_ID == "beta"
    # Change to "omega"
    # And sampleDate=="2019-12-16" & grepl("M110", ctdFiles) & SITE_ID == "omega"
    # Change to "beta"

    ### *** Looks like there are more - see code below function ****
    # ********** There's another that needs fixed!!!!!!! **********

    dplyr::mutate(
      SITE_ID = dplyr::case_when(
        sampleDate=="2019-12-16" & grepl("M45", ctdFiles) & SITE_ID == "beta" ~ "omega",
        sampleDate=="2019-12-16" & grepl("M110", ctdFiles) & SITE_ID == "omega" ~ "beta",
        .default = SITE_ID)
    ) %>%
    # Need to fix also fix stationDepth and lat/long for these two (time stamp should be correct)
    dplyr::mutate(
      stationDepth = dplyr::case_when(
        sampleDate=="2019-12-16" & grepl("M45", ctdFiles) & SITE_ID == "omega" ~ 110,
        sampleDate=="2019-12-16" & grepl("M110", ctdFiles) & SITE_ID == "beta" ~ 45,
        .default = stationDepth),
      Latitude = dplyr::case_when(
        sampleDate=="2019-12-16" & grepl("M45", ctdFiles) & SITE_ID == "omega" ~ 43.19983333,
        sampleDate=="2019-12-16" & grepl("M110", ctdFiles) & SITE_ID == "beta" ~ 43.20616667,
        .default = Latitude),
      Longitude = dplyr::case_when(
        sampleDate=="2019-12-16" & grepl("M45", ctdFiles) & SITE_ID == "omega" ~ -86.56983333,
        sampleDate=="2019-12-16" & grepl("M110", ctdFiles) & SITE_ID == "beta" ~ -86.44966667,
        .default = Longitude)
    ) %>%


    # Imputation for stationDepth isn't needed for NOAA (already have from water chem site data via Steve Pothoven) but computing maxCTD depth by site anyway for comparison
    dplyr::mutate(
      maxCTDdepth = max(sampleDepth, na.rm = TRUE),
      .by = SITE_ID
    ) %>%

    dplyr::mutate(
      UID = paste0(Study, "_", SITE_ID, "_", lubridate::date(sampleDateTime), "_", sampleDepth)
    )

  return(noaa)

}


# There's another one for beta that looks wrong - max depth of 99

depths <- noaa %>% dplyr::select(stationDepth, SITE_ID, maxCTDdepth) %>% unique()
plot(maxCTDdepth~stationDepth, data=depths)
abline(0, 1)

# Looks like one site is ~45 m but has maxCTDdepth of ~100 m
# This is beta  45 beta            104

beta_data <- noaa %>% filter(SITE_ID == "beta")

uniq_date_depth <- beta_data %>% group_by(sampleDate) %>%
  summarize(maxDepthbyDate=max(sampleDepth, na.rm=T)) %>%
  arrange(desc(maxDepthbyDate))
  # dplyr::select(sampleDate, maxCTDdepth) %>% unique()

# 2 2013-10-29             99

# ********** There's another that needs fixed!!!!!!! **********


# The outlier with maxDepth=104 is
# 2019-12-16
# 104
# Must be wrong site
# M45 has 106

beta_bad <- beta_data %>% filter(sampleDate=="2019-12-16") # M45
unique(beta_bad$ctdFiles)

m45_data <- noaa %>% filter(grepl("M45", ctdFiles))

# So sampleDate=="2019-12-16" & grepl("M45", ctdFiles) & SITE_ID == "beta"
# Change to "omega"

# And sampleDate=="2019-12-16" & grepl("M10", ctdFiles) & SITE_ID == "omega"
# Change to "beta"

# m110 (omega) on this date has max depth 42, must have gotten switched

# Look at NA result values
# Most are right at the surface
# Some look like they exist but are dropped, some seem like they don't exist anyway (already binned values and they were removed). For depths that exist but are dropped, seems to be caused by oce::ctdTrim(., method="downcast") in .oce2df() in ctd03_functions.R

# Removing for now but could return to this in the future

# sum(!is.finite(noaa$RESULT))
# notfinite <- noaa %>% filter(!is.finite(RESULT))
# length(unique(notfinite$ctdFiles)) # 804 have missing values out of 1077
# length(unique(noaa$ctdFiles)) # 804 have missing values
#
# table(notfinite$CodeName)
# table(notfinite$sampleDepth)
