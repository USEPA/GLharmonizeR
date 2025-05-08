# FIRST NOAA CTD SCRIPT TO RUN (before ctd-functions.R)
# - This finds out the meta data associated with each CTD cast based on filepath
#   - Infers station name and date

# [x] KV: Need to edit this description if NOAAProcessing.R is now defunct.

# [x] KV: Please add comments at the top of this document describing what this script does and clarifying/confirming the order in which the NOAA processing files are run

# ** Note: KV has not run or carefully checked this code **
library(oce)
library(tidyverse)
devtools::load_all()
source("scripts/ctd-functions.R")
filepaths <- .getFilePaths()
# noaaWQ <- filepaths["noaaWQ"] # KV: Doesn't appear to be used here
noaaWQSites <- filepaths["noaaWQSites"]
# namingFile <- filepaths["namingFile"] # KV: Doesn't appear to be used here and SHOULD NOT be used in any of these processing scripts (per comments in NOAAProcessing.R)
noaaSites <- openxlsx::read.xlsx(filepaths["noaaWQSites"])

# [x] KV: Could edit path here to point to generic home directory as you did elsewhere
# [ ] KV: Not a high priority, but note that the CTD file paths here are just synced files from Sharepoint and could probably be read in directly from Sharepoint, rather than having a user-specific path here.  https://usepa.sharepoint.com/:f:/r/sites/LakeMichiganML/Shared%20Documents/General/Raw_data/NOAA/CTD%202007-2022?csf=1&web=1&e=WmEi8R
# - this might be hard with ctd data because we'd have to call a download function first.
ctdFiles <- list.files(
  path = file.path(normalizePath("~"), "..","..", "..", "Environmental Protection Agency (EPA)", "Lake Michigan ML - General",
   "Raw_data", "NOAA", "CTD 2007-2022"), recursive = T, pattern = "*.cnv$", full.names = F, ignore.case =T) %>%
  .[grepl("^20.*/[[:digit:]*].*", ., ignore.case=T)]
sum(grepl("21700", ctdFiles, ignore.case = T))
ctdFiles[grepl("21700", ctdFiles, ignore.case = T)]




# Missing 16%
parsedSites <- as.data.frame(ctdFiles) %>%
  mutate(
    # clean up site id from filepath
    SITE_ID = tolower(basename(ctdFiles)),
    SITE_ID = stringr::str_remove_all(SITE_ID, ".cnv|.bin"),
    SITE_ID = stringr::str_remove_all(SITE_ID, "\\s*"),
    # these first 5 took care of all names explicitly in WQ data
    SITE_ID1 = stringr::str_extract(SITE_ID,
      # desired station names (30% total)
      "alpha|beta|omega|gh[:digit:]{2,3}|midlk|sbe|c1|d1|e1"
      ),
    # known other names
    SITE_ID2 = stringr::str_extract(SITE_ID,
      # desired station names (78% total)
      "m10|m15|m20|m25|m30|m45|m110|leg_*[:alpha:][:digit:]*"),
      #|stad|leg_c1|leg_d1|leg_e1"
    SITE_ID3 = stringr::str_extract(SITE_ID,
      # desired station names (78% total)
      "station*[abcd]1*|sta_*[abcd]1*|raw0[0-9]*"),
      #|stad|leg_c1|leg_d1|leg_e1"
    SITE_ID4 = stringr::str_extract(SITE_ID,
      # slight alterations (79% total)
      "100mbuoy|ds-7|ds7"
    ),
    SITE_ID5 = stringr::str_extract(SITE_ID,
      # guessing musk Lake (80% total)
      "muskegon|muskla*k|muskmid|midmusk|musk|ml"
      #"muskegon21"
    ),
    SITE_ID6 = stringr::str_extract(SITE_ID,
      # guessing musk Lake (80% total)
      "m*offshor|x2|gvsubuoy|hope[:digit:]*|raw|tb|temp"
      #"muskegon21"
    ),
    siteID = coalesce(SITE_ID1, SITE_ID2, SITE_ID3, SITE_ID4, SITE_ID5, SITE_ID6)
  )

unknownSites <- parsedSites %>% filter(is.na(siteID)) %>% select(ctdFiles, SITE_ID)
parsedSites <- parsedSites %>% drop_na(siteID) %>% select(ctdFiles, siteID)



parsedDates <- parsedSites %>%
  mutate(
    DateArea1 = stringr::str_split_i(ctdFiles, "/", 2),
    DateArea2 = tools::file_path_sans_ext(stringr::str_split_i(ctdFiles, "/", -1)),
    DateArea3 = stringr::str_split_i(ctdFiles, "/", 3),
    DateArea4 = stringr::str_split_i(ctdFiles, "/", 4),
    dplyr::across(dplyr::starts_with("DateArea"),
      function(x) stringr::str_replace_all(tolower(x),
      c("january" = "-01-", "february" = "-02-", "march" = "-03-", "april" = "-04-", "may" = "-05-", "june" = "-06-",
      "july" = "-07-", "august" = "-08-", "september" = "-09-", "october" = "-10-", "november" = "-11-", "december" = "-12-"))
      ),
    dplyr::across(dplyr::starts_with("DateArea"),
      function(x) stringr::str_replace_all(tolower(x), c("jan" = "-01-", "feb" = "-02-", "mar" = "-03-", "apr" = "-04-", "may" = "-05-", "jun" = "-06-", "jul" = "-07-",
      "aug" = "-08-", "sep" = "-09-", "oct" = "-10-", "nov" = "-11-", "dec" = "-12-"))
      ),

    # clean up date areas
    dplyr::across(dplyr::starts_with("DateArea"), function(x) stringr::str_remove_all(x, "[:alpha:]+_+")),
    dplyr::across(dplyr::starts_with("DateArea"), function(x) stringr::str_remove_all(x, "_+[:alpha:]+")),
    dplyr::across(dplyr::starts_with("DateArea"), function(x) stringr::str_remove_all(x, "\\s")),
    dplyr::across(dplyr::starts_with("DateArea"), function(x) stringr::str_remove_all(x, "m[:digit:]+_")),
  )
# Date area 1 has maybe 2 left but tricky because all use underscores

test <- parsedDates %>%
  mutate(
    # doy then y
    Date1.1 = stringr::str_extract(DateArea1, "[:digit:]{3}-[:digit:]{2}"),
    Date1.1 = lubridate::parse_date_time(Date1.1, orders = c("%j-%y")),
    # mdy (careful that it is skipping the doy in front)
    Date1.2 = stringr::str_extract(DateArea1, "[:digit:]{1,2}-[:digit:]{1,2}-[:digit:]{2}"),
    Date1.2 = lubridate::parse_date_time(Date1.2, orders = c("%m-%d-%y")),
    Date2.1 = stringr::str_extract(DateArea2, "[:digit:]{3}-[:digit:]{2}"),
    Date2.1 = lubridate::parse_date_time(Date2.1, orders = c("%j-%y")),
    Date2.2 = stringr::str_extract(DateArea2, "[:digit:]{1,2}-[:digit:]{1,2}-[:digit:]{2}"),
    Date2.2 = lubridate::parse_date_time(Date2.2, orders = c("%m-%d-%y")),
    Date2.3 = stringr::str_extract(DateArea2, "[:digit:]{1,2}_[:digit:]{1,2}_[:digit:]{2}"),
    Date2.3 = lubridate::parse_date_time(Date2.3, orders = c("%m-%d-%y")),
    Date2.4 = stringr::str_extract(DateArea2, "[:digit:]{3}_[:digit:]{2}"),
    Date2.4 = lubridate::parse_date_time(Date2.4, orders = c("%j-%y")),
    Date3.1 = stringr::str_extract(DateArea3, "[:digit:]{3}-[:digit:]{2}"),
    Date3.1 = lubridate::parse_date_time(Date3.1, orders = c("%j-%y")),
    Date3.2 = stringr::str_extract(DateArea3, "[:digit:]{1,2}-[:digit:]{1,2}-[:digit:]{2}"),
    Date3.2 = lubridate::parse_date_time(Date3.2, orders = c("%m-%d-%y")),
    Date3.3 = stringr::str_extract(DateArea3, "[:digit:]{2,3}_[:digit:]{2}"),
    Date3.3 = lubridate::parse_date_time(Date3.3, orders = c("%j_%y")),
    Date4.1 = stringr::str_extract(DateArea4, "[:digit:]{2,3}-[:digit:]{2}"),
    Date4.1 = lubridate::parse_date_time(Date4.1, orders = c("%j-%y")),
    Date4.2 = stringr::str_extract(DateArea4, "[:digit:]{1,2}_[:digit:]{1,2}_[:digit:]{2}"),
    Date4.2 = lubridate::parse_date_time(Date4.2, orders = c("%m_%d_%y")),
    Date4.3 = lubridate::parse_date_time(DateArea4, orders = c("%m-%d-%y")),
    Date4.4 = stringr::str_extract(DateArea4, "[:digit:]{2,3}_[:digit:]{2}"),
    Date4.4 = lubridate::parse_date_time(Date4.4, orders = c("%j_%y")),
    sampleDateTime = dplyr::coalesce(
      Date1.1, Date1.2,
      Date2.1, Date2.2, Date2.3, Date2.4,
      Date3.1, Date3.2, Date3.3,
      Date4.1, Date4.2, Date4.3, Date4.4)
  )

test2 <- test%>%
  filter(is.na(sampleDateTime)) %>%
  mutate(
    across(starts_with("DateArea"), function(x) str_remove_all(x, "[:alpha:]")),
    across(starts_with("DateArea"), function(x) str_remove_all(x, "-$"))
  ) %>%
  unite(newDateArea, DateArea4, DateArea3, sep = "-") %>%
  mutate(
    date1 = lubridate::parse_date_time(newDateArea, orders = c("%m_%d-%y")),
    date2 = lubridate::parse_date_time(newDateArea, orders = c("%m-%d-%y")),
    sampleDateTime = coalesce(date1, date2)
  ) %>%
  select(ctdFiles, siteID, sampleDateTime)

totalParsedDates <- test %>%
  select(ctdFiles, siteID, sampleDateTime) %>%
  # drop because dealt with in test2
  drop_na(sampleDateTime) %>%
  bind_rows(test2) %>%
  left_join(noaaSites, by = c("siteID" = "Other.names")) %>%
  right_join(as.data.frame(ctdFiles), by = "ctdFiles") %>%
  filter(Keep == "T") %>%
  select(-c(Keep, Justification))

saveRDS(totalParsedDates, "../GL_Data/NOAA/ctdFileMetaData.Rds")

totalParsedDates %>%
  filter(is.na(SITE_ID)) %>%
  distinct(siteID)
