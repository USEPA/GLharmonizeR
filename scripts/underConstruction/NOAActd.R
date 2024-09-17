library(oce)
library(tidyverse)
devtools::load_all()
filepaths <- .getFilePaths()
noaaWQ <- filepaths["noaaWQ"]
namingFile <- filepaths["namingFile"]
NOAA <- noaaReadClean(noaaWQ, namingFile) %>%
  dplyr::mutate(Study = "NOAAwq")
sites <- NOAA %>%
  distinct(SITE_ID)
noaaSites <- openxlsx::read.xlsx(noaaWQ, sheet = "sites") %>%
  dplyr::rename(SITE_ID = `Station:`, Latitude = Lat, Longitude = Long, stationDepth = `Depth.(m)`) %>%
  dplyr::mutate(
    SITE_ID = stringr::str_remove_all(tolower(SITE_ID), "[[:blank:]]")
    ) %>%
  tidyr::separate_longer_delim(Other.names, delim = ",") %>%
  tidyr::separate_longer_delim(Other.names, delim = ";") %>%
  dplyr::mutate(
    Other.names = stringr::str_remove_all(tolower(Other.names), "[[:blank:]]|\\."),
    Longitude = stringr::str_squish(Longitude),
    Longitude = stringr::str_remove(Longitude, "^0"),
  ) %>%
  dplyr::filter(!grepl("somecollections", Other.names)) %>%
  dplyr::filter(!grepl("^08", Other.names)) %>%
  tidyr::separate_wider_delim(Latitude, names=c("LatDeg", "LatDec"), delim = " ") %>%
  tidyr::separate_wider_delim(Longitude, names=c("LonDeg", "LonDec"), delim = " ") %>%
  dplyr::mutate(
    Latitude = as.numeric(LatDeg) + (as.numeric(LatDec) / 60),
    Longitude = as.numeric(LonDeg) + (as.numeric(LonDec) / 60)
  ) %>%
  dplyr::select(-c(LatDeg, LatDec, LonDeg, LonDec))
# [x] How are CDE reported with leg and station?
# All appearance of "Leg" and "Station" appear in the filename (as oppoosed to directory)
#write_csv(noaaSites, "noaaSiteInformation.csv")

ctdDir <- file.path("~", "Environmental Protection Agency (EPA)",
  "Lake Michigan ML - General", "Raw_data", "NOAA", "CTD 2007-2022")
cnvFiles <- c(list.files(path = ctdDir, recursive = T, pattern = "*.cnv$", full.names = F), 
  list.files(path = ctdDir, recursive = T, pattern = "*.CNV$", full.names = T ))
sampleEvents <- cnvFiles[grepl("^20.*/[[:digit:]*].*", cnvFiles, ignore.case=T)]

noaaSites <- read_csv(filepaths["noaaWQSites"])

test <- as.data.frame(sampleEvents) %>%
    fuzzyjoin::fuzzy_join(noaaSites,
    by=c("sampleEvents" = "Other.names"),
    mode='left', #use left join
    match_fun = list(stringr::str_detect)
  )

write_csv(test, "noaaSiteMatching.csv")



# Find unique ways stations are reported in filepaths
legs <- tools::file_path_sans_ext(basename(sampleEvents[grepl("leg", sampleEvents, ignore.case=T)])) %>%
  tolower() %>%
  stringr::str_remove_all(., " ") %>%
  # Leg usually ppears at end of filename
  # otherwise they separate with underscore followed by date
  stringr::str_extract(., "leg.*$") %>%
  # make sure not exclude _ separated site name
  stringr::str_remove(., "_[:digit:].*") %>%
  stringr::str_remove(., "\\_") %>%
  unique(.)

# [ ]  format  in reference table
station <- tools::file_path_sans_ext(basename(sampleEvents))[grepl("sta|station", sampleEvents, ignore.case=T)] %>%
  tools::file_path_sans_ext(.) %>%
  tolower(.) %>%
  stringr::str_extract(., "sta.*$") %>%
  stringr::str_remove(., "_[:digit:].*") %>%
  stringr::str_remove_all(., " ") %>%
  stringr::str_remove_all(., "\\_") %>%
  unique(.)


# [x] Find how many .cnv files

test <- as.data.frame(sampleEvents) %>%
  mutate(
    # clean up site id from filepath
    SITE_ID = tolower(basename(sampleEvents)),
    SITE_ID = stringr::str_remove_all(SITE_ID, ".cnv"),
    SITE_ID = stringr::str_remove_all(SITE_ID, ".bin"),
    SITE_ID = stringr::str_squish(SITE_ID),
    SITE_ID = stringr::str_remove_all(SITE_ID, " "),
    # these first 5 took care of all names explicitly in WQ data
    SITE_ID1 = stringr::str_extract(SITE_ID, 
      # desired station names (30% total)
      "alpha|beta|omega|gh100|gh45|gh15|midlk|c1|d1|e1"
      ),
    # known other names
    SITE_ID2 = stringr::str_extract(SITE_ID, 
      # desired station names (78% total)
      "m15|stad|m45|stab|stationb|m110|muskegon21|staa|station21|100mbuoy|ds-7|legc|legc1|legd|legd1|stad|lege|lege1"
    ),
    SITE_ID3 = stringr::str_extract(SITE_ID, 
      # slight alterations (79% total)
      "sta_d|m45|sta_b|station_b|sta_a|station_21|100mbuoy|ds7|leg_c|leg_c1|leg_d|leg_d1|sta_d|leg_e|leg_e1"
    ),
    SITE_ID4 = stringr::str_extract(SITE_ID, 
      # guessing musk Lake (80% total)
      "musklk|musklake|musk|muskmid|midmusk"
    ),
    SITE_ID5 = stringr::str_extract(SITE_ID, 
      # dangerous Musk Lake guess(80% total)
      "ml"
    ),
    # OTHER PATTERNS NOT INCLUDING FOR NOW
    O_ID1 = stringr::str_extract(SITE_ID, 
      # dangerous Musk Lake guess(80% total)
      "m[:digit:]{2,4}|gh[:digit:]{2,4}X{1,2}"
    ),
    # Other patterns not willing to guess at
    # monthName, tb, x2, raw, numbers
    SITE_ID_COMP = dplyr::coalesce(SITE_ID1, SITE_ID2, SITE_ID3, SITE_ID4, SITE_ID5),
    DateArea1 = stringr::str_split_i(sampleEvents, "/", 2),
    DateArea2 = tools::file_path_sans_ext(stringr::str_split_i(sampleEvents, "/", -1)),
    dplyr::across(dplyr::starts_with("DateArea"), 
      function(x) stringr::str_replace_all(x, c("Jan" = "-01-", "Feb" = "-02-", "Mar" = "-03-", "Apr" = "-04-", "May" = "-05-", "Jun" = "-06-", "Jul" = "-07-", 
      "Aug" = "-08-", "Sep" = "-09-", "Oct" = "-10-", "Nov" = "-11-", "Dec" = "-12-"))
      ),
    DateArea3 = stringr::str_split_i(sampleEvents, "/", 3),
    dplyr::across(dplyr::starts_with("DateArea"), 
      function(x) stringr::str_replace_all(x, c("January" = "-01-", "February" = "-02-", "March" = "-03-", "April" = "-04-", "May" = "-05-", "June" = "-06-", "July" = "-07-", 
      "August" = "-08-", "September" = "-09-", "October" = "-10-", "November" = "-11-", "December" = "-12-"))
      ),
    DateArea1 = stringr::str_remove_all(DateArea1, "[:alpha:]"),
    Date1.1 = stringr::str_extract(DateArea1, "[:digit:]{1,2}-[:digit:]{1,2}-[:digit:]{2,4}"),
    Date1.2 = stringr::str_extract(DateArea1, "[:digit:]{3}-[:digit:]{2}"),
    Date2.1 = stringr::str_extract(DateArea2, "[:digit:]{1,2}-[:digit:]{1,2}-[:digit:]{2,4}"),
    Date3.1 = stringr::str_extract(DateArea3, "[:digit:]{1,2}-[:digit:]{1,2}-{2}"),

    # Convert all dates to datetime
    dplyr::across(dplyr::matches("Date1.1"), function(x) lubridate::parse_date_time(x, orders = c("mdy", "dmy", "ymd", "myd", "ydm", "dym"))),
    dplyr::across(dplyr::matches("Date1.2"), function(x) lubridate::parse_date_time(x, orders = c("jy"))),
    dplyr::across(dplyr::matches("Date2.1"), function(x) lubridate::parse_date_time(x, orders = c("mdy", "dmy", "ymd", "myd", "ydm", "dym"))),
    dplyr::across(dplyr::matches("Date3.1"), function(x) lubridate::parse_date_time(x, orders = c("mdy", "dmy", "ymd", "myd", "ydm", "dym"))),
    # Make sure the datees make sense
    dplyr::across(matches("Date1.1|Date1.2|Date2.1|Date3.1"), 
      function(x) ifelse(x > as.POSIXct("2007-01-01") & x < as.POSIXct("2023-01-01"),
        x, NA
        )),
  )
write_csv(test, "noaaSiteMatching.csv")


# [ ] Join actual file names to naming table
# [ ] parse dates from 2 directories above file
test2<-test %>%
  drop_na(SITE_ID_COMP) %>%
  mutate(
    # replace month names with number
    Date2 = stringr::str_extract(DateArea2, "[:digit:]{1,2}-[:digit:]{1,2}-[:digit:]{2,4}"),
    Date3 = stringr::str_extract(DateArea1, "[:digit:]{6}"),
    date4 = lubridate::parse_date_time(stringr::str_extract(DateArea1, "[:digit:]{6}"), orders = c("mdy", "ymd")),
    date5 = lubridate::parse_date_time(stringr::str_extract(DateArea1, "[:digit:]{5}"), orders = c("mdy", "ymd")),
    dplyr::across(dplyr::matches("Date1|Date2|Date3"), function(x) stringr::str_replace_all(x, "-", "_")),
    # Make them date times 
    dplyr::across(dplyr::matches("Date1|Date2|Date3"), function(x) lubridate::parse_date_time(x, orders = c("dmy", "mdy", "ymd", "myd", "ydm", "dym"))),
    Date = dplyr::coalesce(Date1, Date2, Date3, date4, date5)
  )
  dplyr::filter(
    (Date > as.POSIXct("2007-01-01")) & (Date < as.POSIXct("2023-01-01"))
  )

View(test2)

# PARSE DATES
test2 <- test %>%
  drop_na(SITE_ID_COMP) %>%
  mutate(
    DateArea1Parse1= stringr::str_remove(tools::file_path_sans_ext(DateArea1), "^[:digit:]{3}_"),
    DateArea1Parse1= stringr::str_remove(DateArea1Parse1, "_Laurentian$"),
    A1P1Date1 = stringr::str_extract(DateArea1Parse1, "^[:digit:]{1,2}[-_][:digit:]{1,2}[-_][:digit:]{2,4}"),
    A1P1Date2 = stringr::str_extract(DateArea1Parse1, "[:digit:]{1,2}[-_][:digit:]{1,2}[-_][:digit:]{2,4}$"),
    # looks like this covers date area 1 information
    A2Date1 = stringr::str_extract(DateArea2, "[:digit:]{4}[-_][:digit:]{2}[-_][:digit:]{2}"),
    A2Date2 = stringr::str_extract(DateArea2, "^[:digit:]{1,2}[-_][:digit:]{1,2}[-_][:digit:]{2}"),
    A2Date3 = stringr::str_extract(DateArea2, "[:digit:]{2}_[:digit:]{2}_[:digit:]{4}"),
    A2Date4 = stringr::str_extract(DateArea2, "[:digit:]{6}$"),
    A2Date5 = stringr::str_extract(DateArea2, "^[:digit:]{6}"),
    A2Date6 = stringr::str_extract(DateArea2, "[:digit:]{2}-[:digit:]{2}-[:digit:]{2}"),
    A2Date7 = stringr::str_extract(stringr::str_remove_all(DateArea2, "diel|day|night"), "[:digit:]{6}$"),
    A2Date8 = stringr::str_extract(DateArea2, "[:digit:]{2}[:alpha:]{3}[:digit:]{4}$"),
    A2Date9 = stringr::str_extract(DateArea2, "[:digit:]{3}[_-]([:digit:]{5,8})_[:digit:]{2,4}_[SMOsmo]", group = 1),
    A2Date10 = stringr::str_extract(DateArea2, "[:digit:]{3}-[:digit:]{1,2}[_-]([:digit:]{5,8})_[:digit:]{4}_[0-9]", group = 1),
    A2Date11 = stringr::str_extract(DateArea2, "[:digit:]{2,3}[-_][:digit:]{1,2}[_-]([:digit:]{1,2}[_-][:digit:]{1,2}[_-][:digit:]{2,4})", group = 1),
    A2Date12 = stringr::str_extract(DateArea2, "[:digit:]{2,3}[-_]([:digit:]{1,2}[_-][:digit:]{1,2}[_-][:digit:]{2,4})", group = 1),
    A2Date13 = stringr::str_extract(DateArea2, "M[:digit:]{1,3}[_-]([:digit:]{1,2}[-_][:digit:]{1,2}[-_][:digit:]{2,4})_[:digit:]{4}_[0-9]", group = 1),
    )

test2 %>%
  filter(is.na(A1P1Date1), is.na(A1P1Date2), is.na(A2Date1), is.na(A2Date2), is.na(A2Date3), is.na(A2Date4),
    is.na(A2Date5), is.na(A2Date6), is.na(A2Date7), is.na(A2Date8), is.na(A2Date9), is.na(A2Date10), is.na(A2Date11),
    is.na(A2Date12), is.na(A2Date13)) %>% dim()
  distinct(DateArea2)

test2 %>%
  distinct(A2Date9)

    DateArea2Parse1= stringr::str_remove(DateArea2, "^*day"),
    DateArea2Parse1= stringr::str_remove(DateArea2Parse1, "^*night"),
    DateArea2Parse2= stringr::str_extract(tools::file_path_sans_ext(DateArea2), "[:digit:]{6}$"))

test2 %>% 
  drop_na(testCol) %>%
  distinct(DateArea1Parse1, testCol)
  mutate(test = stringr::str_extract(DateArea1, "^[:digit:]{1,2}[-_][:digit:]{1,2}[-_][:digit:]{2,4}"))


    A1Date1 = lubridate::parse_date_time(DateArea1Parse1, orders = c("dmy", "mdy", "ymd", "myd", "ydm", "dym")),
    A1Date2 = lubridate::parse_date_time(DateArea2, orders = c("dmy", "mdy", "ymd", "myd", "ydm", "dym")),
    A2Date1 = lubridate::parse_date_time(DateArea2Parse2, orders = c("mdy", "myd")),
    A2Date2 = lubridate::parse_date_time(DateArea2Parse2, orders = c("ymd", "ydm")),
    A2Date3 = lubridate::parse_date_time(DateArea2Parse2, orders = c("dmy", "dym")),
  ) %>%
  # [x] Make sure dates are between 2007 and 2022
  mutate() %>%
  # [ ] Monthes are between X and Y
  # [x] Cretae a goodDate output
  mutate(goodDate = coalesce(A1Date1, A1Date2, A2Date1, A2Date2, A2Date3))
  # [ ] If multiple, compress them to comma separated list,
  # [ ] Join with WQ by fuzzy joining if matching site and date is in the comma separated list
test2 %>%
  distinct(DateArea2Parse2, A2Date1, A2Date2, A2Date3)

test2 %>%
  distinct(Date3)

# Parse dates from the string
parsed_dates <- parse_date_time(my_string, orders = c("dmy", "ymd"))
parsed_dates <- strptime(my_string, format= )

# Print the parsed dates
print(parsed_dates)


test %>% 
  filter(is.na(SITE_ID1), is.na(SITE_ID2), is.na(SITE_ID3), is.na(SITE_ID4),
  is.na(SITE_ID5), is.na(SITE_ID6), is.na(SITE_ID7), is.na(SITE_ID8), is.na(SITE_ID9),
  is.na(SITE_ID10)
  ) %>%
  distinct(SITE_ID)

test %>%
  filter(!is.na(SITE_ID1))

test %>%  
  distinct(SITE_ID1, SITE_ID2)


# [ ] Compute coverage by .cnv files
# If good enough
# [ ] join lat lon via station name
purrr::possibly()
readOce <- function(x) oce2df(suppressWarnings(oce::read.oce(x)), studyName = "NOAA", bin = TRUE, downcast = TRUE)
safeReadOce <- purrr::possibly(readOce, quiet = T)


noaaCTD <- cnvFiles[c(1:4, 1740:1746)] %>%
    purrr::map(
      \(x) safeReadOce(x) %>%
        mutate(fileName = basename(x)),
# [ ] parse date and station name
          .progress = TRUE)
    dplyr::bind_rows() %>% 
    dplyr::mutate(Study = "NOAA")
