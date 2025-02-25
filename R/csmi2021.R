#' Load and join data for CSMI 2021 from csv excel files
#'
#' @description
#' `.readCleanCSMI2021` returns a dataframe of all of the joined water quality data relating to CSMI 2021
#'
#' @details
#m' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @param csmi2021 a string specifying the directory to CSMI 2021 data
#' @return dataframe of the fully joined water quality data from CSMI 2021
.readCleanCSMI2021 <- function(csmi2021, namingFile) {
  # [ ] try to keep the spatial variability by respecting which datasets to pair together for lat/lng
  # [ ] Check if there are 2 reports of PAR EPA measurements
    # - take the setdifference (removing Nikki's)

  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)
  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))
  
  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "CSMI_Map", na.strings = c("", "NA")) %>%
      dplyr::mutate(ANALYTE = stringr::str_remove_all(ANALYTE, "\\."))
  mdls <- file.path(csmi2021, "Chem2021_detection%20limits.xlsx") %>%
    # The detection limit file contains MDLs and the values used to impute results <MDL.
    openxlsx::read.xlsx(sheet = "detection limits", rows = 1:3) %>%
    dplyr::select(15:28) %>%
    dplyr::slice(1) %>%
    tidyr::pivot_longer(dplyr::everything(), values_to = "mdl", names_to = "ANALYTE") %>%
    dplyr::mutate(
      ANALYTE = stringr::str_extract(ANALYTE, "^[:alnum:]*"),
      ANALYTE = ifelse(ANALYTE == "chl", "Chla", ANALYTE)
      ) %>%
    dplyr::left_join(renamingTable) %>%
    dplyr::left_join(key) %>%
    dplyr::rename(ReportedUnits = Units) %>%
    dplyr::left_join(conversions) %>%
    dplyr::mutate(mdl = ifelse(!is.na(ConversionFactor), mdl * ConversionFactor, mdl)) %>%
    dplyr::select(ANALYTE, UNITS = TargetUnits, mdl) %>%
    dplyr::distinct()
  # CTD
  # \Lake Michigan ML - General\Raw_data\CSMI\2021\2020 LM CSMI LEII CTD combined_Fluoro_LISST_12.13.21.xlsx
  # Contact is James Gerads


  ## bin averaged over 1 meter depth intervals
  ## -9.99E-29 is NA
  ## There are already processed, formatted ready to use files Should we use that?
  epaCTD <- file.path(csmi2021, "2020%20LM%20CSMI%20LEII%20CTD%20combined_Fluoro_LISST_12.13.21.xlsx") %>%
    openxlsx::read.xlsx(
      sheet = "Lake Michigan 2020 CSMI Data", startRow = 2, na.strings = c("", "-9.99e-29"),
      check.names = TRUE
    ) %>%
    dplyr::rename(Site = X2, sampleDateTime = X3) %>%
    dplyr::mutate(
      sampleDateTime = as.POSIXct(sampleDateTime * 86400, origin = "1900-01-01", tz = "UTC"),
      sampleDateTime = lubridate::ymd_h(paste(sampleDateTime, "12")),
      sampleDate = lubridate::floor_date(sampleDateTime, "days")) %>%
    # don't select bio samples, scans
    dplyr::select(2:5, 7,9,14, 21, 22, 23, sampleDate) %>%
    dplyr::select(-sampleDateTime) %>%
    dplyr::rename(cpar = CPAR.Corrected.Irradiance....) %>%
    dplyr::mutate(cpar = cpar /100) %>%
    tidyr::pivot_longer(
      cols = 3:7,
      names_to = "ANALYTE",
      values_to = "RESULT"
    ) %>% 
    # [x] Need to include pH
    # [x] Don't rename, instead think about a regex pivot to also grab the units
    tidyr::separate_wider_regex(ANALYTE, patterns = c("ANALYTE" = ".*", "\\.\\.", "UNITS" = ".*\\..*"), too_few = "align_start") %>%
    dplyr::mutate(
      UNITS = stringr::str_remove_all(UNITS, "[^%|^[:alnum:]]"),
      UNITS = ifelse(grepl("^CPAR", ANALYTE, ignore.case=FALSE), "percent", UNITS), 
      ANALYTE = stringr::str_remove_all(ANALYTE, "\\."),
      sampleDateTime = lubridate::ymd_hm(paste(sampleDate, "12:00")),
      Study = "CSMI_2021_CTD"
    ) %>%
    dplyr::rename(SITE_ID = Site, sampleDepth = Depth..fresh.water..m., Latitude = Latitude..deg., Longitude = Longitude..deg.) %>%
    dplyr::select(-sampleDate)

  usgsCTDsites <- file.path(csmi2021, "CTD_OP_2021_LM_CSMI.xlsx") %>%
    openxlsx::read.xlsx(
      sheet = "Sheet2",
      check.names = TRUE
    ) %>%
    dplyr::distinct(SITE_ID = TRANSECT, Latitude = BEG_LATITUDE_DD, Longitude = BEG_LONGITUDE_DD, siteDepth = beg_depth)


  
  usgsCTD <-file.path(csmi2021, "2021%20July%20Lake%20Michigan%20CSMI%20CTD%20for%20EPA.csv")%>%
    readr::read_csv() %>%
    tidyr::pivot_longer(
      6:13,
      names_to = c("ANALYTE", "UNITS"),
      names_pattern = "(^[^_]+)_(.*)$",
      values_to = "RESULT"
    ) %>%
    dplyr::mutate(
      sampleDate = lubridate::dmy(Date),
      UNITS = tolower(stringr::str_remove_all(UNITS, "_")),
      Study = "CSMI_2021_CTD"
    ) %>%
    tidyr::unite(UID, Transect, Serial, remove = FALSE) %>%
    dplyr::select(
      SITE_ID = Transect,
      sampleDepth = Depth_m,
      sampleDate, UID, ANALYTE, UNITS, RESULT, Study
    )


  # bin starting at 0.5m every 1m so 0.5-1.5 ...
  # contains some usgs and some epa sites
  nikkiPAR <- file.path(csmi2021, "USGS_sites_for_percentPAR-forKelseyV.xlsx") %>%
    openxlsx::read.xlsx(
      sheet = "UpdatedData",
      check.names = TRUE
    ) %>% # no missingness
    dplyr::mutate(
      Date = lubridate::ymd_hms(Date),
      Date = lubridate::floor_date(Date, "days"),
      hours = round(time_converted * 24),
      sampleDateTime = lubridate::ymd_h(paste(Date, hours)),
      cpar = dc.pc.PAR / 100,
      # rebinned with evertyhing +/- 0.5 going to nearest whole number
      sampleDepth = round(as.numeric(Depth_m)),
      #SITE_ID = stringr::str_remove(tolower(Transect), "_")
      SITE_ID = Transect
    ) %>%
    dplyr::filter(sampleDepth != 0) %>%
    dplyr::reframe(cpar = mean(cpar, na.rm = T), .by = c(SITE_ID, sampleDateTime, sampleDepth)) %>%
    # still no missingness
    dplyr::mutate(
      # for joining to CTD
      sampleDate = lubridate::floor_date(sampleDateTime, "days")
    ) %>%
    # [ ] Ask Nikki if we don't expect same sites to be observed in 
    # CTD and PAR samples
    dplyr::mutate(
      Study = "CSMI_2021_CTD",
      UID = paste0(Study, "-", 1:nrow(.)),
      UNITS = "percent",
      ANALYTE = "cpar",
      RESULT = cpar,
    ) %>%
    dplyr::select(-c(cpar, UID)) %>%
    mutate(agency = ifelse(SITE_ID %in% usgsCTDsites$SITE_ID, "USGS", "EPA"))

  # technically this contains usgs PAR + a little bit from EPA sites too 
  usgs <- dplyr::bind_rows(usgsCTD, nikkiPAR) %>%
    dplyr::mutate(
      # assume PAR measured at same time as CTD
      sampleDateTime1 = unique(na.omit(sampleDateTime))[1],
      # else fill in with 12 noon
      sampleDateTime2 = lubridate::ymd_hm(paste(sampleDate, "12:00")),
      .by = c(SITE_ID, sampleDate),
    ) %>%
    dplyr::mutate(
      sampleDateTime = dplyr::coalesce(sampleDateTime, sampleDateTime1, sampleDateTime2),
      # pH is the only analyte that misses the pivot longer pattern
      ANALYTE = ifelse(is.na(ANALYTE), "pH", ANALYTE)
    ) %>%
    dplyr::select(-c(sampleDateTime1, sampleDateTime2, sampleDate)) %>%
    dplyr::left_join(usgsCTDsites)
  
  ctdDat <- dplyr::bind_rows(epaCTD, usgs)

  ########################
  ### NIKKI PAR DATA LOOK#
  ########################
  # selectCTDcasts <- nikkiPAR %>%
  #   filter(sampleDepth !=0, sampleDepth < 31, ANALYTE == "cpar") %>%
  #   mutate(
  #     # find casts with at least one value exceeding 1
  #     Quality = ifelse(sum(RESULT > 1.0) != 0, "Exceeds", "OK"),
  #     .by = c(SITE_ID, sampleDateTime)
  #   )

  # selectCTDcasts %>%
  #   mutate(label = if_else((abs(sampleDepth - 1)) == min(abs(sampleDepth - 1)) & (Quality == "Exceeds"), as.character(interaction(SITE_ID, date(sampleDateTime))), NA_character_), .by = c(SITE_ID, sampleDateTime)) %>%
  #   ggplot(aes(x = sampleDepth, y = RESULT, group = interaction(SITE_ID, sampleDateTime), col = Quality, linetype = agency)) +
  #   geom_line() +
  #   ggrepel::geom_label_repel(aes(label = label)) +
  #   xlim(0,30)

  #############################################
  # Assessing CPAR > 1 ########################
  # p1 <- usgs %>%
  #   filter(ANALYTE == "cpar") %>%
  #   ggplot(aes(x = factor(sampleDepth), y = RESULT)) +
  #   geom_boxplot() +
  #   ggtitle("Nikki CPar distribution vs depth") +
  #   xlab("Sample depth (m)") +
  #   ylab("CPAR measure (proportion)")
  # selectCTDcasts <- epaCTD %>% 
  #   mutate(agency = "EPA") %>%
  #   bind_rows(usgs) %>%
  #   filter(sampleDepth !=0, sampleDepth < 31, ANALYTE == "cpar") %>%
  #   mutate(
  #     # find casts with at least one value exceeding 1
  #     Quality = ifelse(sum(RESULT > 1.0) != 0, "Exceeds", "OK"),
  #     .by = c(SITE_ID, sampleDateTime)
  #   ) %>%
  #   nest_by(agency, Quality, SITE_ID, sampleDateTime) %>%
  #   # grab 3 of each factor with highest recorded CPAR 
  #   mutate(
  #     m = max(data$RESULT, na.rm = T)
  #   ) %>%
  #   arrange(desc(m)) %>%
  #   ungroup() %>%
  #   #slice_head(n=3, by= c(agency, Quality)) %>%
  #   unnest(data)
  
  # selectCTDcasts <- nikkiPAR %>% 
  #   filter(sampleDepth !=0, sampleDepth < 31, ANALYTE == "cpar") %>%
  #   mutate(
  #     # find casts with at least one value exceeding 1
  #     Quality = ifelse(sum(RESULT > 1.0) != 0, "Exceeds", "OK"),
  #     .by = c(SITE_ID, sampleDateTime)
  #   )

  # selectCTDcasts %>%
  #   mutate(label = if_else((abs(sampleDepth - 7.1)) == min(abs(sampleDepth - 7.1)) & (Quality == "Exceeds"), as.character(interaction(SITE_ID, date(sampleDateTime))), NA_character_), .by = c(SITE_ID, sampleDateTime)) %>%
  #   ggplot(aes(x = sampleDepth, y = RESULT, group = interaction(SITE_ID, sampleDateTime), linetype = Quality, col = agency)) +
  #   geom_line() +
  #   ggrepel::geom_label_repel(aes(label = label)) +
  #   xlim(0,30)
  
  # p2 <- usgs %>%
  #   filter(ANALYTE == "cpar") %>%
  #   mutate(over = RESULT > 1) %>%
  #   reframe(.by = sampleDepth, over = sum(over, na.rm = T)) %>%
  #   ggplot(aes(x = factor(sampleDepth), y = over)) +
  #   geom_point() +
  #   ggtitle("Nikki: # over 100%") +
  #   xlab("Sample depth (m)") +
  #   ylab("# CPAR measures > 1.0")
  # p1 / p2

  # usgs %>%
  #   filter(ANALYTE == "cpar", sampleDepth ==1) %>%
  #   reframe(
  #     number1m = n(),
  #     numberGT1.0 = sum(RESULT >1,na.rm = T),
  #     propGT1.0 = numberGT1.0 / number1m
  #     ) %>%
  #     gt::gt()

  #   # CPAR > 100% check
  #   # check if CPAr over 100
  #   library(patchwork)
  #   p1 <- usgsPAR %>% 
  #     reframe(over = mean(RESULT > 1, na.rm =T), .by = c(sampleDepth)) %>%
  #     ggplot(aes(y = over, x= factor(sampleDepth))) +
  #     geom_point() +
  #     ggtitle("Nikki")
  # 
  #   p2 <- epaCTD %>% 
  #     dplyr::rename(depth = Depth..fresh.water..m.) %>%
  #     filter(ANALYTE == "cpar", depth < 31) %>%
  #     reframe(over = mean(RESULT > 1, na.rm =T), .by = c(depth)) %>%
  #     ggplot(aes(y = over, x= factor(depth))) +
  #     geom_point() +
  #     ggtitle("Gerads")
  # 
  #   p1 / p2
  #   ########################
  #   # Check coverage for site ID lat longs
  #   sitelists <- list(
  #     "usgsCTD" = tolower(unique(usgsCTD$Transect)),
  #     #"usgsSits" = tolower(unique(usgsCTDsites$TRANSECT)),
  #     #"Gerard" = str_remove(tolower(unique(CTD$Site)), "_"),
  #     "Zoop" = tolower(unique(zooPlank$SITE_ID)),
  #     # "WQ" = tolower(unique(WQ$Site)),
  #     "USGSPar" = str_remove(tolower(unique(usgsPAR$Site)), "_")
  #     )
  #   ggvenn::ggvenn(sitelists)
  #   #######################
  # 
  # - there isn't any overlaps, so keep both
  #   sitelists <- list(
  #     gerardsiteDates = epaCTD %>% 
  #                 mutate(Site = str_remove(tolower(Site), "_")) %>%
  #                 distinct(Site, sampleDate) %>% 
  #                 arrange(Site, sampleDate) %>%
  #                 unite(temp, Site, sampleDate) %>%
  #                 pull(temp)
  #                 ,
  #     usgsSiteDates = usgsPAR %>%
  #                 mutate( Site = str_remove(tolower(Site), "_")) %>%
  #                 distinct(Site, sampleDate) %>%
  #                 arrange(Site, sampleDate) %>%
  #                 unite(temp, Site, sampleDate) %>%
  #                 pull(temp)
  #     )
  #   ggvenn::ggvenn(sitelists)
  # 
  # intersect(gerardsiteDates$Site, usgsSiteDates$Site)
  # setdiff(gerardsiteDates$Site, usgsSiteDates$Site)
  # setdiff(usgsSiteDates$Site, gerardsiteDates$Site)
  # intersect(gerardsiteDates$sampleDate, usgsSiteDates$sampleDate)
  # setdiff(gerardsiteDates$sampleDate, usgsSiteDates$sampleDate)
  # setdiff(usgsSiteDates$sampleDate, gerardsiteDates$sampleDate)
  # inner_join(gerardsiteDates, usgsSiteDates)
  ######################################################################
  
  # grab additional site data from zooplankton files
  zooPlank <- file.path(csmi2021, "LakeMichigan_CSMI_2021_Zooplankton_Taxonomy_Densities.csv") %>%
    readr::read_csv(show_col_types = FALSE) %>%
    dplyr::rename(SITE_ID = TRANSECT) %>%
    dplyr::reframe(
      Latitude2 = mean(Latitude, na.rm = T), Longitude2 = mean(Longitude, na.rm = T),
      .by = SITE_ID
    ) %>%
    dplyr::mutate(SITE_ID = tolower(SITE_ID))

  WQ <- file.path(csmi2021, "Chem2021_FinalShare.xlsx") %>% 
    openxlsx::read.xlsx(sheet = "DetLimitCorr") %>%
    dplyr::select(-30) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("L"), ~ as.numeric(.))) %>%
    tidyr::separate_wider_regex(`Time.(EST)`, patterns = c("time" = ".*", "tz" = "\\(.*\\)"), too_few = "align_start") %>%
    # [x] parse the time column along with date
    dplyr::mutate(
      Date = as.POSIXct(Date * 86400, origin = "1899-12-30", tz = "UTC"),
      # [x] flag it if we need to assume it's noon
      QAcomment = ifelse(grepl("no time", time, ignore.case = T) | is.na(time), "Assumed sample at noon", NA),
      time = ifelse(grepl("no time", time, ignore.case = T) | is.na(time), "12:00", time),
      time = stringr::str_remove_all(time, "[:space:]"),
      # one date is reported funkily so we report the average
      time = ifelse(time == "3:45/4:29", "4:07", time)
    ) %>%
    tidyr::unite("sampleDateTime", Date, time, sep = " ") %>%
    dplyr::mutate(sampleDateTime = lubridate::ymd_hm(sampleDateTime)) %>%
    dplyr::rename(stationDepth = `Site.Depth.(m)`, sampleDepth = `Separate.depths.(m)`) %>%
    dplyr::select(-c(
      Month, Ship, `Research.Project`, `Integrated.depths.(m)`, `DCL?`, `Stratified/.Unstratified?`,
      Station, tz
    )) %>%
    dplyr::mutate(Study = "CSMI_2021_WQ", UID = paste0(Study, "-", `STIS#`)) %>%
    tidyr::pivot_longer(-c(Study, UID, `STIS#`, Site, sampleDateTime, stationDepth, sampleDepth, QAcomment, Lake), names_to = "ANALYTE", values_to = "RESULT") %>%
    # NA's and non-reports are the only NA's in this dataset
    tidyr::drop_na(RESULT) %>%
    dplyr::mutate(
      ANALYTE = stringr::str_remove_all(ANALYTE, "[-|\\+|=]"),
    ) %>%
    tidyr::separate_wider_regex(ANALYTE, patterns = c("ANALYTE" = "^[:alpha:]*", "\\.", ".g.*L$" ), too_few = "align_start") %>%
    # figured out parsing before joining with CTD is WAAAAAAY easier
    dplyr::rename(SITE_ID = Site) %>%
    dplyr::bind_rows(., ctdDat) %>%
    dplyr::left_join(renamingTable, by = c("Study", "ANALYTE")) %>%
    dplyr::mutate(
      Year = 2021,
    ) %>%
    # water chem contains station depth, ctd contains lat lon, so for those sites that
    # have both types of measurement taking the group mean of those values will simply
    # replace the na values
    dplyr::mutate(
      # [x] repalce CTD station depth with Wchem station depth by taking gruoped mean
      # [x] Then do max depth if there are still missing - flag it if this needs to be done
      Latitude = ifelse(is.na(Latitude), mean(Latitude, na.rm = T), Latitude),
      Longitude = ifelse(is.na(Longitude), mean(Longitude, na.rm = T), Longitude),
      QAcomment = ifelse(is.na(stationDepth), paste(QAcomment, "station Depth estimated as the maximum sample Depth"), QAcomment),
      stationDepth = ifelse(is.na(stationDepth), max(sampleDepth, na.rm = T), stationDepth),
      stationDepth = dplyr::coalesce(stationDepth, siteDepth),
      .by = SITE_ID
    ) %>%
    dplyr::mutate(
      SITE_ID = stringr::str_remove_all(SITE_ID, "_"),
      ANALYTE = stringr::str_extract(ANALYTE, "^[:alpha:]*")
    ) %>%
    dplyr::mutate(
      SITE_ID = tolower(SITE_ID),
      SITE_ID = stringr::str_replace(SITE_ID, "^pwa", "pw"),
      SITE_ID = stringr::str_replace(SITE_ID, "^lvd", "lud")
    ) %>%
    # After adding site info from zooplank, missing lat/lons is 2%
    dplyr::left_join(zooPlank, by = "SITE_ID") %>%
    dplyr::mutate(
      Longitude = dplyr::coalesce(Longitude, Longitude2),
      Latitude = dplyr::coalesce(Latitude, Latitude2)
    ) %>%
    dplyr::select(-c(Latitude2, Longitude2)) %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    dplyr::left_join(key) %>%
    dplyr::left_join(conversions) %>%
    dplyr::mutate(RESULT = ifelse(!is.na(ConversionFactor), RESULT * ConversionFactor, RESULT)) %>%
    dplyr::left_join(mdls) %>%
    dplyr::filter(CodeName != "Remove")

  # return the joined data
  return(WQ)
}


# [x] What % of chem have ctd and vice versa
# NOT a great number. Check out code bleow
# WQ %>%
#  reframe(
#   CTD = sum(Study == "CSMI_2021_CTD"),
#   WQ = sum(Study == "CSMI_2021_WQ"),
#   .by = c(SITE_ID)
#   ) %>%
#   mutate(CTD = CTD > 1, WQ = WQ > 1) %>%
#   count(CTD, WQ)


# Appears there are no chl-a measurements for the Gaurdian data,
# but USGS collected chl-a data at some of the same sites within a week or so. Need to confirm with Ryan/Aabir.
