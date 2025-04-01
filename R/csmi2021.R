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

  key <- openxlsx::read.xlsx(namingFile, sheet = "Key") %>%
    dplyr::mutate(Units = tolower(stringr::str_remove(Units, "/"))) %>%
    dplyr::rename(TargetUnits = Units)

  conversions <- openxlsx::read.xlsx(namingFile, sheet = "UnitConversions") %>%
    dplyr::mutate(ConversionFactor = as.numeric(ConversionFactor))%>% 
    dplyr::distinct() # Duplicate rows
  
  renamingTable <- openxlsx::read.xlsx(namingFile, sheet = "CSMI_Map", na.strings = c("", "NA")) %>%
      dplyr::mutate(ANALYTE = stringr::str_remove_all(ANALYTE, "\\."))

  mdls <- file.path(csmi2021, "Chem2021_detection%20limits.xlsx") %>%
    # The detection limit file contains MDLs and the values used to impute results <MDL.
    openxlsx::read.xlsx(sheet = "detection limits", rows = 1:3) %>%
    dplyr::select(15:28) %>%
    dplyr::slice(1) %>%
    tidyr::pivot_longer(dplyr::everything(), values_to = "mdl", names_to = "ANALYTE") %>%
    # KV: Need to extract units from name, not use units from renamingTable! Edited accordingly
    tidyr::separate_wider_delim(ANALYTE, delim = ".", names= c("ANALYTE", "UNITS"), too_many = "merge", too_few = "align_start") %>%
    dplyr::mutate(
      ANALYTE = stringr::str_extract(ANALYTE, "^[:alnum:]*"),
      ANALYTE = ifelse(ANALYTE == "chl", "chla", ANALYTE),
      ) %>%
    dplyr::mutate(Study = "CSMI_2021_WQ") %>%
    # KV: renaming table was not joining correctly because Study was not included and numbers were removed from ANALYTE in renaming Table
    dplyr::left_join(renamingTable) %>% # Note DOC not measured
    dplyr::filter(CodeName != "Remove") %>% # Note also removes NAs - which is DOC
    dplyr::left_join(key) %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    dplyr::mutate(
      ReportedUnits = stringr::str_replace(ReportedUnits, "[.]", " "),
      ReportedUnits = stringr::str_remove(ReportedUnits, "/"),
      ReportedUnits = tolower(ReportedUnits)) %>%
      ReportedUnits = stringr::str_remove(ReportedUnits, "/"),
      ReportedUnits = tolower(ReportedUnits)) %>%
    # KV: conversions was not joining correctly because ReportedUnits hadn't been modified
    dplyr::left_join(conversions) %>%
    dplyr::mutate(mdl = ifelse(!is.na(ConversionFactor), mdl * ConversionFactor, mdl)) %>%
    dplyr::select(ANALYTE, CodeName, TargetUnits, mdl)
    # KV: why would you want to to change TargetUnits to UNITS here? Should join to the table correctly keeping as TargetUnits, and TargetUnits more clearly signifies that the units are converted
    # CC: probably a legacy thing that I didn't get around to changing . I agree with your assessment



  ### CTD ###

  # EPA CTD
  # EPA contact is James Gerads
  ## bin averaged over 1 meter depth intervals. Think bin depth is for 1 m below that depth (e.g., 1 m is 1-2 m)
  ## -9.99E-29 is NA
  epaCTD <- file.path(csmi2021, "2020%20LM%20CSMI%20LEII%20CTD%20combined_Fluoro_LISST_12.13.21.xlsx") %>%
    openxlsx::read.xlsx(
      sheet = "Lake Michigan 2020 CSMI Data", startRow = 2, na.strings = c("", "-9.99e-29"),
      check.names = TRUE
    ) %>%
    dplyr::rename(SITE_ID = X2, sampleDateTimetemp = X3) %>%
    dplyr::mutate(
      sampleDateTimetemp = as.POSIXct(sampleDateTimetemp * 86400, origin = "1899-12-30", tz = "UTC"),
      # KV: corrected origin as in WQ function
      # sampleDateTime = lubridate::ymd_h(paste(sampleDateTime, "12")),
      # [x] KV: Times are in 'Station Reference' tab. Need to add in.
      # [x] KV: Otherwise, need flag for imputing time here and elsewhere
      sampleDate = lubridate::floor_date(sampleDateTimetemp, "days")
      ) %>%
    # assuming only one sample per site per day
    dplyr::left_join(times, by = c("SITE_ID", "sampleDate")) %>%
    # don't select bio samples, scans
    dplyr::select(
      sampleDateTime, SITE_ID:Temperature..deg.C., Oxygen..mg.l.,Specific.Conductance..uS.cm.,
      CPAR.Corrected.Irradiance....,pH:Longitude..deg.) %>%
    dplyr::select(-sampleDateTimetemp) %>%
    dplyr::rename(cpar = CPAR.Corrected.Irradiance....) %>%
    dplyr::mutate(cpar = cpar /100) %>%
    tidyr::pivot_longer(
      cols = Temperature..deg.C.:pH,
      names_to = "ANALYTE",
      values_to = "RESULT"
    ) %>%
    tidyr::separate_wider_regex(ANALYTE, patterns = c("ANALYTE" = ".*", "\\.\\.", "UNITS" = ".*\\..*"), too_few = "align_start") %>%
    dplyr::mutate(
      UNITS = stringr::str_remove_all(UNITS, "[^%|^[:alnum:]]"),
      UNITS = ifelse(ANALYTE=="cpar", "percent", UNITS),
      UNITS = ifelse(ANALYTE=="pH", "unitless", UNITS),
      UNITS = ifelse(UNITS=="degC", "C", UNITS),
      ANALYTE = stringr::str_remove_all(ANALYTE, "\\."),
      Study = "CSMI_2021_CTD"
    ) %>%
    dplyr::rename(sampleDepth = Depth..fresh.water..m., Latitude = Latitude..deg., Longitude = Longitude..deg.) %>%
    dplyr::mutate(SITE_ID = tolower(SITE_ID),
                  SITE_ID = stringr::str_remove_all(SITE_ID, "_")) %>%
    dplyr::mutate(
      UID = paste0("EPActd-", SITE_ID, "-", sampleDepth)
    )
    # [X] KV: Needs UID


  # USGS CTD
  usgsCTDsites <- file.path(csmi2021, "CTD_OP_2021_LM_CSMI.xlsx") %>%
    openxlsx::read.xlsx(
      sheet = "Sheet2",
      check.names = TRUE
    ) %>%
    dplyr::distinct(SITE_ID = TRANSECT, Latitude = BEG_LATITUDE_DD, Longitude = BEG_LONGITUDE_DD, stationDepth = beg_depth) %>%
    dplyr::mutate(SITE_ID = tolower(SITE_ID),
                  SITE_ID = stringr::str_remove_all(SITE_ID, "_"))



  usgsCTD <- file.path(csmi2021, "2021%20July%20Lake%20Michigan%20CSMI%20CTD%20for%20EPA.csv")%>%
    readr::read_csv() %>%
    tidyr::pivot_longer(
      Temp_C:PAR_uEinsteins_m2,
      names_to = c("ANALYTE"),#, "UNITS"),
      # names_pattern = "(^[^_]+)_(.*)$",
      values_to = "RESULT"
    ) %>%
    tidyr::separate_wider_delim(ANALYTE, delim = "_", names= c("ANALYTE", "UNITS"), too_many = "merge", too_few = "align_start") %>%
    dplyr::mutate(
      ANALYTE = paste0(ANALYTE, "_USGS") # Added to avoid problems with EPA and USGS CTD data having same study ID and similar parameter names with different meanings/decisions
    ) %>%
    dplyr::mutate(
      sampleDate = lubridate::dmy(Date),
      UNITS = tolower(stringr::str_remove_all(UNITS, "_")),
      UNITS = ifelse(ANALYTE=="pH_USGS", "unitless", UNITS),
      Study = "CSMI_2021_CTD"
    ) %>%
    tidyr::unite(UID, Transect, Serial, remove = FALSE) %>%
    dplyr::mutate(
      UID = paste0("USGSctd-", UID)
    ) %>%
    dplyr::select(
      SITE_ID = Transect,
      sampleDepth = Depth_m,
      sampleDate,  UID, ANALYTE, UNITS, RESULT, Study # sampleDateTime,
    ) %>%
    dplyr::mutate(SITE_ID = tolower(SITE_ID),
                  SITE_ID = stringr::str_remove_all(SITE_ID, "_"))

    # [X] KV: Add '_USGS' to end of analyte names to deal with similar parameter names between EPA and USGS CTD datasets that have different meanings or decisions.
      # KV: Note that there were several errors for inclusion of the USGS CTD data in the renamingTable due to apparent copy/paste errors and differences between EPA and USGS data sheets. USGS data should've been included separately with different study IDs to account for differences in names across spreadsheets (e.g., for conductivity, temp), but adding '_USGS' to end helps at least.



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
      SITE_ID = Transect
    ) %>%
    dplyr::filter(sampleDepth != 0) %>%
    dplyr::reframe(cpar = mean(cpar, na.rm = T),
                   sampleDateTime = min(sampleDateTime, na.rm = T), # Do min in case a cast overlaps the hour
                   .by = c(SITE_ID, sampleDateTime, sampleDepth)) %>%
    # still no missingness
    dplyr::mutate(
      # for joining to CTD
      sampleDate = lubridate::floor_date(sampleDateTime, "days")
    ) %>%
    dplyr::mutate(SITE_ID = tolower(SITE_ID),
                  SITE_ID = stringr::str_remove_all(SITE_ID, "_")) %>%
    dplyr::mutate(
      Study = "CSMI_2021_CTD",
      UID = paste0("NBpar-", SITE_ID, "-", sampleDepth),
      UNITS = "percent",
      ANALYTE = "cpar_USGS",
      RESULT = cpar,
    ) %>%
    dplyr::select(-c(cpar)) %>%
    filter(SITE_ID %in% usgsCTDsites$SITE_ID)
    # mutate(agency = ifelse(SITE_ID %in% usgsCTDsites$SITE_ID, "USGS", "EPA"))

    # [X] KV: Needs UID - Use NB for Nikki Berry



  # EPA and USGS CTD sites are mutually exclusive
  # sum(epaCTD$SITE_ID %in% usgsCTDsites$SITE_ID)
  # sum(usgsCTDsites$SITE_ID %in% epaCTD$SITE_ID)

  # All Nikki PAR data sites in either USGS or EPA CTD
  # Have CPAR for EPA sites to bottom of lake, so keep that data and remove Nikki PAR data for EPA sites
  # sum(unique(nikkiPAR$SITE_ID) %in% usgsCTDsites$SITE_ID) # 59 USGS sites in Nikki PAR
  # sum(unique(nikkiPAR$SITE_ID) %in% epaCTD$SITE_ID) # 42 EPA sites in Nikki PAR
  # length(unique(nikkiPAR$SITE_ID)) # 101 total Nikki sites


  # Combine usgs CTD and PAR
  usgs <- dplyr::bind_rows(usgsCTD, nikkiPAR) %>%
    dplyr::mutate(
      QAcode = ifelse(is.na(sampleDateTime), "T", NA),
      QAcomment = ifelse(is.na(sampleDateTime), "Time imputed as noon", NA),
      # assume PAR measured at same time as CTD
      sampleDateTime1 = unique(na.omit(sampleDateTime))[1],
      # else fill in with 12 noon
      sampleDateTime2 = lubridate::ymd_hm(paste(sampleDate, "12:00")),
      .by = c(SITE_ID, sampleDate),
    ) %>%
    dplyr::mutate(
      sampleDateTime = dplyr::coalesce(sampleDateTime, sampleDateTime1, sampleDateTime2),
    ) %>%
    dplyr::select(-c(sampleDateTime1, sampleDateTime2, sampleDate)) %>%
    dplyr::mutate(SITE_ID = tolower(SITE_ID),
                  SITE_ID = stringr::str_remove_all(SITE_ID, "_")) %>%
    dplyr::left_join(usgsCTDsites)

    # [ ] KV: Need a flag for imputing noon time above
  
    # [x] KV: Need a flag for imputing noon time above


  ctdDat <- dplyr::bind_rows(epaCTD, usgs)



  # grab additional site data from zooplankton files
  # Note that these don't actually end up providing additional lat/longs for imputation
  zooPlank <- file.path(csmi2021, "LakeMichigan_CSMI_2021_Zooplankton_Taxonomy_Densities.csv") %>%
    readr::read_csv(show_col_types = FALSE) %>%
    dplyr::rename(SITE_ID = TRANSECT) %>%
    dplyr::reframe(
      Latitude2 = mean(Latitude, na.rm = T), Longitude2 = mean(Longitude, na.rm = T),
      .by = SITE_ID
    ) %>%
    dplyr::mutate(SITE_ID = tolower(SITE_ID),
                  SITE_ID = stringr::str_remove_all(SITE_ID, "_"))  # Adding b/c of capitalization


  WQ <- file.path(csmi2021, "Chem2021_FinalShare.xlsx") %>%
    openxlsx::read.xlsx(sheet = "DetLimitCorr") %>%
    dplyr::select(-30) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("L"), ~ as.numeric(.))) %>%
    # KV: Thought this might help with the time as decimal issue, but it did not
    # Add (EST) if not present in Time
    # dplyr::rename(Time.EST=`Time.(EST)`) %>%
    # dplyr::mutate(
    #   Time.EST = case_when(
    #     str_detect(Time.EST, pattern="\\(")==TRUE ~ Time.EST,
    #     str_detect(Time.EST, pattern="\\(")==FALSE ~ paste(Time.EST, " (EST)"))
    # )
    tidyr::separate_wider_regex(`Time.(EST)`, patterns = c("time" = ".*", "tz" = "\\(.*\\)"), too_few = "align_start") %>%
    # [x] parse the time column along with date
    dplyr::mutate(
      Date = as.POSIXct(Date * 86400, origin = "1899-12-30", tz = "UTC"),
      # [x] flag it if we need to assume it's noon
      # [ ] KV: Need to discuss if actually want a formal flag for imputed sample times and also whether it is even necessary
      # [ ] KV: There is also no flag for CSMI 2021 time issues in flagsMap_withDecisions and likely other datasets. This needs to be checked thoroughly for all datasets
      QAcomment = ifelse(grepl("no time", time, ignore.case = T) | is.na(time), "Assumed sample at noon", NA),
      time = ifelse(grepl("no time", time, ignore.case = T) | is.na(time), "12:00", time),
      time = stringr::str_remove_all(time, "[:space:]"),
      # one date is reported funkily so we use the average
      time = ifelse(time == "3:45/4:29", "4:07", time)
    ) %>%
    tidyr::unite("sampleDateTime", Date, time, sep = " ") %>%
    dplyr::mutate(sampleDateTime = lubridate::ymd_hm(sampleDateTime)) %>%

    # [ ] **** KV: The above date and time code no longer works. Simply running the code up to here shows several NAs for sampleDateTime. Time zones are no longer being respected - i.e., you're not dealing with the times that are labeled as CDT (and are assumed EST otherwise).

    dplyr::rename(stationDepth = `Site.Depth.(m)`, sampleDepth = `Separate.depths.(m)`) %>%
    dplyr::select(-c(
      Month, Ship, `Research.Project`, `Integrated.depths.(m)`, `DCL?`, `Stratified/.Unstratified?`,
      Station, tz
    )) %>%
    dplyr::mutate(Study = "CSMI_2021_WQ", UID = paste0(Study, "-", `STIS#`)) %>%
    tidyr::pivot_longer(-c(Study, UID, `STIS#`, Site, sampleDateTime, stationDepth, sampleDepth, QAcomment, Lake), names_to = "ANALYTE", values_to = "RESULT") %>%
    # NA's and non-reports are the only NA's in this dataset
    tidyr::drop_na(RESULT) %>%
    # KV: No, need to extract units from name, not use units from renamingTable! Edited accordingly
    tidyr::separate_wider_delim(ANALYTE, delim = ".", names= c("ANALYTE", "UNITS"), too_many = "merge", too_few = "align_start") %>%
    dplyr::mutate(
      ANALYTE = stringr::str_extract(ANALYTE, "^[:alnum:]*"),
      ANALYTE = ifelse(ANALYTE == "chl", "chla", ANALYTE),
    ) %>%
    dplyr::rename(SITE_ID = Site) %>%
    # Join CTD data
    dplyr::bind_rows(., ctdDat) %>%
    # Deal with capitalization and underscore diffs across datasets
    dplyr::mutate(SITE_ID = tolower(SITE_ID),
                  SITE_ID = stringr::str_remove_all(SITE_ID, "_")) %>%
    # PWA and LVD are typos for PW and LUD, respectively
    # Needed to move this below code up before doing imputation by SITE_ID!!
    dplyr::mutate(
      SITE_ID = stringr::str_replace(SITE_ID, "^pwa", "pw"),
      SITE_ID = stringr::str_replace(SITE_ID, "^lvd", "lud")
    ) %>%
    dplyr::left_join(renamingTable, by = c("Study", "ANALYTE")) %>%
    dplyr::filter(CodeName != "Remove") %>%
    dplyr::mutate(
      Year = 2021,
    ) %>%
    # water chem contains station depth, ctd contains lat lon, so for those sites that
    # have both types of measurement taking the group mean of those values will simply
    # replace the na values
    dplyr::mutate(
      # [x] replace CTD station depth with Wchem station depth by taking gruoped mean
          # NOTE: THIS WAS NOT ACTUALLY DONE BELOW
      # [x] Then do max depth if there are still missing - flag it if this needs to be done
      Latitude = ifelse(is.na(Latitude), mean(Latitude, na.rm = T), Latitude), # fills in some EPA water chem lat/longs but not all
      Longitude = ifelse(is.na(Longitude), mean(Longitude, na.rm = T), Longitude),
      stationDepth = ifelse(is.na(stationDepth), mean(stationDepth, na.rm = T), stationDepth), # This was missing
      QAcomment = ifelse(is.na(stationDepth), paste(QAcomment, "station Depth estimated as the maximum sample Depth"), QAcomment),
      stationDepth = ifelse(is.na(stationDepth), max(sampleDepth, na.rm = T), stationDepth), # Fills in the rest of the EPA CTD max depths - need a flag
      .by = SITE_ID
    ) %>%
    # [ ] KV: Need an actual flag for imputing station depth in flagsMap, not just in QAcomment

    # All stationDepths are filled in at this point, but many CTD stationDepths were filled in by max CTD depth
    # Only missing Lat/Longs are for STO sites (sto5, sto23, sto30)
    # None of these are actually in the zooPlank file, so not needed below
    # Ryan Lepak: RVLG sampled STO5 (Stony lake), STO23 and STO30 yet their shoreline differences were 7, 10.5, 20.5, 27, 34.5 and 43kms from shore. It would take curious rounding if we were to trust these at face value. Or, these coordinates are from another cruise.
    # So something seems off with these sites
    # [ ] KV: Follow up with Ryan to get lat/longs for STO or else remove these sites
    # [ ] KV: Check whether reasonable to impute stationDepth with max CTD depth

    # dplyr::mutate(
    #   ANALYTE = stringr::str_extract(ANALYTE, "^[:alpha:]*") # Probably not necessary
    # ) %>%

    # After adding site info from zooplank, missing lat/lons is 2%
    # KV: Zooplank file doesn't actually fill anything extra in after moving the SITE_ID mods further up in the code so that lat/longs were imputed correctly. But will keep in because it doesn't hurt anything
    dplyr::left_join(zooPlank, by = "SITE_ID") %>%
    dplyr::mutate(
      Longitude = dplyr::coalesce(Longitude, Longitude2),
      Latitude = dplyr::coalesce(Latitude, Latitude2)
    ) %>%
    dplyr::select(-c(Latitude2, Longitude2)) %>%
    dplyr::rename(ReportedUnits = UNITS) %>%
    dplyr::mutate(
      ReportedUnits = stringr::str_replace(ReportedUnits, "[.]", " "),
      ReportedUnits = stringr::str_remove(ReportedUnits, "/"),
      ReportedUnits = tolower(ReportedUnits)) %>%
    # KV: conversions was not joining correctly because ReportedUnits hadn't been modified
    dplyr::left_join(key) %>%
    dplyr::left_join(conversions) %>%
    dplyr::mutate(RESULT = ifelse(!is.na(ConversionFactor), RESULT * ConversionFactor, RESULT)) %>%
    dplyr::left_join(mdls)

  # return the joined data
  # missingness/joining checks in output:
  # mean(is.na(df$CodeName)): 0
  # mean(df$CodeName == "Remove"): 0
  # mean(is.na(df$TargetUnits)): 0
  # df %>% filter(ReportedUnits != TargetUnits) %>% reframe(mean(is.na(ConversionFactor))): 0 cases
  # mean(is.na(df$sampleDateTime))  # 0
  return(WQ)
}


# KV moved below code to clean up function

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
