#' ---
#' title: "GLNPO Data"
#' output:
#'   html_document:
#'     df_print: paged
#' ---

#+ echo=FALSE, message=FALSE, warning=FALSE
library(tidyverse) # 1.3.2
library(lubridate) # 1.9.2
library(rgdal) # 1.6-6
library(sf) # 1.0-12
library(sp) # 1.4-5
library(RColorBrewer)
# require(devtools)
# install_version("gratia", version = "0.7.2", repost = "http://cran.us.r-project.org")

#+  message=FALSE, warning=FALSE
raw_data_dir <- "C:/Users/KVITENSE/Environmental Protection Agency (EPA)/Lake Michigan ML - General/Raw_data"
dat_source <- "GLNPO"

Chl_all <- read.csv(paste0(raw_data_dir, "/", dat_source, "/Chla_AllYears_2023-04-03.csv")) # 8391
str(Chl_all)
#+ message=FALSE, warning=FALSE
Chl_all$Chla <- as.numeric(Chl_all$VALUE_1)

Chl_all <- Chl_all %>% dplyr::select(-X)

#' Filter missing/bad values
#+
# Get rid of 0 values
Chl_all <- Chl_all %>% filter(!is.na(Chla) & (Chla>0)) %>% filter(!is.na(SAMPLE_DEPTH_M)) # 7827

# Filter QC Type
unique(Chl_all$QC_TYPE)
# Filter to only "routine field sample"
Chl <- Chl_all %>% filter(QC_TYPE=="routine field sample") # 7462

# Filter sample type
unique(Chl$SAMPLE_TYPE)
# filter out "Composite", unsure about "INSITU_MEAS"
Chl <- Chl %>% filter(!SAMPLE_TYPE=="Composite") # 7192
insitu <- Chl %>% filter(SAMPLE_TYPE=="INSITU_MEAS")

# Fix dates - note CDT is same as EST (UTC -5 for both)
Chl_EST <- Chl %>% filter(TIME_ZONE=="EST") %>% 
mutate(Sample_Date=ymd_hm(SAMPLING_DATE, tz = "EST"))
Chl_EDT <- Chl %>% filter(TIME_ZONE=="EDT") %>% 
mutate(Sample_Date=ymd_hm(SAMPLING_DATE, tz = "EST")-hours(1)) # not recognized, use atlantic standard time
Chl_CDT <- Chl %>% filter(TIME_ZONE=="CDT") %>% 
mutate(Sample_Date=ymd_hm(SAMPLING_DATE, tz = "EST")) # use EST
Chl_GMT <- Chl %>% filter(TIME_ZONE=="GMT") %>% 
mutate(Sample_Date=ymd_hm(SAMPLING_DATE, tz = "GMT"))

Chl <- rbind(Chl_EST, Chl_EDT, Chl_CDT, Chl_GMT) %>% arrange(Sample_Date)
rm(Chl_EST, Chl_EDT, Chl_CDT, Chl_GMT)
unique(Chl$UNITS_1) # All same units, ug/L
Chl$logChla <- log10(Chl$Chla)


#' Thermocline samples are about 10-30 m
#+ echo=FALSE
# unique(Chl$DEPTH_CODE)
#+
thermocline <- Chl %>% filter(DEPTH_CODE=="Thermocline")
range(thermocline$SAMPLE_DEPTH_M)
# range(month(thermocline$Sample_Date))
# range(month(Chl$Sample_Date))


#+ echo=FALSE, warning=FALSE, fig.width=10
hist(thermocline$SAMPLE_DEPTH_M, breaks=20, main="Depth of thermocline samples", xlab="Depth (m)")




#' Use depths <=10 m? -- NOAA samples will be right in the middle, and generally above thermocline. Can include depth of sample as predictor.
#+
Chl_10m <- Chl %>% filter(SAMPLE_DEPTH_M<=10) # 1843
insitu10 <- Chl_10m %>% filter(SAMPLE_TYPE=="INSITU_MEAS")

#' Distribution of depths below 10 m
#+
hist(Chl_10m$SAMPLE_DEPTH_M, breaks=20, main="Sample depth")
range(Chl_10m$YEAR)

hist(Chl_10m$Chla, main="Chl-a")
range(Chl_10m$Chla)

hist(Chl_10m$logChla, main="log10(Chl-a)")

#+ echo=FALSE
# Chl_5m <- Chl %>% filter(SAMPLE_DEPTH_M<=5) # 1516 (1347 < 5m, so 169 at exactly 5 m)
# Remarks <- Chl_10m %>% filter(!(RESULT_REMARK_1=="")) # 104
# unique(Remarks$RESULT_REMARK_1)
# Remarks %>% filter(RESULT_REMARK_1=="Correction Factor, lab")

#' Remove observations with any flags
#+
Chl_10m_QC <- Chl_10m %>% filter(RESULT_REMARK_1=="")


#' Plot Chla over time
#+ echo=FALSE, warning=FALSE, fig.width=10
Chl_10m_QC %>% mutate(Sample_Depth=(-1)*SAMPLE_DEPTH_M) %>% 
  ggplot(aes(x=Sample_Date, y=logChla, col=Sample_Depth)) +
  geom_point() +
  theme_light()

#' Plot Chla from year 1995+
#+ echo=FALSE, warning=FALSE, fig.width=10
Chl_10m_QC %>% mutate(Sample_Depth=(-1)*SAMPLE_DEPTH_M) %>% 
  filter(year(Sample_Date)>1995) %>% 
  ggplot(aes(x=Sample_Date, y=logChla, col=Sample_Depth)) +
  geom_point() +
  theme_light()

#' Look at sampling locations spatially
#+ warning=FALSE, message=FALSE
MI.shape <- readOGR(dsn=paste0(raw_data_dir, "/Spatial/MI_shoreline"), layer="MI_shoreline")
MI.shape.latlong <- spTransform(MI.shape, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
MI.shape_sf <- st_as_sf(MI.shape.latlong)
MI.shape.agg <- MI.shape_sf %>% summarise(geometry = sf::st_union(geometry)) 

#' Note that only observations from 1996-2022 have lat-longs. Data from 1983-1993 have sites IDs, so could potentially track down this information. From above figures, there seems to be a break point in the data at 1996, possibly because of different sampling locations/approaches? Additionally, zoop data are only available starting in 1997, so **making executive decision to remove data <1996.**
#+
# Chla data with vs. without lat-longs
Chl_10m_coords <- Chl_10m_QC %>% filter(!is.na(LATITUDE))  # 895, 1996-2022
Chl_10m_nocoords <- Chl_10m_QC %>% filter(is.na(LATITUDE)) # 844, 1983-1993
range(Chl_10m_nocoords$YEAR) # 1983-1993, no coordinates
range(Chl_10m_coords$YEAR)


#' Plot 1996-2022 Chla spatially
#+ fig.width=10
Chl_10m_sf <- st_as_sf(Chl_10m_coords, coords=c("LONGITUDE", "LATITUDE"), crs=4326, agr="constant")

cols <- brewer.pal(9, "YlGn")[3:9]
pal <- colorRampPalette(cols)

# ggplot() + geom_sf(data=MI.shape.agg, colour="black", fill="gray70", lwd=.5) +
#   geom_sf(data=Chl_10m_sf, aes(col=Chla)) + 
#   scale_color_gradientn(colours=pal(100))+
#   theme_light()

#' Jitter points
#+ fig.width=10
ggplot() + geom_sf(data=MI.shape.agg, colour="black", fill="gray70", lwd=.5) +
  geom_sf(data=st_jitter(Chl_10m_sf, factor = 0.02), aes(col=Chla)) + 
  scale_color_gradientn(colours=pal(100))+
  theme_light()




####### Add TP data #############
TP_all <- read.csv(paste0(raw_data_dir, "/", dat_source, "/TP_AllYears_2023-04-03.csv")) # 8391
str(TP_all)
TP_all <- TP_all %>% dplyr::select(-X)

#+ message=FALSE, warning=FALSE
#+ 
# Filtrate TP = total dissolved P, total/bulk TP = total phosphorus
unique(TP_all$FRACTION_1)
unique(TP_all$FRACTION_2)

TP_F1_Filt <- TP_all %>% filter(FRACTION_1=="Filtrate")   # These samples also have total in Value_2
TP_F1_Total <- TP_all %>% filter(FRACTION_1=="Total/Bulk") # These samples only have total in VALUE_1, 1994-1995
unique(TP_F1_Filt$FRACTION_2)
unique(TP_F1_Total$FRACTION_2)

sort(unique(TP_F1_Filt$YEAR)) # 1983-1993, 1996-2019
sort(unique(TP_F1_Total$YEAR)) # 1994, 1995 -- removing these anyway

# Filter to 1996+
TP_96 <- TP_all %>% filter(YEAR>1995)

# Filter QC Type
unique(TP_96$QC_TYPE)
# Filter to only "routine field sample"
TP_96 <- TP_96 %>% filter(QC_TYPE=="routine field sample") # 4023

# Filter sample type
unique(TP_96$SAMPLE_TYPE)
# filter out "Composite", unsure about "INSITU_MEAS"
TP_96 <- TP_96 %>% filter(!SAMPLE_TYPE=="Composite") # 3654

# Filter depths
TP_96 <- TP_96 %>% filter(SAMPLE_DEPTH_M<=10) # 946

unique(TP_96$FRACTION_1)
unique(TP_96$FRACTION_2)

TP_96$TDP_ugL <- as.numeric(TP_96$VALUE_1)
TP_96$TP_ugL <- as.numeric(TP_96$VALUE_2)

# Note that mg/L units are wrong - all ug/L
# unique(TP_96$UNITS_2)
# TP_96 %>% filter(UNITS_2=="mg/l") %>% ggplot(aes(x=TP_ugL))+geom_histogram() # No way these are mg/l? Just 2009 and 2012 - checked canned files to confirm
# TP_96 %>% filter(UNITS_2=="ug/l") %>% ggplot(aes(x=TP_ugL))+geom_histogram()


# Fix dates - note CDT is same as EST (UTC -5 for both)
unique(TP_96$TIME_ZONE)
TP_EST <- TP_96 %>% filter(TIME_ZONE=="EST") %>% mutate(Sample_Date=ymd_hm(SAMPLING_DATE, tz = "EST"))
TP_EDT <- TP_96 %>% filter(TIME_ZONE=="EDT") %>% mutate(Sample_Date=ymd_hm(SAMPLING_DATE, tz = "EST")-hours(1)) # not recognized, use atlantic standard time
TP_CDT <- TP_96 %>% filter(TIME_ZONE=="CDT") %>% mutate(Sample_Date=ymd_hm(SAMPLING_DATE, tz = "EST")) # use EST
TP_GMT <- TP_96 %>% filter(TIME_ZONE=="GMT") %>% mutate(Sample_Date=ymd_hm(SAMPLING_DATE, tz = "GMT"))

TP_96 <- rbind(TP_EST, TP_EDT, TP_CDT, TP_GMT) %>% arrange(Sample_Date)

TP_96 %>% filter(is.na(TDP_ugL) ) # No NAs
TP_96 %>% filter((TDP_ugL==0)) # No zeros
TP_96 %>%  filter(is.na(SAMPLE_DEPTH_M)) # no missing depths

# Add NAs for observations with any remarks
TP_96$TDP_ugL[!(TP_96$RESULT_REMARK_1=="")] <- NA
TP_96$TP_ugL[!(TP_96$RESULT_REMARK_2=="")] <- NA

# TDP_flag <- TP_96 %>% filter(!RESULT_REMARK_1=="") # 114
# TP_flag <- TP_96 %>% filter(!RESULT_REMARK_2=="")  # 28


# Join to Chl
names(Chl_10m_sf)
names(TP_96)
Chl_sel <- Chl_10m_sf %>% dplyr::select(-Row, -SAMPLING_DATE, -DEPTH_CODE, -MEDIUM, -QC_TYPE, -ANL_CODE_1, -ANALYTE_1, -VALUE_1, -UNITS_1, -FRACTION_1, -METHOD_1, -RESULT_REMARK_1) # 895

names(Chl_sel)[names(Chl_sel) %in% names(TP_96)]
TP_sel <- TP_96 %>% dplyr::select(names(Chl_sel)[names(Chl_sel) %in% names(TP_96)], TP_ugL, TDP_ugL) # 946

dat_join <- left_join(Chl_sel, TP_sel)

sum(is.na(dat_join$Chla))
sum(is.na(dat_join$TP_ugL)) # 83 = 55 recent years missing + 28 orig NAs; get 55 if don't replace flagged obs with NAs
sum(is.na(dat_join$TDP_ugL)) # 169 = 55 recent years missing + 114 orig NAs; get 55 if don't replace flagged obs with NAs

# Could join just on sample ID???
length(unique(dat_join$SAMPLE_ID)) # 895 unique IDs
length(unique(TP_96$SAMPLE_ID)) # 946
dat_join %>% filter(!SAMPLE_ID %in% TP_sel$SAMPLE_ID) # 55 don't match -- Just 2021-2022

dat_join$logTP <- log10(dat_join$TP_ugL)
dat_join$logTDP <- log10(dat_join$TDP_ugL)

names(dat_join)

dat_join %>% ggplot(aes(x=TP_ugL, y=Chla)) +
  geom_point()

dat_join %>% ggplot(aes(x=logTP, y=logChla, col=SEASON)) +
  geom_point()

dat_join %>% ggplot(aes(x=logTP, y=logChla, col=-1*SAMPLE_DEPTH_M)) +
  geom_point()

dat_join %>% ggplot(aes(x=logTP, y=logChla, col=-1*STN_DEPTH_M)) +
  geom_point()


#' Jitter points
#+ fig.width=10
cols2 <- brewer.pal(9, "Reds")[3:9]
pal2 <- colorRampPalette(cols2)
ggplot() + geom_sf(data=MI.shape.agg, colour="black", fill="gray70", lwd=.5) +
  geom_sf(data=st_jitter(dat_join, factor = 0.02), aes(col=TP_ugL)) + 
  scale_color_gradientn(colours=pal2(100), na.value="transparent")+
  theme_light()
  

# What are the gray points

#+ echo-FALSE
# library(knitr); rmarkdown::render("Code/Merge_Data/GLNPO_merge.R")