#' ---
#' title: "GLNPO Data"
#' output:
#'   html_document:
#'     df_print: paged
#' ---

#+ echo=FALSE, message=FALSE, warning=FALSE
library(tidyverse)
library(lubridate)

#+  message=FALSE, warning=FALSE
raw_data_dir <- "C:/Users/KVITENSE/Environmental Protection Agency (EPA)/Lake Michigan ML - General/Raw_data"
dat_source <- "GLNPO"

Chl_all <- read.csv(paste0(raw_data_dir, "/", dat_source, "/Chla_AllYears_2023-04-03.csv")) # 8391
str(Chl_all)
#+ message=FALSE, warning=FALSE
Chl_all$Chla <- as.numeric(Chl_all$VALUE_1)

#' Filter missing/bad values - get rid of 0 values
#+
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
Chl_EST <- Chl %>% filter(TIME_ZONE=="EST") %>% mutate(Sample_Date=ymd_hm(SAMPLING_DATE, tz = "EST"))
Chl_EDT <- Chl %>% filter(TIME_ZONE=="EDT") %>% mutate(Sample_Date=ymd_hm(SAMPLING_DATE, tz = "EST")-hours(1)) # not recognized, use atlantic standard time
Chl_CDT <- Chl %>% filter(TIME_ZONE=="CDT") %>% mutate(Sample_Date=ymd_hm(SAMPLING_DATE, tz = "EST")) # use EST
Chl_GMT <- Chl %>% filter(TIME_ZONE=="GMT") %>% mutate(Sample_Date=ymd_hm(SAMPLING_DATE, tz = "GMT"))

Chl <- rbind(Chl_EST, Chl_EDT, Chl_CDT, Chl_GMT) %>% arrange(Sample_Date)


#' Thermocline is about 10-30 m
#+ echo=FALSE
# unique(Chl$DEPTH_CODE)
#+
thermocline <- Chl %>% filter(DEPTH_CODE=="Thermocline")
range(thermocline$SAMPLE_DEPTH_M)
month(thermocline$Sample_Date)

#+ echo=FALSE, warning=FALSE, fig.width=10
hist(thermocline$SAMPLE_DEPTH_M, breaks=20, main="Depth of thermocline samples", xlab="Depth (m)")




#' Do 10 m? -- NOAA samples will be right in the middle, and generally above thermocline

#' Can include depth of sample as predictor
#+
Chl_10m <- Chl %>% filter(SAMPLE_DEPTH_M<=10) # 1843
Chl_10m$logChla <- log10(Chl_10m$Chla)

#' Sample depths below 10 m
#+
hist(Chl_10m$SAMPLE_DEPTH_M, breaks=20, main="Sample depth")
range(Chl_10m$YEAR)

hist(Chl_10m$Chla, main="Chl-a")
range(Chl_10m$Chla)

hist(Chl_10m$logChla, main="log10(Chl-a)")

# Chl_5m <- Chl %>% filter(SAMPLE_DEPTH_M<=5) # 1516 (1347 < 5m, so 169 at exactly 5 m)

# Remarks <- Chl_10m %>% filter(!(RESULT_REMARK_1=="")) # 104
# unique(Remarks$RESULT_REMARK_1)
# Remarks %>% filter(RESULT_REMARK_1=="Correction Factor, lab")

Chl_10m_QC <- Chl_10m %>% filter(RESULT_REMARK_1=="")


#' Plot all years
#+ echo=FALSE, warning=FALSE, fig.width=10
Chl_10m_QC %>% mutate(Sample_Depth=(-1)*SAMPLE_DEPTH_M) %>% 
  ggplot(aes(x=Sample_Date, y=logChla, col=Sample_Depth)) +
  geom_point() +
  theme_light()

#' Plot year 2000+
#+ echo=FALSE, warning=FALSE, fig.width=10
Chl_10m_QC %>% mutate(Sample_Depth=(-1)*SAMPLE_DEPTH_M) %>% 
  filter(year(Sample_Date)>1999) %>% 
  ggplot(aes(x=Sample_Date, y=logChla, col=Sample_Depth)) +
  geom_point() +
  theme_light()

#' Unique lat-longs -- need Lake MI shape file
#+ echo=FALSE, warning=FALSE, fig.width=10
# names(Chl_10m)
lat_longs <- Chl_10m %>% dplyr::select(LATITUDE, LONGITUDE) %>% unique()
lat_longs %>% ggplot(aes(x=LONGITUDE, y=LATITUDE)) +
  geom_point() +
  theme_light()

# library(knitr); rmarkdown::render("Code/Merge_Data/GLNPO_merge.R")