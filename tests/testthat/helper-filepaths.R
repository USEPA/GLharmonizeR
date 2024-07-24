namingFile <- "GL_Data-main/Meta/Analytes3.xlsx"
# %% NCCA
# Not using not assessed data after Hugh's suggestion

# Hydrographic
NCCAhydrofiles2010 <- c("https://www.epa.gov/sites/default/files/2016-01/assessed_ncca2010_hydrolab.csv") # ,
NCCAhydrofile2015 <- "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_hydrographic_profile_great_lakes-data.csv"
NCCAsecchifile2015 <- "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_secchi_great_lakes-data.csv"

# Sites
NCCAsites2010 <- "https://www.epa.gov/sites/default/files/2016-06/assessed_ncca2010_siteinfo.revised.06212016.csv"
NCCAsites2015 <- "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_site_information_great_lakes-data.csv"
NCCAsites2022 <- "https://www.epa.gov/system/files/other-files/2024-04/nla22_siteinfo.csv"

# Water quality
NCCAwq2010 <- "https://www.epa.gov/sites/default/files/2016-01/assessed_ncca2010_waterchem.csv"
NCCAwq2015 <- "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_water_chemistry_great_lakes-data.csv"
NCCAqa2010 <- "GL_Data-main/NCCA/ncca_qa_codes.csv"
NCCAwqQA <- "GL_Data-main/NCCA/NCCAQAcounts_withDecision.xlsx"

# %% GLENDA
Glenda <- "GL_Data-main/GLENDA/GLENDA.Rds"
GLENDAlimitsPath <- "GL_Data-main/GLENDA/GLENDAlimits.Rds"
GLENDAsitePath <- "GL_Data-main/GLENDA/GLENDAsiteInfo.Rds"

# SeaBird data  (This is not done yet. Need to process the data and store those smaller files)
seaBird <- "GL_Data-main/GLENDA/seabird.Rds"

# %% CSMI
# csmi2010 <- file.path("2002-2010 Water Quality", "2010")
# csmi2015 <- file.path("2015 CSMI Lake Michigan", "WQ data and database",
#   "CSMI data & database", "CSMI_LkMich2015_Database_working_minsRivMouths.accdb")
# csmi2021 <- file.path(teamsFolder, "Raw_data", "CSMI", "2021")
csmi2015 <- "GL_Data-main/CSMI_2015/CSMI2015_newQuery.zip"
csmi2021 <- "GL_Data-main/CSMI_2020_2021"

# %% NOAA
noaaWQ <- "GL_Data-main/NOAA/NOAA_WQ_2024_04_26.xlsx"
