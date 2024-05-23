# File paths
# NCCA
#NCCAhydrofiles2010 = c("https://www.epa.gov/sites/default/files/2016-01/assessed_ncca2010_hydrolab.csv"# , 
# Not using not assessed data after Hugh's suggestion
#"https://www.epa.gov/sites/default/files/2016-01/not_assessed_ncca2010_hydrolab.csv"
#)
teamsFolder <- file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)",
 "Lake Michigan ML - General")
#NCCAhydrofile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_hydrographic_profile_great_lakes-data.csv"
#NCCAsecchifile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_secchi_great_lakes-data.csv" 
NCCAhydrofiles2010 <- c(file.path("Data", "NCCA", "assessed_ncca2010_hydrolab.csv"))
NCCAhydrofile2015  <- file.path("Data", "NCCA", "ncca_2015_hydrographic_profile_great_lakes-data.csv")
NCCAsecchifile2015 <- file.path("Data", "NCCA", "ncca_2015_secchi_great_lakes-data.csv")

#NCCA WQ files
NCCAsites2010 <- file.path("Data", "NCCA", "assessed_ncca2010_siteinfo.revised.06212016.csv")
NCCAsites2015 <- file.path("Data", "NCCA", "ncca_2015_site_information_great_lakes-data.csv")
NCCAwq2010 <- c(file.path("Data", "NCCA", "assessed_ncca2010_waterchem.csv"))
# , file.path("Data", "NCCA", "nassessedWaterChem2010.csv")
NCCAqa2010 <- file.path("Data", "NCCA", "ncca_qa_codes.csv")
NCCAwq2015 <- file.path("Data", "NCCA", "ncca_2015_water_chemistry_great_lakes-data.csv")
NCCAwqQA <- file.path(teamsFolder, "Results", "NCCAQAcounts_withDecision.xlsx")

# GLENDA 
Glenda <- file.path("Data", "GLENDA", "GLENDA.Rds")
GLENDAlimitsPath <- file.path("Data", "GLENDA", "GLENDAlimits.Rds")
# GLENDAflagsPath <- file.path("Data", "GLENDA", "")
GLENDAflagsPath <- NULL
GLENDAsitePath <- file.path("Data", "GLENDA", "GLENDAsiteInfo.Rds")

# SeaBird data 
seaBird <- list.files(path = file.path(teamsFolder, "Raw_data", "Seabird"), 
  pattern = , ".cnv$", full.names=T) 
seaBird <- seaBird[grepl("_MI", seaBird, ignore.case = t)]


# L drive data
# "L:", "Priv", "Great lakes Coastal" 
# csmi2010 <- file.path("2002-2010 Water Quality", "2010")
# csmi2015 <- file.path("2015 CSMI Lake Michigan", "WQ data and database", 
#   "CSMI data & database", "CSMI_LkMich2015_Database_working_minsRivMouths.accdb")
# csmi2021 <- file.path(teamsFolder, "Raw_data", "CSMI", "2021")

csmi2010 <- file.path("Data", "CSMI", "2010")
csmi2015 <- file.path("Data", "CSMI", "CSMI2015_newQuery.accdb")
csmi2021 <- file.path("Data", "CSMI", "2021")

namingFile <- file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)", "Lake Michigan ML - General", "Results", "Analytes3.xlsx")
