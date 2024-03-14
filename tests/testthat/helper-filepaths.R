# File paths
# NCCA
NCCAhydrofiles2010 = c("https://www.epa.gov/sites/default/files/2016-01/assessed_ncca2010_hydrolab.csv"# , 
# Not using not assessed data after Hugh's suggestion
#"https://www.epa.gov/sites/default/files/2016-01/not_assessed_ncca2010_hydrolab.csv"
)
NCCAhydrofile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_hydrographic_profile_great_lakes-data.csv"
NCCAsecchifile2015 = "https://www.epa.gov/sites/default/files/2021-04/ncca_2015_secchi_great_lakes-data.csv" 

#NCCA WQ files
siteDirectory <- file.path("Data", "NCCA")
preFiles <- file.path("Data", "NCCA", "nca_waterchemdata.csv")
tenFiles<- c(file.path("Data", "NCCA", "assessed_ncca2010_waterchem.csv"),
            file.path("Data", "NCCA", "nassessedWaterChem2010.csv"))
fifteenFiles <- file.path("Data", "NCCA", "ncca_2015_water_chemistry_great_lakes-data.csv")


# GLENDA 
glendaData <- file.path("Data", "GLENDA.Rds")

# L drive data
# csmi2010 <- file.path("L:", "Priv", "Great lakes Coastal", "2002-2010 Water Quality", "2010")
# csmi2015 <- file.path("L:", "Priv", "Great lakes Coastal", "2015 CSMI Lake Michigan",
#   "WQ data and database", "CSMI data & database", "CSMI_LkMich2015_Database_working_minsRivMouths.accdb")
# csmi2021 <- file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)",
#   "Lake Michign ML - General", "Raw_data", "CSMI", "2021")
# namingFile <- file.path(
#   "C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)", "Lake Michigan ML - General", "Results", "Analytes3.xlsx"
# )
csmi2010 <- file.path("Data", "CSMI", "2010")
csmi2015 <- file.path("Data", "CSMI", "CSMI2015_newQuery.accdb")
csmi2021 <- file.path("Data", "CSMI", "2021")

namingFile <- file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)", "Lake Michigan ML - General", "Results", "Analytes3.xlsx")




