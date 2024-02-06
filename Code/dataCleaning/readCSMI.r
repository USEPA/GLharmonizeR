library(RODBC)
dta <- odbcConnectAccess2007("/Users/ccoffman/Environmental Protection Agency (EPA)/Lake Michigan ML - General/Results/CSMI_LkMich2015_Database_working_minsRivMouths.accdb")


db_list <- RODBC::sqlQuery(
  channel = dta,
  query = "SELECT name FROM master..sysdatabases")
##
RODBC::sqlTables(
  channel = dta
)

df1 <- sqlFetch(dta, "MSysNameMap")   #loads the table called 'bar' in the original Access filedf2 <- sqlFetch(dta, "bin")   #loads the table called 'bin' in the original Access file
df <- sqlFetch(dta, "L1_StationMaster")   #loads the table called 'bar' in the original Access filedf2 <- sqlFetch(dta, "bin")   #loads the table called 'bin' in the original Access file
df <- sqlFetch(dta, "L2a_StationSampleEvent")
df <- sqlFetch(dta, "L2b_ThermStructure")
df <- sqlFetch(dta, "L3a_SampleLayerList")
df <- sqlFetch(dta, "L3b_CTDLayerData")
df <- sqlFetch(dta, "L3b_LabWQdata")
df <- sqlFetch(dta, "Metadata_ChangeLog")
df <- sqlFetch(dta, "Metadata_ChemLayerDef")
df <- sqlFetch(dta, "LMetadata_ThermLayerDef")
df <- sqlFetch(dta, "Metadata_WQanalytes")
sqlColumns(dta)
odbcGetInfo(dta)


sqlQuery(channel, paste("select State, Murder from USArrests",
                        "where Rape > 30 order by Murder"))

sqlColumns(dta, "L1_StationMaster")
