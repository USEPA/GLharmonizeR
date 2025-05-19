# [ ] KV: Need to revisit this documentation and where to reference it


# Data
The data were taken from the following sources. Here we summarize how the data were gathered from each source, the data contained within each source, as well as the cleaning results.

## The Great Lakes Environmental Database, [GLENDA](https://cdx.epmeea.gov/)
Credentials were requested for querying GLENDA (Great Lakes Environmental Database Query System) hosted through the central data exchange by the EPA. We directly queried the database on 2023/10/23 using the GLENDA Query System and based our query off of the following criteria: 

- Select Query: Water Quality Survey Data
- Analyte Code: "Selected every analyte"
- Year: "Select all"
- Season: Blank
- Station: Blank
- Lake: Blank

Data were then downloaded directly using the Query GLENDA Database button. A summary of the dataset after cleaning is shown in the following table 

![Summary of GLENDA data. \label{tab:GLENDAsummary}](figsTables/GLENDA_summary.png)


## National Coastal Condition Assessment, [NCCA](https://www.epa.gov/national-aquatic-resource-surveys/ncca)
NCCA data are downloaded directly from Rscripts using the file locations hosted on the NCCA website. The files are stored as comma separated files and located by filtering by survey choosing all Coastal surveys. Data containing both WaterChem and Site information were downloaded for each year. A summary of the resulting data after cleaning is shown in the following table

![Summary of NCCA data. \label{tab:NCCAsummary}](figsTables/NCCAsummary.png)

## CSMI (hosted locally)

## National Oceanic and atmospheric Administration, [NOAA](https://www.noaa.gov/)
