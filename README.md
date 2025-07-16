# LMChla 


This R package contains functions to integrate Great Lakes water quality data from different federal sources, with data currently available for Lake Michigan. The R package harmonizes water quality parameter names, units, and quality control (QC) flags across survey efforts. Functions for imputing missing and censored data and linking observations in time and space are in progress. This R package and associated data are intended to improve the ability of investigators in federal, state, and local agencies and academia to utilize Great Lakes water quality data to better understand and manage the Great Lakes. 

# Installation
This package can be installed directly from the Github source code as follows.

```r
devtools::github_install("USEPA/LM_Chla")
```

Note: This requires the package "devtools" which can be installed as `install.packages("devtools")`.


# Aquiring data
This package aids users in acquiring fully assembled and harmonized water quality data for Lake Michigan in two different ways:

1) Loading preassembled data
  - Access after R package install using `data("allWQ")
  - Note: this comes from a static realization of the source data and therefore may not be up to date.

2) Using functions provided by the package

```r
df <- assembleData()
```
  - Creates R object `df` for use in current R session
  - Does not write output
  
```r
df <- assembleData(out="filepath", binaryOut = TRUE)
```
  - This will save an RData (rda) binary version of the compiled data to the location specified by "filepath" (note that an '.rda' extension will automatically be added to the provided filepath and should not be included)
  - RData/rda files can be loaded in R using `load("filepath.rda")`
  - Also creates an R object `df` for use in current R session

```r
df <- assembleData(out="filepath", binaryOut = FALSE)
```
  - This will save a CSV version of the compiled data to the location specified by "filepath" (note that a '.csv' extension will automatically be added to the provided filepath and should not be included)
  - CSV files can be read into R using `read.csv("filepath.csv")` or `readr::read_csv("filepath.csv")`
  - Also creates an R object `df` for use in current R session

<!---
The full documentation is contained [here](docs/UserInfo.md). The sections below link to different sections throughout that document. The documentation is split into different types that target different end users: [general users](#user-documentation) and [developers](#developer-documentation). Additionally, we provide sparse [documentation on the process](#processtechnical-documentation) which we used to develop this software. The documentation was split this way as per suggestion in [this blogpost](https://helpjuice.com/blog/software-documentation).
-->

<!---
# Suggested workflows -- IN DEVELOPMENT
Suggested workflows (still in development) are included with the package. [These workflows](R/postProcessing.R) provide the ability to:

- Impute censored data based on detection limits
  - starting with data (`df`) in long format
  ```r
  dfimputed <- .dlImputation(df, imputeMethod = "halfMDL")
  ```

- Pivot from long to wide format
  ```r
  dfshort <- .exactPivot(dflong)
  ```
--> 


# Recommendations for users
- Utilize QC flags and remarks
- Be aware of censored data (see QC flags)
- Report any issues via Github either as a discussion or open an issue


<!---
## [General functionality](docs/useNdesign.md)
In general, this toolbox is meant to aid researchers by reading, cleaning, and joining data from different sources for Lake Michigan. This toolbox does the following (each of which will be documented more thoroughly in the following sections)

- Download data from remote sources (under construction)
- Clean each dataset individually
- Combine into a unified dataset
- Provide utilities for processing (imputation etc.)
--> 

# Data sources
Data were taken from the following sources:

- EPA's Great Lakes National Program Office (GLNPO) Great Lakes Environmental Database, [GLENDA](https://cdx.epmeea.gov/)
  - 1983 - 2023
  - Queried Water Quality Survey Data for Lake Michigan on 2023/10/23
- EPA's Great Lakes National Program Office (GLNPO) Seabird Database Application available through the [Great Lakes Portal](https://login.glnpo.net/dana-na/auth/url_default/welcome.cgi)
  - 2003 - 2023
- EPA's National Coastal Condition Assessment, [NCCA](https://www.epa.gov/national-aquatic-resource-surveys/ncca)
  - 2010, 2015
- Cooperative Science Monitoring Initiative [CSMI](https://www.epa.gov/great-lakes-monitoring/cooperative-science-and-monitoring-initiative-csmi)
  - 2015, 2020 
- National Oceanic and Atmospheric Administration Great Lakes Environmental Research Laboratory, [NOAA](https://www.glerl.noaa.gov/)
  - 2007 - 2022


# Dataset design
The fundamental sampling unit is defined by a unique temporal and spatial position defined by date (with or without time of day), latitude, longitude, and sample depth. The data are available in "long" format where each row represents an observation for a single analyte indexed by position and time. Functions to pivot the data to wide format based on closeness in time and space are in progress.



# Disclaimer
The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.
