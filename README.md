# GLharmonizeR

This R package contains functions to integrate Great Lakes water quality data from different federal sources, with data currently available for Lake Michigan. The R package harmonizes water quality parameter names, units, and quality control (QC) flags across survey efforts. Functions for imputing missing and censored data and linking observations in time and space are in progress. This R package and associated data are intended to improve the ability of investigators in federal, state, and local agencies and academia to utilize Great Lakes water quality data to better understand and manage the Great Lakes.

# Installation

First make sure the package "devtools" is installed:
``` r
install.packages("devtools")
```

The GLharmonizeR package can be installed directly from the Github source code as follows:

``` r
devtools::install_github("USEPA/GLharmonizeR")
# Load library
library(GLharmonizeR)
```

**Troubleshooting the installation:**
If you get an error similar to below, please delete the noted 00LOCK directory and try the install again.

<span style="color:red;">Error: Failed to install 'GLharmonizeR' from GitHub:</span><br>
<span style="color:red;">&nbsp;&nbsp;ERROR: failed to lock directory ‘C:\\Program Files\\R\\R-4.4.0\\library’ for modifying</span><br>
<span style="color:red;">Try removing ‘C:\\Program Files\\R\\R-4.4.0\\library/00LOCK</span>


# Acquiring data

This package aids users in acquiring fully assembled and harmonized water quality data for Lake Michigan using the `assembleData()` function in a few different ways, as shown below. *Note that the* `assembleData()` *function will produce warnings from the underlying functions that manipulate the data. These are all expected and do not indicate a problem.*

**1) Create an R object for use in current R session without writing out the data:**
``` r
df <- assembleData()
```
-   Creates R object `df` for use in current R session
-   Does not write output


**2) Write data to a compressed .rda file by supplying file path.**
``` r
df <- assembleData(out="filepath", binaryOut = TRUE)

# Example "filepath" for specific user:
# df <- assembleData(out="C:/Users/KVITENSE/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/LakeMichiganData", binaryOut = TRUE)
```
-   This will save an RData (rda) binary version of the compiled data to the location specified by "filepath" (note that an '.rda' extension will automatically be added to the provided filepath and should not be included)
-   RData/rda files can be loaded in R using `load("filepath.rda")`
-   Also creates an R object `df` for use in current R session


**3) Write data to a CSV file by supplying file path.**

``` r
df <- assembleData(out="filepath", binaryOut = FALSE)

# Example "filepath" for specific user:
# df <- assembleData(out="C:/Users/KVITENSE/OneDrive - Environmental Protection Agency (EPA)/Profile/Desktop/LakeMichiganData", binaryOut = FALSE)
```
-   This will save a CSV version of the compiled data to the location specified by "filepath" (note that a '.csv' extension will automatically be added to the provided filepath and should not be included)
-   CSV files can be read into R using `read.csv("filepath.csv")` or `readr::read_csv("filepath.csv")`
-   Also creates an R object `df` for use in current R session



# Column names and descriptions

| Column name       |                                                                                                     Description                                                                                                      |
|-------------|:---------------------------------------------------------:|
| UID               |                                                                                         Unique sample ID across all sources                                                                                          |
| Study             |                                                                                                    Name of study                                                                                                     |
| SITE_ID           |                                                                                                     Name of site                                                                                                     |
| Latitude          |                                                                                             Latitude in decimal degrees                                                                                              |
| Longitude         |                                                                                             Longitude in decimal degrees                                                                                             |
| stationDepth      |                                                                                               Station depth in meters                                                                                                |
| sampleDate        | Date of sampling event. This is either the date provided with the data (with unknown time of day) or is the date associated with the collection time in EST (and thus may differ from the date in sampleDateTimeUTC) |
| sampleDateTimeUTC |                                                                            Date-time of sampling event in UTC time zone, where available                                                                             |
| sampleDepth       |                                                                                              Depth of sample in meters                                                                                               |
| DEPTH_CODE        |                                                                           Depth code of sample from original data source, where available                                                                            |
| CodeName          |                                                                                Harmonized short name for the water quality parameter                                                                                 |
| LongName          |                                                                             Harmonized descriptive name for the water quality parameter                                                                              |
| Category          |                                                 Water quality category inspired by LAGOS-US LIMNO. <https://doi.org/10.6073/pasta/2c58f5a50ab813919f99cc1f265f271c>                                                  |
| ANALYTE_Orig_Name |                                                                               Water quality parameter name in the original data source                                                                               |
| RESULT            |                                                                                               Value of the measurement                                                                                               |
| MDL               |                                                                                       Method detection limit, where available                                                                                        |
| RL                |                                                                                           Reporting limit, where available                                                                                           |
| Units             |                                                                                     Units of measurement for RESULT, MDL and RL                                                                                      |
| ReportedUnits     |                                                                                    Measurement units in the original data source                                                                                     |
| ConversionFactor  |                                                         Multiplicative conversion factor if conversion was necessary to convert from ReportedUnits to Units                                                          |
| Unified_Flag      |                                                                     Unified quality control flag across datasets (see further description below)                                                                     |
| Unified_Comment   |                                                                                   Unified quality control comment across datasets                                                                                    |
| METHOD            |                                                                                        Method of measurement, where available                                                                                        |
| LAB               |                                                                                       Lab taking measurement, where available                                                                                        |
| Orig_QAcode       |                                                                             QA code in the original data source or added in data checks                                                                              |
| Orig_QAcomment    |                                                                            QA comment in the original data source or added in data checks                                                                            |
| Orig_QAdefinition |                                                                                      QA definition in the original data source                                                                                       |

# Unified QC flags

The unified flags to which all QC codes/comments in the original data sources were mapped are described below. The flags/comments from the original data sources can be found in the following columns: Orig_QAcode, Orig_QAcomment, and Orig_QAdefinition. Note that some flags were not found in the original data source and were added during QC checks for this package.

Note that non-detected values ('N' flag) and values below the reporting limit ('R' flag) were preserved in the data with RESULT set to NA. When available, method detection limits are reported in the 'MDL' column, and reporting limits are found in the 'RL' column.

Similarly, Secchi disk measurements that were reported to be clear to bottom were flagged with 'B' and RESULT was set to NA.

Tools are in development to impute these censored measurements but are not available in the current package version.

Note that all values preserved in this dataset were judged to be acceptable per conversations with data owners. However, we leave it to users to inspect flagged data and determine whether observations should be removed for any given application.

| Unified flag code (Unified_Flag column) | Unified flag meaning (Unified_Comment column)                                                           |
|---------------------|---------------------------------------------------|
| B                                       | Secchi clear to bottom                                                                                |
| C                                       | Lab correction factor                                                                                 |
| D                                       | Station depth estimated as max sample depth of CTD (conductivity, temperature, depth) instrument data |
| E                                       | Estimated value (between MDL and RL or another issue)                                                 |
| F                                       | Flag for chlorophyll-a samples analyzed at WSLH which uses a 5 micron pore size                       |
| H                                       | Holding time exceeded                                                                                 |
| L                                       | MDL imputed                                                                                           |
| N                                       | Below Method Detection Limit (MDL) or another unspecified detection limit (non-detection)             |
| P                                       | Corrected PAR exceeds 100% (underwater PAR exceeds surface PAR)                                       |
| Q                                       | Lab or field QC issue                                                                                 |
| R                                       | Below Reporting Limit                                                                                 |

# Water quality parameters included

Below is a list of water quality parameters currently included in the data. Note that the Category column below is inspired by the water quality parameter 'Theme' in LAGOS-US LIMNO module (Shuvo et al. 2023).

| CodeName  | LongName                                                  | Category          | Units |
|-------------|---------------------|---------------------|------------|
| CaCO3     | Alkalinity                                                | Chemical_Physical | mg/L           |
| Diss_NHx  | Ammonia/ammonium, dissolved                               | Nutrients_Algae   | ug/L           |
| Tot_As    | Arsenic, unfiltered                                       | Contaminants      | ug/L           |
| Diss_Ca   | Calcium, dissolved                                        | Chemical_Physical | mg/L           |
| Tot_Ca    | Calcium, unfiltered                                       | Chemical_Physical | mg/L           |
| Part_C    | Carbon, particulate                                       | Clarity_Carbon    | ug/L           |
| Diss_Cl   | Chloride, dissolved                                       | Chemical_Physical | mg/L           |
| Chla      | Chlorophyll-a                                             | Nutrients_Algae   | ug/L           |
| Hardness  | Hardness                                                  | Chemical_Physical | mg/L           |
| Diss_Mg   | Magnesium, dissolved                                      | Chemical_Physical | mg/L           |
| Tot_Mg    | Magnesium, unfiltered                                     | Chemical_Physical | mg/L           |
| Diss_Mn   | Manganese, dissolved                                      | Chemical_Physical | ug/L           |
| Tot_Mn    | Manganese, unfiltered                                     | Chemical_Physical | ug/L           |
| Sed_Moist | Moisture content, sediment, unfiltered                    | Chemical_Physical | percent        |
| Tot_Mo    | Molybdenum, unfiltered                                    | Chemical_Physical | ug/L           |
| Diss_N    | Nitrogen, dissolved                                       | Nutrients_Algae   | ug/L           |
| Part_N    | Nitrogen, particulate                                     | Nutrients_Algae   | ug/L           |
| Sed_TN    | Nitrogen, sediment, unfiltered                            | Nutrients_Algae   | mg/g           |
| Tot_N     | Nitrogen, unfiltered                                      | Nutrients_Algae   | ug/L           |
| DOC       | Organic carbon, dissolved                                 | Clarity_Carbon    | mg/L           |
| POC       | Organic carbon, particulate                               | Clarity_Carbon    | mg/L           |
| Sed_TOC   | Organic carbon, sediment, unfiltered                      | Clarity_Carbon    | mg/g           |
| Diss_NOx  | Oxidized nitrogen (nitrite+nitrate), dissolved            | Nutrients_Algae   | ug/L           |
| DO        | Oxygen, dissolved                                         | Chemical_Physical | mg/L           |
| pH        | pH                                                        | Chemical_Physical | unitless       |
| Diss_P    | Phosphorus, dissolved                                     | Nutrients_Algae   | ug/L           |
| Part_P    | Phosphorus, particulate                                   | Nutrients_Algae   | ug/L           |
| Sed_TP    | Phosphorus, sediment, unfiltered                          | Nutrients_Algae   | mg/g           |
| Tot_P     | Phosphorus, unfiltered                                    | Nutrients_Algae   | ug/L           |
| CPAR      | Photosynthetically active radiation (PAR) as % of surface | Chemical_Physical | percent        |
| Diss_K    | Potassium, dissolved                                      | Chemical_Physical | mg/L           |
| Tot_K     | Potassium, unfiltered                                     | Chemical_Physical | mg/L           |
| Secchi    | Secchi Disc Transparency                                  | Clarity_Carbon    | m              |
| Tot_Se    | Selenium, unfiltered                                      | Contaminants      | ug/L           |
| Diss_SiO2 | Silica, dissolved                                         | Nutrients_Algae   | mg/L           |
| Diss_Na   | Sodium, dissolved                                         | Chemical_Physical | mg/L           |
| Tot_Na    | Sodium, unfiltered                                        | Chemical_Physical | mg/L           |
| Cond      | Specific conductivity                                     | Chemical_Physical | uS/cm          |
| Diss_SO4  | Sulfate, dissolved                                        | Chemical_Physical | mg/L           |
| SRP       | Soluble reactive phosphorus (orthophosphate)              | Nutrients_Algae   | ug/L           |
| Temp      | Temperature                                               | Chemical_Physical | C              |
| Turb_FTU  | Turbidity, Formazin Turbidity Units                       | Clarity_Carbon    | FTU            |
| Turb_NTU  | Turbidity, Nephelometric Turbidity Units                  | Clarity_Carbon    | NTU            |

# Data sources

Data were taken from the following sources:

-   EPA's Great Lakes National Program Office (GLNPO) Great Lakes Environmental Database, [GLENDA](https://cdx.epa.gov/)
    -   1983 - 2023
    -   Queried Water Quality Survey Data for Lake Michigan on 2023/10/23
-   EPA's GLNPO Seabird Database Application available through the [GLNPO.NET Portal](https://login.glnpo.net/dana-na/auth/url_default/welcome.cgi)
    -   2003 - 2023
-   EPA's National Coastal Condition Assessment, [NCCA](https://www.epa.gov/national-aquatic-resource-surveys/ncca)
    -   2010, 2015
-   Cooperative Science Monitoring Initiative [CSMI](https://www.epa.gov/great-lakes-monitoring/cooperative-science-and-monitoring-initiative-csmi)
    -   2015, 2021
    -   2021 PAR data from Berry et al. (2025)
-   National Oceanic and Atmospheric Administration Great Lakes Environmental Research Laboratory, [NOAA](https://www.glerl.noaa.gov/)
    -   2007 - 2022

# Dataset design

The fundamental sampling unit is defined by a unique temporal and spatial position defined by date (with or without time of day), latitude, longitude, and sample depth. The data are available in "long" format where each row represents an observation for a single water quality parameter indexed by position and time. Functions to pivot the data to wide format based on proximity in time and space are in progress.

# Recommendations for users

-   Utilize the QC flags and comments for your specific application
-   Be aware of censored data (see QC flags)
-   Report any issues via Github either as a discussion or open an issue

# Known issues
-   Precompiled data file in /data/allWQ.rda is not being recognized after package install.
-   Unit tests are not currently implemented.
-   Some GLNPO sites are missing latitude/longitudes or do not have decimal degrees reported to many decimal places. This is especially true for earlier years (1980s and 1990s).
-   Some CSMI 2021 sites (STO#) are missing latitude/longitudes.
-   Times need to be added for CSMI 2021 EPA CTD data (in 'Station Reference' tab).
-   NCCA 2015 Diss_NOx was computed as sum of nitrate and nitrite, and their MDLs and RLs were also summed together; unsure of validity of this approach.
-   NCCA 2020 data are not currently included (functions still need review).
-   NOAA CTD data has a lot of dropped data at 1-2 m depth, which needs investigation.

<!---
1)  Loading preassembled data
-   Access after R package install using 
``` r
data("allWQ")
```
-   Note: this comes from a static realization of the source data created on 2025-07-21 and therefore may not be up to date.
-->




# Citations

Shuvo, A.K., N.R. Lottig, K.E. Webster, A. Delany, K. Reinl, C. Gries, N.J. Smith, A.C. Poisson, I.M. McCullough, S.M. Collins, K.B. King, E. Phillips, K.S. Cheruvelil, and P.A. Soranno. 2023. LAGOS-US LIMNO: Data module of surface water chemistry from 1975-2021 for lakes in the conterminous U.S. ver 5. Environmental Data Initiative. https://doi.org/10.6073/pasta/2c58f5a50ab813919f99cc1f265f271c (Accessed 2025-07-17).

Berry, N. L., P. M. Dieter, and D. Bunnell. 2025. Underwater measurements of photosynthetically active radiation from 2021 Lake Michigan CSMI survey. U.S. Geological Survey ScienceBase. doi: 10.5066/P13E7ZZF 

# Disclaimer

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government.

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
-   Pivot from long to wide format

    ``` r
    dfshort <- .exactPivot(dflong)
    ```

    --\> \<!--- \## [General functionality](docs/useNdesign.md) In general, this toolbox is meant to aid researchers by reading, cleaning, and joining data from different sources for Lake Michigan. This toolbox does the following (each of which will be documented more thoroughly in the following sections)

-   Download data from remote sources (under construction)

-   Clean each dataset individually

-   Combine into a unified dataset

-   Provide utilities for processing (imputation etc.) 
-->
