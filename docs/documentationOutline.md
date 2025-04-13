

# Introduction to GLharmonizeR
- Accessing GL WQ data is diffult
  - Dispersed across multiple places
  - Different formats
  - Naming and units conventions
  - Flagging conventions

- Other efforts
  - [TADA](https://www.epa.gov/waterdata/TADA) -  queries [Water Quality Portal (WQP)](https://gcc02.safelinks.protection.outlook.com/?url=https%3A%2F%2Fwww.waterqualitydata.us%2F&data=05%7C01%7CHinman.Elise%40epa.gov%7C15a61b9ae4f6411c75a808db3a0648f9%7C88b378b367484867acf976aacbeca6a7%7C0%7C0%7C638167570887982826%7CUnknown%7CTWFpbGZsb3d8eyJWIjoiMC4wLjAwMDAiLCJQIjoiV2luMzIiLCJBTiI6Ik1haWwiLCJXVCI6Mn0%3D%7C3000%7C%7C%7C&sdata=ekQT1YdebuE8bYD41gyEaEw1ITvw2DjNIXqoArbRYCo%3D&reserved=0)
    - Pros: Simple, friendly UI using Shiny, has data from NCCA, GLENDA
    - Cons: Data is from source and therefore missinging large amounts of NCCA, GLENDA and CSMI completely, querying difficult given the multiplicity of names

- What we bring
  - Compile data directly from the sources (more thorough)
  - Add additional data (NOAA, CSMI)
  - Simpler naming convention

- data source overview
- GLENDA
- CSMI
- NCCA
- NOAA

# Usage
## Data permissions 
- Certain datasets require permissions 
  - GLENDA, Seabird
- Describe the process to gain access to these

## R package
- Installation 
- Dependencies
- Running the `assembleData` command

# Method of data curation
Explained using the Extract, Transform, Load (ETL) pneumonic
- Exploring each dataset individually

```mermaid
{% include docs/figsTables/design/developmentCycle.mmd  %}
```

## Extraction
- GLENDA
- CSMI
- NCCA

## Transform
This has many parts, taking the language from [Nature brief](https://www.nature.com/articles/s41597-024-02956-3). Specifically, their section on Implementation for Retrospective Harmonization

- Entities - four entities for which observations occur: lake, site, site/depth, and sample event
- Data model -  how entities are related to one another (and linked through common identifiers)......
- Schema - metadata (Analytes3.xlsx) (analyte descriptions and data column descriptions) along with a datatable

- Standardize format
  - GLENDA
  - CSMI
  - NCCA
  - NOAA
- Normalize variables
  - All are similar - point to the renaming table
  - Flags - meeting with data managers to discern risk levels
- Combine data
- Deduplicate


## Load
- Filter options
- Store as .Rds for efficent read/write/storage
- User only has to run once any time the data is updated


## Testing and Code implementation
- Code implemented following tidyverse style guide
- Unit testing implemented for major features following [Software Engineering at Google](https://abseil.io/resources/swe-book/html/toc.html)
- Data "beta-test" with collaborators
- Data validation
  - Convert small amounts of data by hand for referencing the functions

# Data summary
- Spatio-temporal distributions
- Counts of analyte measurements
- Missingness

# Discussion
- Sampling design
  - mixed types
    - NCCA - heirarchical
    - CSMI - mixed internally
    - GLENDA - ???
- Interpretting analyses
  - Summary statistics
  - Model based
    - Inferential
    - Predicitive
- Missingness 
  - imputation strategies