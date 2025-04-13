# General outline for processing data

These are the general outlines for functions. Some parts of each function may not be necessary given the formatting of the raw data.
Relevant checs

- Read and format file functions (.readFormatXXX)
  - Place file in GL_Data repo
  - Read file from GL_Data
    - Skip empty lines and metadata
    - Read column headers
    - Specify column types for non-character columns
    - pivot into long format
  - Checklist:
    - [ ] input filepath output dataframe
    - [ ] output is long format
    - [ ] type checks: measurements, lat, and lons are doubles
    - [ ] all relevant columns have a descriptive name
    - [ ] all secondary files can be read satisfying same checklist
    - [ ] Secondary files are joined in
      - Choose degree decimal 83 for lat/lon coordinates if possible

- Clean (.cleanXXX)
  - source specific filtering
  - join in unifying meta data
  - rename analytes
  - Get desired units from key
  - convert units
  - apply relevant flags
  - Checklist:
    - [ ] All analytes are mapped to CodeName, if not, must be explicitly listed as "Remove"
      - dplyr: . %>% count(ANALYTE, ANL_CODE, CodeName) %>% reframe(sum(is.na(CodeName)) == 0)
      - base: table(df$ANALYTE, df$ANL_CODE, df$CodeName)
      - If not, fill out the rest of the name mapping sheet such that there aren't any more missing
    - [ ] All flags are mapped to decision
      - Same check as for naming
    - [ ] All analytes mapped to CodeNames (or "remove")

    - [ ] Check missingness
      - [ ] Lat/lon depth
      - [ ] Units
      - [ ] Result
      - [ ] CodeName


- If minimal reformatting is required can make function .readCleanXXX

