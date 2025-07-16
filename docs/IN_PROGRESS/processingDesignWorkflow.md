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
    - [ ] type checks: measurements, lat, and lons are doubles (note: lat/lon are DD83 not degree minutes)
    - [ ] all relevant columns have a descriptive name
    - [ ] all meta data (site information, flag information, etc) can be read satisfying same checklist
    - [ ] Secondary files are joined in
    - [ ] Missingness per column is unchanged
      - [ ] If changed, is it expected?
        - Pivoting will change the missingness
        - Censored data have additional NA's introduced

- Clean (.cleanXXX)
  - source specific filtering
  - join in unifying meta data
    - Unified flags, unified analyte names, unified units
  - apply relevant flags
  - Checklist:
    - [ ] All analytes are mapped to CodeName, if not, must be explicitly listed as "Remove"
      - dplyr: . %>% count(ANALYTE, ANL_CODE, CodeName) %>% reframe(sum(is.na(CodeName)) == 0)
      - base: table(df$ANALYTE, df$ANL_CODE, df$CodeName)
      - If not, fill out the rest of the name mapping sheet such that there aren't any more missing
      - Test must specify all of the analytes that are not mapped EXPLICITLY (as "remove"), NOT PASSIVELY
    - [ ] All flags are mapped to decision
      - Same check as for naming

    - [ ] Missingness unchanged unless known difference
      - [ ] Lat/lon depth - same missingness, less analytes "remove"-d
      - [ ] Units - no missingness
      - [ ] Result - same missingness less, less analytes "remove"-d
      - [ ] CodeName - same missingness less, less analytes "remove"-d 


- If minimal reformatting is required can make function .readCleanXXX

## File naming
Each set of processing functions which apply to a distinct data source should get its own filename that describes the source data which it is processing.
