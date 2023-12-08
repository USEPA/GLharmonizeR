[Go back](../README.md)

# Data Cleaning 
Here we outline the different steps that were taken to preprocess each dataset. In general, we applied filters with respect to 

- Sample types
- QC types
- Comments/ FLAGS
- Location (Lat/Lon or Lake names)
- NA's or otherwise problematic values

In addition there were extra considerations in harmonizing based upon:

- Matching units
- Naming analytes
- Other data tidying steps

Additionally, there were some other tidying up that was unique to each dataset.

## [GLENDA](https://cdx.epmeea.gov/)
[Cleaning function](Code/DataCleaning/readCleanGLEDA.R)

For a look at how we protyped this QC procedure see the [accompanying prototyping document](Results/Code/dataCleaning/QCGLENDA.html)

## Additional data sources
* [GLENDA](https://cdxapps.epa.gov/cdx-glenda/action/querytool/querySystem) 
* [QA codes](https://cdxapps.epa.gov/cdx-glenda/action/linkdownloads/ExcelDownloads?downloadFile=&uploadFileId=964) taken from 2019 prepackaged data, from the "Qualifiers" sheet. Note: Didn't use "QC codes" sheet because couldn't find where this would map to the dataset 
* Didn't utilize Depth codes from prepackaged workbook either  

### Filters
- Columns were type enforced
- SAMPLE_TYPE: Selected "Individual" or "INSITU_MEAS" 
- QC_TYPE: Selected "routine field sample"
- Dropped observations where both the VALUE and REMARK were missing. These were interpretted as being completely unmeasured given that the initial shape of the dataset introduced many more missing values 
  - This was done prior to enforcing VALUE as a numeric so we didn't induce any missingness

### Harmonizing
- Unified measurement units
  - Checked for misreported units based on orders of magnitude separation
- Renamed analytes 
- Missing fractions were filled in when obvious (for instance if analyte is always soluble)
- Lat/Lon (Under Construction)

### Other
- Pivoted into a long format before applying all of the above filters and harmonization steps making it easier for analysis.
- Inferred inclusion risks for each RESULT_REMARK (see later in report)
- Incorporated detection limits for imputation with reference to the SOPs

## NCCA
- Sample types
- QC types
- Comments/ FLAGS
- Location (Lat/Lon or Lake names)
- NA's or otherwise problematic values

In addition there were extra considerations in harmonizing based upon:

- Matching units
- Naming analytes
- Other data tidying steps

Additionally, there were some other tidying up that was unique to each dataset.



[Go back](../README.md)