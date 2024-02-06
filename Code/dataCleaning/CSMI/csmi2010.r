# Should probably check that these data are not redundant with 2010 NCCA data


#' Load and join data for CSMI 2010 from csv excel files 
#'
#' @description
#' `.LoadCSMI2010` returns a dataframe of all of the joined water quality data relating to CSMI 2010
#' 
#' @details
#' This is a hidden function, this should be used for development purposes only, users will only call
#' this function implicitly when assembling their full water quality dataset
#' @importFrom magrittr "%>%"
#' @param directorypath a string specifying the directory path of the access database
#' @return dataframe of the fully joined water quality data from CSMI 2010
.LoadCSMI2010 <- function(directoryPath){
  df <- readxl::read_xlsx(file.path(directoryPath, "GL2010db.xlsx")) %>%
    # Should we remove blk, intercal, recal, Ipom?
    filter(!grepl("dup", `blk/dup other`)) %>%
    # group all of the different tables together
    pivot_longer(starts_with("STIS #"),
                  names_to = "__stis", values_to = "STIS#") %>%
    pivot_longer(starts_with("Sample Type"),
                  names_to = "__sampleType", values_to = "sampleType") %>%
    # particulate nitroget was contained in muyltiple tables so combine them (they're distinguished by their sample type)
    mutate(`Part N ug/L` = coalesce(`Part N ug/L...35`, `Part N ug/L...37`)) %>%
    select(-contains("..."), -contains("__"))  %>%
    rename(`Na+ mg/L` = `Na+ mg/l`) %>%
    pivot_longer(c(8:25, 33), names_pattern = "^([[:graph:]]*) (.*/L)$", names_to = c("ANALYTE", "UNITS"), values_to = "RESULT") %>%
    mutate(UNITS = str_remove(UNITS, "^[[:space:]]*"))


    # move detection limits to own column
  dls <- df %>%
    filter(`STIS#` == "method detection limit") %>%
    distinct(ANALYTE, RESULT) %>% 
    drop_na() %>%
    rename(mdl = RESULT) %>%
    mutate(mdl = as.numeric(mdl))

  df <- df %>%
    filter(`STIS#` != "STIS #") %>%
    filter(! grepl("detection limit", `STIS#`)) %>%
    left_join(dls, by = "ANALYTE") %>%
    drop_na(RESULT) %>%
    mutate(RESULT = as.numeric(RESULT)) 



  # Didn't find anything immediately usable in here, maybe it will come up when we do more intense QC
  # meta1 <- readxl::read_xls(file.path(directoryPath, "LMich10forms.xls"))
  # meta2 <- readxl::read_xls(file.path(directoryPath, "smpstts10.xls"))

  # CTD data, look like raw CTD measures, where the sheet names might correspond to the site?
#   ctd <- readxl::excel_sheets(file.path(
#     "L:",
#     "Priv",
#     "Great lakes Coastal",
#     "2010 MED Lake Michigan",
#     "CTD data",
#     "CTD_GB",
#     "CTD_Casts_2010",
#     "CTD Casts 2010 Excel.xlsx"
#   ))

  return(df)
}
