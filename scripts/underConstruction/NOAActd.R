library(oce)
library(tidyverse)

 ctdDir <- file.path("~", "Environmental Protection A
ency (EPA)", "Lake Michigan ML - General", "Raw_data",
 "NOAA", "CTD 2007-2022")

allfiles <- list.files(ctdDir, full.names = FALSE, recursive = TRUE)
sampleEvents <- allfiles[grepl("^20.*/[[:digit:]*].*", allfiles, ignore.case=T)]


# [ ] Find how many .cnv files
cnvFiles <- c(list.files(path = ctdDir, recursive = T, pattern = "*.cnv", full.names = T ), 
  list.files(path = ctdDir, recursive = T, pattern = "*.CNV", full.names = T ))
# [ ] Compute coverage by .cnv files
# If good enough
# [ ] join lat lon via station name
purrr::possibly()
readOce <- function(x) oce2df(suppressWarnings(oce::read.oce(x)), studyName = "NOAA", bin = TRUE, downcast = TRUE)
safeReadOce <- purrr::possibly(readOce, quiet = T)


noaaCTD <- cnvFiles[c(1:4, 1740:1746)] %>%
    purrr::map(
      \(x) safeReadOce(x) %>%
        mutate(fileName = basename(x)),
# [ ] parse date and station name
          .progress = TRUE)
    dplyr::bind_rows() %>% 
    dplyr::mutate(Study = "NOAA")

