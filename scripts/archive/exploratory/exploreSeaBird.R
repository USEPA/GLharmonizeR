library(tidyverse)
library(readxl)
library(oce)


file <- file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)", 
"Lake Michigan ML - General", "Raw_data", "Seabird_bulk_file_download_2024_04_12_100785", "70221_CS82882.cnv")
df <- read.oce(file) 
# do this with moving median
df <- despike(df)

test <- as.data.frame(df@data)

length(test$flag)
sum(is.na(test$flag))
test %>% distinct(flag)