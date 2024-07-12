library(devtools)
library(tidyverse)

teamsFolder <- file.path("C:", "Users", "ccoffman", "Environmental Protection Agency (EPA)", "Lake Michigan ML - General")
seaBird <- list.files(path = file.path(teamsFolder, "Raw_data", "Seabird"), 
  pattern = , ".cnv$", full.names=T) 
seaBird <- seaBird[grepl("_MI", seaBird, ignore.case = t)]
test <- F
if (test) {
   seaBird <- seaBird[c(1:5, (length(seaBird) - 5): length(seaBird))]
}
seaBirdDf <- seaBird %>%
    purrr::map(\(x)
      oce2df(suppressWarnings(oce::read.oce(x)), studyName = "SeaBird", bin = TRUE, downcast = TRUE), .progress = TRUE) %>%
    dplyr::bind_rows() %>% 
    dplyr::mutate(Study = "SeaBird")
    saveRDS(seaBirdDf, "../GL_Data/GLENDA/seabird.Rds")

# Save the cleaned seabird Files

