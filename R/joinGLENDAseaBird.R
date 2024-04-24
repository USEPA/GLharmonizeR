#
#test <- seaBirdFiles %>%
#  purrr::map(seabird2df) %>%
#  dplyr::bind_rows()
#
#test %>%
#  distinct(Station)
#
#testIDs <- readRDS("tests/testthat/fixtures/GLENDAtestIDs.Rds")
#df <- readCleanGLENDA("Data/GLENDA/GLENDA.Rds", sampleIDs = testIDs, namingFile = namingFile)
#df %>%
#  distinct(STATION_ID)
#
#
#joinGLENDASeaBird <- function(glendaDf, seaBirdDf) {
#  dplyr::full_join(glendaDf, seaBirdDf, by = c("STATION_ID" = "Station", "sampleDate", ))
#}
