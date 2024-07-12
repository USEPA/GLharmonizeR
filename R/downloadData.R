
.downloadData <- function() {
  download.file(url = "https://github.com/kvitense/GL_Data/archive/refs/heads/main.zip",
    destfile = "GL_Data-main.zip")
  unzip("GL_Data-main.zip")
  file.remove("GL_Data-main.zip")
  setwd("GL_Data-main/CSMI_2015")
  unzip("CSMI2015_newQuery.zip")
  setwd("../..")
}

