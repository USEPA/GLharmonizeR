library(tidyverse)
library(xml2)
read_xml("Data/Meta/Characteristic.xml") %>%
  xml_find_all("//d1:WQXElementRow") %>%
  lapply(function(x) {
    xml_children(x) %>%
      xml_attrs() %>%
      bind_rows() %>%
      # filter(colname %in% c("UniqueIdentifier", "Name", "CASNumber", "SRSID", "SampleFractoinRequired", "AnalyticalMethodRequired", "GroupName")) %>%
      pivot_wider(names_from = "colname", values_from = "value") 
  }) %>%
  bind_rows() %>%
  write_csv(df, "Data/Meta/Characteristic.csv")

pg %>% 
  filter(! grepl("retired", Name)) %>% dim()


pg %>% 
  filter(grepl("Alkalin", Name)) %>%
  select(AnalyticalMethodRequired, MethodSpeciationRequired, SampleFractionRequired)

pg %>%
  filter(SampleFractionRequired == "Y",
         MethodSpeciationRequired=="Y") %>%
  print(n= 36)