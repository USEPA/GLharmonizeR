library(tidyverse)

# Load existing Qa file
oldQA <- file.path("~", "Environmental Protection Agency (EPA)", "Lake Michigan ML - General", "Results", "NCCAQAcounts_withDecision - Copy.xls") %>%
  readxl::read_excel(.) %>%
  # simplify existiing Qa file
  tidyr::separate_longer_delim(c(QAcode, Definition) , delim = ";") %>%
  distinct(SAMPYEAR, QAcode, Definition, QAconsiderations, Decision, Action) %>%
  mutate(
    QAcode = stringr::str_squish(QAcode),
    Definition = stringr::str_squish(Definition),
    Study = ifelse(
      QAcode == "B" ~ paste0("NCCA_secchi_", SAMPYEAR),
      .default = "NCCA_secchi_",
    )
    )

# WQ kkk

# Load new QA file
newQA <- read_csv("tempFlags.csv")


# Join the two, coalesce with priority to the previous file to preserve work


