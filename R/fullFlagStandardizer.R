# Input full dataset
# - Has necessary column names : Study, Flag, QAcomment, QACode

# output: full dataset + unified flag strategy
# having:
# - coverage for all flags present in data
# - has column names: Flag, QAcomment, QACode
# - Missing values inserted appropriately


.fullFlagStandardizer <- function(data, flagMapperFile) {
  # Read flagMapper file
  flagMap <- openxlsx::read.xlsx(..., )

  # Join flagMapper to data
  data <- dplyr::left_join(data, flagMap, by = ...)
  # Drop old columns
}

# gather all of the column names that contain flags
# Coalesce them as necessary
# ID all uniques
# Put them into spreadsheet
# flags <- df %>%
#  select(Study, QAcomment) %>%
#  distinct()
#
#
# nccaFlags <- openxlsx::read.xlsx(namingFile, sheet= "flag_Map") %>%
#  select(SAMPYEAR, QAcode, Definition, QAconsiderations) %>%
#  mutate(Study = paste("NCCA", SAMPYEAR, sep = "_")) %>%
#  select(-SAMPYEAR)
