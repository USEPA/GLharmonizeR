# GLENDA
  # [x] extract self reported mdl's
  # Luckily there is no overlap so
  # count(CodeName, mdl, YEAR) %>% arrange(desc(n))
  selfMdls <- df %>%
    dplyr::filter(
      is.na(as.numeric(VALUE)),
      grepl("<", VALUE)
    ) %>%
    dplyr::distinct(ANALYTE, YEAR, VALUE) %>%
    dplyr::mutate(VALUE = readr::parse_number(VALUE), YEAR = as.numeric(YEAR)) %>%
    dplyr::rename(mdl = VALUE) %>%
    dplyr::left_join(., renamingTable, by = "ANALYTE") %>%
    dplyr::select(YEAR, CodeName, mdl) %>%
    dplyr::distinct()

# NCCA 2015 WQ
  # imputing detection limits 
    # 95% missingness beofre LRL 88% after
    # 40% missingness before MDL 24% after
    # Didn't do anything for method so removing it's inference
    # fill in lab specific quantities
    dplyr::mutate(
      LRL = mean(LRL, na.rm = T),
      MDL = mean(MDL, na.rm = T),
      # Lab isn't missing
      .by = c(LAB, ANL_CODE)
    ) %>%
      # Only 5% couldn't be imputed this way
      NitriteMDL = mean(ifelse(ANL_CODE == "NITRITE_N", MDL, NA), na.rm = TRUE),
      NitrateMDL = mean(ifelse(ANL_CODE == "NITRATE_N", MDL, NA), na.rm = TRUE),
      NitrateNitriteMDL = NitriteMDL + NitrateMDL,
      # [x] create mdl for imputing nitrate nitrite
      MDL = ifelse(ANL_CODE == "NITRATE_N", NitrateNitriteMDL, MDL),
