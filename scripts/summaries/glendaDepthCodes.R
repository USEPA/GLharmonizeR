library(tidyverse)
library(devtools)
load_all()
filepaths <- .getFilePaths()
GLENDA <- .readFormatGLENDA(filepaths["Glenda"], n_max = Inf)  %>%
  mutate(
    DEPTH_CODE =
      case_when(
        grepl("Five", DEPTH_CODE, ignore.case = T) ~ "5",
        grepl("ten", DEPTH_CODE, ignore.case = T) ~ "10",
        grepl("twenty", DEPTH_CODE, ignore.case = T) ~ "20",
        grepl("thirty", DEPTH_CODE, ignore.case = T) ~ "30",
        grepl("forty", DEPTH_CODE, ignore.case = T) ~ "40",
        grepl("fifty", DEPTH_CODE, ignore.case = T) ~ "50",
        grepl("one hundred", DEPTH_CODE, ignore.case = T) ~ "100",
        grepl("two hundred", DEPTH_CODE, ignore.case = T) ~ "200",
        .default = DEPTH_CODE,
      )
  ) %>%
  filter(is.na(as.numeric(DEPTH_CODE))) %>%
  filter(DEPTH_CODE != ".", DEPTH_CODE != "Not Applicable") %>%
  filter(!grepl("bottom", DEPTH_CODE, ignore.case = T)) 

codeOrder <- GLENDA %>% 
  reframe(m= max(SAMPLE_DEPTH_M, na.rm = T), .by = DEPTH_CODE) %>%
  arrange(m) %>%
  pull(DEPTH_CODE)

GLENDA <- GLENDA %>%
  mutate(DEPTH_CODE = factor(DEPTH_CODE, levels = codeOrder))


GLENDA %>%
  ggplot(aes(x = SAMPLE_DEPTH_M, col = DEPTH_CODE)) +
  geom_histogram(aes(y = after_stat(ncount))) +
  facet_wrap(~DEPTH_CODE)

fi <- GLENDA %>% 
  reframe(.by = DEPTH_CODE, five = fivenum(SAMPLE_DEPTH_M)) %>%
  mutate(.by = DEPTH_CODE, Position = rep(c("Minimum", "First Quartile", "Median", "Third Quartile", "Maximum"))) %>%
  pivot_wider(id_cols = DEPTH_CODE, names_from = Position, values_from = five) %>%
  mutate(DEPTH_CODE = factor(DEPTH_CODE, levels = codeOrder)) %>%
  arrange(DEPTH_CODE)

fi %>%
  gt::gt()

fi %>%
  select(DEPTH_CODE, Minimum, Maximum) %>%
  pivot_longer(c(Minimum, Maximum)) %>%
  mutate(DEPTH_CODE = factor(DEPTH_CODE, levels = codeOrder)) %>%
  ggplot(aes(x = value, y = DEPTH_CODE, col = DEPTH_CODE)) +
  geom_point() +
  geom_line()


