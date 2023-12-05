library(tidyverse)

# SHORT LIST 
trouble <- df %>%
  mutate(above = STN_DEPTH_M < 20) %>%
  group_by(ANALYTE, FRACTION, METHOD, above) %>%
  drop_na(VALUE) %>%
  reframe(d = log10(max(VALUE)/ min(VALUE))) %>%
  filter(d > 2)

### ONly check those on the short list
test <- df %>%
  mutate(VALUE = log10(VALUE + 0.000001), above = STN_DEPTH_M < 20) %>%
  inner_join(trouble, by = c("ANALYTE", "FRACTION", "METHOD", "above")) %>%
  group_by(ANALYTE, FRACTION, above, METHOD) %>%
  reframe(
          choices = map(list(dist(VALUE)), ~ .x  %>%
              as.matrix() %>%
              (function(x) colMeans(x >2, na.rm = T))))



df %>%
  filter(ANALYTE == "Sodium") %>%
  mutate(VALUE = ifelse(littleUnit, VALUE / 1000, VALUE)) %>%
  ggplot(aes(x = VALUE)) +
  geom_histogram()


