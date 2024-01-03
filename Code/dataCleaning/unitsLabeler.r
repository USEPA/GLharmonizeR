library(tidyverse)


# Convert all units to standard units using the WQX units conversion data table
#units <- read_csv("https://cdx.epa.gov/wqx/download/DomainValues/MeasureUnit.CSV")
units <- read_csv("Data/Meta/MeasureUnit.CSV") %>%
  # remove any annotated with retired
  filter(!grepl("retired", Description, ignore.case = T)) %>%
    mutate(Code = tolower(Code),
    `Target Unit` = tolower(`Target Unit`),
    # temperature is the only that has extra letters
    Code = ifelse(grepl("Temperature", Description, ignore.case = T),
                 str_remove(Code, "deg "), 
                 Code))

standardizeUnits <- function(df, conversionTable){
  df %>% 
    # if no units, relabel with most common units
    mutate(UNITS = if_else(UNITS == "none", names(sort(table(UNITS), decreasing = T)[1]), UNITS), .by = ANALYTE) %>%
    # convert to target units
    left_join(units, by = c( "UNITS" ="Code" )) %>%
    mutate(VALUE = VALUE * `Conversion Factor`,
          UNITS = `Target Unit`) %>%
    # Could consider combining organic carbon units, but haven't yet
    # detect outliers 
    mutate(below10 = SAMPLE_DEPTH_M < 10) %>%
    mutate(p25 = quantile(VALUE, 0.25, na.rm= T),
           p75 = quantile(VALUE, 0.75, na.rm= T),
           IQR = p75 - p25,
           lowerCutoff = p25 - 2* IQR,
           upperCutoff = p75 + 2 * IQR,
           outlier = !between(VALUE, lowerCutoff, upperCutoff),
           .by = c(ANALYTE, below10))
}

df <- standardizeUnits(df, conversionTable) 
  group_by(ANALYTE) %>%
  summarize(mean(outlier, na.rm = T))
### ONly check those on the short list
test <- df %>%
  mutate(VALUE = log10(VALUE + 0.000001), above = STN_DEPTH_M < 20) %>%
  inner_join(trouble, by = c("ANALYTE", "FRACTION", "METHOD", "above")) %>%
  group_by(ANALYTE, FRACTION, above, METHOD) %>%
  reframe(
          choices = map(list(dist(VALUE)), ~ .x  %>%
              as.matrix() %>%
              (function(x) colMeans(x >2, na.rm = T)))
              )


test <- read_csv("Data/Meta/Quantitation Limit Type.csv")
test <- read_csv("Data/Meta/ResultMeasureQualifier.CSV")
test <- read_csv("Data/Meta/ResultSampleFraction.CSV")
test <- read_csv("Data/Meta/Horizontal Collection Method.csv")
test <- read_csv("Data/Meta/QAQCCharacteristicValidation.CSV")a
test %>% 
  filter(grepl("Calcium", Characteristic, ignore.case = T)) %>%
  select(4,9,10)

df %>%
  ggplot(aes(x = VALUE)) + 
  geom_histogram() +
  scale_x_log10() + 
  facet_wrap(~ANALYTE, scales= "free")

df %>%
  summarize(d = dip.test()))

df %>%
  mutate(VALUE = log10(VALUE + 0.0000001)) %>%
  group_by(ANALYTE) %>%
  nest() %>%
  mutate(test = map(data, ~ diptest::dip.test(.x$VALUE)$statistic))  %>%
  unnest(test) 
df %>%
  ggplot(aes(x = VALUE, ))+
  geom_histogram() + 
  geom_text(aes(x = Inf, y = Inf, label = test)) %>%
  facet_wrap(~ANALYTE, scales = "free")

