library(tidyverse)



filter_N_plot <- function(df, threshold) {
  df %>%
    dplyr::filter(., a < threshold) %>%
    ggplot(aes(x = a)) +
    geom_histogram() 
}

A <- data.frame(
  "a" = rnorm(100),
  "b" = rnorm(100) 
)

A %>%
  filter_N_plot(., -1)


