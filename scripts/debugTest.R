library(tidyverse)


df <- data.frame(
  "c" = c(1,12,3),
  "d" = c(2,3,4)
)

df %>% 
ggplot(aes(x = c, y = d)) +
  geom_point()
