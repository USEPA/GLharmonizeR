library(tidyverse)
lakes <- sf::read_sf("data/other/ne_50m_lakes.shp")

statline <- data.frame(x = seq(from =0, to =2.2, length.out = 50)) %>%
  mutate(y =  cos(x) +  rnorm(n = 50, mean=0, sd = 0.1)) %>%
  mutate(x = (x- min(x))/ (max(x) - min(x)),
         y = (y- min(y))/ (max(y) - min(y)),
         x = x * (88 - 84.9) - 88,
         y = y * (46- 41.7) + 41.7)
 

g <- ggplot(lakes) +
  theme_void() +
  geom_sf(fill = "white") +
  coord_sf(xlim = c(-88, -84.9), ylim = c(41.7, 46)) + 
  geom_point(data = statline, aes(x = x, y=y), size= 0.075) +
  geom_smooth(data = statline, aes(x = x, y=y), color = "#87B13F", size = 0.33, se = F)

hexSticker::sticker(
  g,
  package = "GLHarmonizeR",
  p_size = 20,
  s_width = 1,
  s_height = 1,
  s_x = 1,
  dpi = 320, filename = "docs/figsTables/logo.png"
)

hexSticker::sticker(
  g,
  package = "",
  p_size = 20,
  s_width = 1,
  s_height = 1,
  s_x = 1,
  dpi = 320, filename = "docs/figsTables/logoNoWords.png"
)
usethis::use_logo("docs/figsTables/logo.png")
