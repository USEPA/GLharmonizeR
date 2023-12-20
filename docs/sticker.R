library(tidyverse)
lakes <- sf::read_sf("Data/Other/ne_10m_lakes.shp")

statline <- data.frame(x = seq(from =0, to =2.2, length.out = 50)) %>%
  mutate(y =  cos(x) +  rnorm(n = 50, mean=0, sd = 0.1)) %>%
  mutate(x = (x- min(x))/ (max(x) - min(x)),
         y = (y- min(y))/ (max(y) - min(y)),
         x = x * (88 - 84.9) - 88,
         y = y * (46- 41.7) + 41.7)
 

g <- ggplot(lakes) +
  theme_void() +
  geom_sf(fill = "grey") +
  coord_sf(xlim = c(-88, -84.9), ylim = c(41.7, 46)) + 
  geom_point(data = statline, aes(x = x, y=y), size= 0.1) +
  geom_smooth(data = statline, aes(x = x, y=y), color = "#87B13F", size = 0.1)





hexSticker::sticker(
  g,
  package = "LM Chla",
  p_size = 20,
  s_width = 1,
  s_height = 1,
  s_x = 1,
  dpi = 320, filename = "docs/figsTables/logo.png"
)

magick::image_read("docs/figsTables/logo.png")

