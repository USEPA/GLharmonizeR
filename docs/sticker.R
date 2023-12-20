library(tidyverse)
lakes <- sf::read_sf("Data/Other/ne_10m_lakes.shp")
g <- ggplot(lakes) +
  geom_sf(fill = "grey") +
  coord_sf(xlim = c(-88, -84.9), ylim = c(41.7, 46)) +
  theme_minimal() + 
  theme(axis.text = element_blank(),
        plot.background = element_blank(),
    panel.background = element_blank() )


hexSticker::sticker(
  g,
  package = "LM Chla",
  p_size = 20,
  s_width = 1,
  s_height = 1,
  dpi = 320, filename = "docs/figsTables/logo.png"
)

magick::image_read("docs/figsTables/logo.png")

