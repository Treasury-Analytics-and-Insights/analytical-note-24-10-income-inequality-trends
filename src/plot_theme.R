library(ggplot2)
library(ggtext)
library(scales)

tsy_palette <- c(
  rgb(166,206,227, maxColorValue = 255),
  rgb(31,120,180, maxColorValue = 255),
  rgb(178,223,138, maxColorValue = 255),
  rgb(51,160,44, maxColorValue = 255),
  rgb(251,154,153, maxColorValue = 255),
  rgb(227,26,28, maxColorValue = 255),
  rgb(253,191,111, maxColorValue = 255),
  rgb(255,127,0, maxColorValue = 255),
  rgb(202,178,214, maxColorValue = 255),
  rgb(106,61,154, maxColorValue = 255),
  rgb(255,255,153, maxColorValue = 255),
  rgb(177,89,40, maxColorValue = 255)
)

custom_theme <- theme_minimal(base_size = 12) + theme(
  panel.grid.major.y = element_line(color = "black", linewidth = 0.2),
  panel.grid.minor.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.x = element_blank(),
  plot.subtitle = element_text(size = 10, face = "bold"),
  axis.title.x = element_text(size = 10, face = "bold"),
  axis.title.y = element_text(size = 10, face = "bold"),
  legend.position = "bottom",
  legend.key.size = unit(8, "points"),
  legend.margin=margin(t = 0,b=0, unit='points'),
  legend.justification = "center",
  legend.text = element_text(size=10),
  plot.title = element_text(size = 14),
  text=element_text(color="black"),
  axis.text=element_text(color="black", size = 8),
  axis.ticks.x = element_line(linewidth=0.2),
  plot.title.position = "plot"
)

custom_theme_panel_border <- custom_theme + theme(
  panel.border = element_rect(colour = "grey", fill = NA)
)

custom_theme_small <- theme_minimal(base_size = 8) + theme(
  panel.grid.minor.x = element_blank(),
  panel.grid.major.x = element_blank(),
  axis.text.x = element_text(size = 6),
  legend.position = "bottom",
  legend.key.size = unit(8, "points"),
  legend.text = element_markdown(),
  plot.title = element_markdown(size = 8)
)

options(ggplot2.discrete.colour = tsy_palette)
options(ggplot2.discrete.fill = tsy_palette)

