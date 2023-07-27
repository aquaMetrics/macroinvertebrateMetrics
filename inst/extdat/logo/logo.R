# Script for generating logo

logo <- function() {
  ### Script to create logos -----------------------------------------------
  library(hexSticker)
  library(wesanderson)
  library(png)
  library(grid)

  ### macroinvertebrateMetrics ----------------------
  ### Image originally downloaded from flickr
  # https://flic.kr/p/wM17Uk
  # Gammarus pulex L.
  # No known copyright restrictions
  # Image from page 114 of "Blätter für Aquarien- und Terrarien-Kunde" (1917)
  # Title: Blätter für Aquarien- und Terrarien-Kunde
  # Identifier: bltterfraquarien2817stut
  # Year: 1917 (1910s)
  # Publisher: Stuttgart
  # Contributing Library: Harvard University, Museum of Comparative Zoology, Ernst Mayr Library
  # Digitizing Sponsor: Harvard University, Museum of Comparative Zoology, Ernst Mayr Library
  img <- readPNG(system.file("extdat/images", "gammarus-pulex.png", package = "macroinvertebrateMetrics"))
  g <- rasterGrob(img, interpolate = TRUE)

  sticker(g,
          package = "macroinvertebrateMetrics",
          p_color = "#d66853",
          p_x =1 ,
          p_y = 1.1,
          p_size = 12,
          s_x = 1,
          s_y = 1,
          s_width = 1.5,
          s_height = 1.5,
          h_color = "#d66853",
          h_size = 2,
          filename = "man/figures/macro_logo.png",
          white_around_sticker = FALSE,
          l_x = 1,
          l_y = 0.8,
          spotlight = TRUE,
          p_family = "serif",
          h_fill = "#2f579a"
  )

  sticker(g,
          package = "macroinvertebrateMetrics",
          p_color = "#d66853",
          p_x =1 ,
          p_y = 1.1,
          p_size = 12,
          s_x = 1,
          s_y = 1,
          s_width = 1.5,
          s_height = 1.5,
          h_color = "#d66853",
          h_size = 2,
          filename = "vignettes/images/macro_logo.png",
          white_around_sticker = T,
          l_x = 1,
          l_y = 0.8,
          spotlight = TRUE,
          p_family = "serif",
          h_fill = "#2f579a"
  )

  sticker(g,
          package = "macroinvertebrateMetrics",
          p_color = "#d66853",
          p_x =1 ,
          p_y = 1.1,
          p_size = 12,
          s_x = 1,
          s_y = 1,
          s_width = 1.5,
          s_height = 1.5,
          h_color = "#d66853",
          h_size = 2,
          filename = "inst/extdat/images/macro_logo.png",
          white_around_sticker = T,
          l_x = 1,
          l_y = 0.8,
          spotlight = TRUE,
          p_family = "serif",
          h_fill = "#2f579a"
  )
}
