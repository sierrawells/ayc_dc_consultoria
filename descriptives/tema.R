#
# Autora: SW
# Mantenedoras: SW, AF, PB
# Licencia:  Data Cívica 2024 ©
# ---------------------------------------------
# hunef-analysis/descriptives/tema.R 

# TODO

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, magick, add2ggplot, svglite, showtext)

devices <- c("png", "svg")

# PALETAS DE COLORES ====
pal <- c("#fb9a17", "#5336c6", "#fce1ef", "#b7b6f1", "#e8c963", "#b6dbac", "#47423f")

# DEGRADADOS ====
grad <- c("#e06d3f", "#fbe7cb")

# VALORES CONTSTANTES ====
ejes <- "#A5A5A5"
texto <- "#242223"
nota_al_pie <- "#47423f"
fondo <- "white"

labels_font <- "Barlow"

# FUNCIÓN PARA AGREGAR LOGOS ====
add_both_logos <- function(graf, escala){
  graf_w_dc_logo <- add2ggplot::add_logo(
    plot_path = graf,
    logo_path = here("descriptives/logo_dc.png"),
    logo_position = "bottom right",
    logo_scale = escala)
  
  magick::image_write(graf_w_dc_logo, graf)
  
  graf_w_ayc_logo <- add2ggplot::add_logo(
    plot_path = graf,
    logo_path = here("descriptives/logo_ayc.png"),
    logo_position = "bottom left",
    logo_scale = escala)
  
  magick::image_write(graf_w_ayc_logo, graf)
}

# TEMA PARA GRÁFICAS ====
tema <- theme(
  plot.title = element_text(family = "Barlow",
                            face = "bold",
                            color = texto,
                            size = 20),
  plot.subtitle = element_text(family = "Barlow",
                               color = texto,
                               size = 16),
  axis.text.x = element_text(family = "Barlow",
                             color = texto,
                             size = 14),
  axis.text.y = element_text(family = "Barlow",
                             color = texto,
                             face = "bold",
                             size = 12),
  plot.caption = element_text(family = "Barlow",
                              color = nota_al_pie,
                              size = 12),
  plot.caption.position = "plot",
  axis.title = element_text(family = "Barlow",
                            color = texto,
                            size = 14),
  legend.title = element_text(family = "Barlow",
                             face = "bold",
                             color = texto,
                             size = 14),
  legend.text = element_text(family = "Barlow",
                             color = texto,
                             size = 12),
  legend.background = element_rect(fill = fondo),
  legend.position = "top",
  legend.justification='left',
  strip.text = element_text(
    family = "Barlow",
    face = "bold",
    color = texto,
    size = 12),
  strip.background = element_rect(fill = fondo),
  axis.line = element_line(color = ejes),
  axis.ticks = element_line(color = ejes),
  panel.background = element_blank(),
  panel.grid = element_blank(),
  plot.background = element_rect(fill = fondo, color = fondo),
  plot.margin = margin(5, 5, 40, 5))

# done.