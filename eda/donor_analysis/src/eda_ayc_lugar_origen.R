#
# Author: SW
# Maintainer(s): SW, AF, PB, IS
# License:  Data Cívica 2025 ©
# ---------------------------------------------

# TODO
# Informes de transparencia antes de 2020?

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, janitor, curl, googledrive, readxl, ggthemes,
               ggtext, scales)

# Files
paths <- list(output = here("donor_analysis/eda/output/"))

drive_auth(email = "sierra.wells@datacivica.org")

drive_ayc <- drive_ls(as_id("1C1nq47Q8fnMRaVa1wMeZb1ZIpIRl-Pt0"))

# READ DATA ====

# AyC
ayc_ls <- list()

for (i in seq_along(drive_ayc$id)) {
  name <- drive_ayc$name[i]
  
  year <- str_extract(name, "\\d{4}") %>% 
    as.numeric()
  
  drive_download(as_id(drive_ayc$id[i]), 
                 path = paste0(tempdir(), "/", name),
                 overwrite = TRUE)
  
  tempo <- read_excel(paste0(tempdir(), "/", name), sheet = "Ingreso por donativos") %>% 
    clean_names() %>% 
    mutate(across(.cols = everything(),
                  .fns = ~ ifelse(.x == "", NA, .x)),
           year = year)
  
  ayc_ls[[i]] <- tempo
  
}

rm(tempo)

ayc_donaciones <- bind_rows(ayc_ls)

# GRAPH PREP ====
informe_transp_caption <- "Fuente: Elaboración por Data Cívica a partir de los Informes de Transparencia del SAT"

# CAMBIO EXTRANJERO VS. NACIONAL A LO LARGO DEL TIEMPO ====
cambio_lugar_origen <- ayc_donaciones %>% 
  mutate(
    total = monto_especie + monto_efectivo,
    lugar_origen = case_when(
      str_detect(tolower(donante), "extranjero") ~ "extranjero",
      str_detect(tolower(donante), "nacional") ~ "nacional",
      T ~ NA_character_)) %>% 
  group_by(year, lugar_origen) %>%
  summarize(total = sum(total)) %>% 
  group_by(year) %>%
  mutate(perc = total / sum(total, na.rm = TRUE))

# Total
cambio_lugar_origen_line <- cambio_lugar_origen %>% 
  ggplot(aes(x = year, y = total, color = lugar_origen)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_label(aes(label = paste0(comma(round(total / 1e6)), " MDP")),
            vjust = -0.5, hjust = 0.5, show.legend = FALSE, fontface = "bold",
            fill = "white", label.size = NA, size = 4) +
  labs(title = "Donaciones recibidas por donatarias autorizadas, por lugar de origen del donante",
       subtitle = "Donantes <span style='color:#ea4335;'>**nacionales**</span> vs. <span style='color:#4285f4;'>**extranjeros**</span>",
       x = "Año",
       y = "Monto total de donaciones") +
  scale_color_manual(values = gdocs_pal()(2)) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = " MDP")) +
  theme_gdocs() +
  theme(legend.position = "none",
        plot.subtitle = element_markdown())

ggsave(filename = paste0(paths$output, "cambio_lugar_origen_line.png"),
       plot = cambio_lugar_origen_line,
       width = 12, height = 8)

# Percent
cambio_lugar_origen_bar <- cambio_lugar_origen %>% 
  ggplot(aes(x = year, y = perc, fill = fct_reorder(lugar_origen, -perc))) +
  geom_col(position = "stack") +
  geom_label(aes(label = scales::percent(perc, accuracy = 1),
                 color = fct_reorder(lugar_origen, -perc)),
               position = position_stack(vjust = 0.5), show.legend = FALSE,
               fontface = "bold", fill = "white") +
  labs(title = "Donaciones recibidas por donatarias autorizadas, por lugar de origen del donante",
       subtitle = "Donantes <span style='color:#ea4335;'>**nacionales**</span> vs. <span style='color:#4285f4;'>**extranjeros**</span>",
       x = "Año",
       y = "Porcentaje del monto total de donaciones",
       caption = informe_transp_caption) +
  scale_fill_manual(values = rev(gdocs_pal()(2))) +
  scale_color_manual(values = rev(gdocs_pal()(2))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_gdocs() +
  theme(legend.position = "none",
        plot.subtitle = element_markdown())

ggsave(filename = paste0(paths$output, "cambio_lugar_origen_bar.png"),
       plot = cambio_lugar_origen_bar,
       width = 12, height = 8)

# CAMBIO TIPO DE DONANTE A LO LARGO DEL TIEMPO ====
cambio_tipo_donante <- ayc_donaciones %>% 
  mutate(tipo_donante = case_when(str_detect(donante, "Persona Física") ~ "persona física",
                                  str_detect(donante, "Persona Moral") ~ "persona moral",
                                  str_detect(donante, "Sector Público") ~ "sector público")) %>% 
  group_by(year, tipo_donante) %>%
  summarize(total = sum(monto_especie + monto_efectivo, na.rm = TRUE)) %>%
  group_by(year) %>%
  mutate(perc = total / sum(total, na.rm = TRUE))

# Total
cambio_tipo_donante_line <- cambio_tipo_donante %>%
  ggplot(aes(x = year, y = total, color = tipo_donante)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_label(aes(label = paste0(comma(round(total / 1e6)), " MDP")),
             vjust = -0.5, hjust = 0.5, show.legend = FALSE, fontface = "bold",
             fill = "white", label.size = NA, size = 4) +
  labs(title = "Donaciones recibidas por donatarias autorizadas, por tipo de donante",
       subtitle = "Donantes <span style='color:#fbbc04;'>**personas morales**</span> vs. <span style='color:#ea4335;'>**personas físicas**</span> vs. <span style='color:#4285f4;'>**sector público**</span>",
       x = "Año",
       y = "Monto total de donaciones",
       caption = informe_transp_caption) +
  scale_color_manual(values = gdocs_pal()(3)) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = " MDP")) +
  theme_gdocs() +
  theme(legend.position = "none",
        plot.subtitle = element_markdown())
  
ggsave(filename = paste0(paths$output, "cambio_tipo_donante_line.png"),
       plot = cambio_tipo_donante_line,
       width = 12, height = 8)

# Porcentaje

cambio_tipo_donante_bar <- cambio_tipo_donante %>%
  ggplot(aes(x = year, y = perc, fill = fct_reorder(tipo_donante, perc))) +
  geom_col(position = "stack") +
  geom_label(aes(label = scales::percent(perc, accuracy = 1),
                 color = fct_reorder(tipo_donante, perc)),
             position = position_stack(vjust = 0.5), show.legend = FALSE,
             fontface = "bold", fill = "white") +
  labs(title = "Donaciones recibidas por donatarias autorizadas, por tipo de donante",
       subtitle = "<span style='color:#fbbc04;'>**Personas morales**</span> vs. <span style='color:#ea4335;'>**personas físicas**</span> vs. <span style='color:#4285f4;'>**sector público**</span>",
       x = "Año",
       y = "Porcentaje del monto total de donaciones",
       caption = informe_transp_caption) +
  scale_fill_manual(values = gdocs_pal()(3)) +
  scale_color_manual(values = gdocs_pal()(3)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_gdocs() +
  theme(legend.position = "none",
        plot.subtitle = element_markdown())

ggsave(filename = paste0(paths$output, "cambio_tipo_donante_bar.png"),
       plot = cambio_tipo_donante_bar,
       width = 12, height = 8)

# TIPO DE DONANTE POR LUGAR DE ORIGEN ====

tipo_donante_lugar_origen <- ayc_donaciones %>% 
  mutate(
    total = monto_especie + monto_efectivo,
    lugar_origen = case_when(
      str_detect(tolower(donante), "extranjero") ~ "Donantes extranjeros",
      str_detect(tolower(donante), "nacional") ~ "Donantes nacionales",
      T ~ NA_character_),
    tipo_donante = case_when(str_detect(donante, "Persona Física") ~ "persona física",
                             str_detect(donante, "Persona Moral") ~ "persona moral",
                             str_detect(donante, "Sector Público") ~ "sector público")) %>% 
  group_by(lugar_origen, tipo_donante) %>%
  summarize(total = sum(total, na.rm = TRUE)) %>%
  group_by(lugar_origen) %>%
  mutate(perc = total / sum(total, na.rm = TRUE))

tipo_donante_lugar_origen_bar <- tipo_donante_lugar_origen %>% 
  ggplot(aes(x = 0, y = perc, fill = fct_reorder(tipo_donante, perc))) +
  facet_wrap(~lugar_origen, nrow = 1) +
  geom_col(position = "stack") +
  geom_label(aes(label = scales::percent(perc, accuracy = 1),
                 color = fct_reorder(tipo_donante, perc)),
             position = position_stack(vjust = 0.5), show.legend = FALSE,
             fontface = "bold", fill = "white") +
  labs(title = "Donaciones recibidas por donatarias autorizadas, por tipo de donante",
       subtitle = "<span style='color:#fbbc04;'>**Personas morales**</span> vs. <span style='color:#ea4335;'>**personas físicas**</span> vs. <span style='color:#4285f4;'>**sector público**</span>",
       x = "",
       y = "Porcentaje del monto total de donaciones",
       caption = informe_transp_caption) +
  scale_fill_manual(values = gdocs_pal()(3)) +
  scale_color_manual(values = gdocs_pal()(3)) +
  theme_gdocs() +
  theme(legend.position = "none",
        plot.subtitle = element_markdown(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

ggsave(filename = paste0(paths$output, "tipo_donante_lugar_origen_bar.png"),
       plot = tipo_donante_lugar_origen_bar,
       width = 12, height = 8)
