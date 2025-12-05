#
# Author: SW
# Maintainer(s): SW, PB, IS
# License:  Data Cívica 2025 ©
# ---------------------------------------------

# TODO

# Packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, here, googledrive, ggwordcloud, lubridate, scales,
               ggtext, tidytext, scales)

# Files
paths <- list(
  news_w_lemmas =  here("import_clean/news_analysis/output/google_news_scraped_lemmatized.csv"),
  theme = here("descriptives/tema.R"),
  output = here("descriptives/output/"))

drive_auth(email = "sierra.wells@datacivica.org")

# READ DATA ====
news_w_lemmas <- read_csv(paths$news_w_lemmas) %>% 
  mutate(date = case_when(
    str_detect(date, "hace") & (str_detect(date, "minuto") |
                                  str_detect(date, "hora") |
                                  str_detect(date, "día") |
                                  str_detect(date, "semana")) ~ "ago 2025",
    str_detect(date, "hace") & str_detect(date, "1 mes") ~ "jul 2025",
    T ~ date),
    mes = str_extract(date, "[a-z]{3}"),
    year = str_extract(date, "\\d{4}"),
    mes_numeric = case_when(
      mes == "ene" ~ 1,
      mes == "feb" ~ 2,
      mes == "mar" ~ 3,
      mes == "abr" ~ 4,
      mes == "may" ~ 5,
      mes == "jun" ~ 6,
      mes == "jul" ~ 7,
      mes == "ago" ~ 8,
      mes == "sep" ~ 9,
      mes == "oct" ~ 10,
      mes == "nov" ~ 11,
      mes == "dic" ~ 12,
      TRUE ~ NA_real_),
    date = as.Date(paste0(year, "-", mes_numeric, "-01")))

# GRAPH PREP ====
google_news_caption <- "Fuente: Elaboración por Data Cívica a partir de noticias scrapeadas de Google News"

source(paths$theme)

n_notas <- nrow(news_w_lemmas)

# PREPARE LEMMAS FOR COUNT ====
lemmas_to_exclude <- c(
  "méxico",
  "osc",
  "ong",
  "ongs",
  "civil",
  "sociedad",
  "mil",
  "organización",
  "organizaciones"
)

lemmas_for_count <- news_w_lemmas %>% 
  # Indicate search words (to exclude from lemmas list)
  mutate(
    searchword_amlo = str_detect(tolower(search_url), "amlo"),
    searchword_morena = str_detect(tolower(search_url), "morena"),
    searchword_sheinbaum = str_detect(tolower(search_url), "sheinbaum"),
    searchword_gobierno_colectivos = str_detect(tolower(search_url), "gobierno") & 
      str_detect(tolower(search_url), "colectivos"),
    searchword_defensores_territorio = str_detect(tolower(search_url), "defensores") & 
      str_detect(tolower(search_url), "territorio"),
    # Clean lemmas
    across(contains("lemmas"), ~str_remove_all(.x, "['\\[\\]]")),
    # Create single lemmas column
    lemmas = paste0(title_lemmas, ", ", description_lemmas),
    # Standardize "defensor" lemmas
    lemmas = ifelse(str_detect(lemmas, "defensor"), "defensor", lemmas)) %>% 
  select(id_nota, date, search_url, starts_with("searchword_"), lemmas, media) %>% 
  separate_rows(lemmas, sep = ", ") %>% 
  mutate(
    # Replace search words in lemmas with NA
    lemmas = 
      case_when(
        (searchword_amlo == TRUE & lemmas == "amlo") ~ NA_character_,
        (searchword_morena == TRUE & lemmas == "morena") ~ NA_character_,
        (searchword_sheinbaum == TRUE & lemmas == "sheinbaum") ~ NA_character_,
        (searchword_gobierno_colectivos == TRUE & lemmas %in% c("gobierno", "colectivos")) ~ NA_character_,
        (searchword_defensores_territorio == TRUE & lemmas %in% c("defensor", "territorio")) ~ NA_character_,
        lemmas %in% lemmas_to_exclude ~ NA_character_,
        lemmas == "sheinbaum" ~ "Sheinbaum",
        lemmas == "amlo" ~ "AMLO",
        lemmas == "ine" ~ "INE",
        lemmas == "cdmx" ~ "CDMX",
        TRUE ~ lemmas)) %>% 
  filter(!is.na(lemmas))

# MOST COMMON LEMMAS ====
lemmas_count <- lemmas_for_count %>% 
  group_by(lemmas) %>% 
  count() %>% 
  filter(n >= 20)

write_csv(lemmas_count, 
          file = paste0(paths$output, "/csv/lemmas_count.csv"))

lemmas_wordcloud <- lemmas_count %>% 
  ggplot(aes(label = lemmas, size = n)) + 
  geom_text_wordcloud(
    color = pal[2],
    family = labels_font,
    fontface = "bold") +
  scale_size_area(max_size = 20) +
  labs(
    title = "Palabras más frecuentes en noticias sobre sociedad civil en Google News",
    subtitle = "Tamaño de la palabra indica frecuencia en títulos y descripciones de notas",
    caption = paste0(google_news_caption,
                     "\nSe excluyen términos de búsqueda y palabras genéricas como 'México', 'OSC', 'ONG', 'organización', etc.",
                     "\nSolo se incluyen palabras con al menos 20 menciones en las notas scrapeadas.")) +
  tema

for (device in devices){
  ggsave(lemmas_wordcloud,
         filename = paste0(paths$output, "lemmas_wordcloud.", device),
         width = 12, height = 8)
  
  walk(paste0(paths$output, "lemmas_wordcloud.", device),
       ~add_both_logos(.x, escala = 7))
}

lemmas_lollis <- lemmas_count %>% 
  ggplot(aes(y = fct_reorder(lemmas, n), x = n)) +
  geom_segment(aes(xend = 0, yend = fct_reorder(lemmas, n)), color = ejes) +
  geom_point(color = pal[2], size = 3) +
  labs(
    title = "Palabras más frecuentes en noticias sobre sociedad civil en Google News",
    subtitle = paste0("Por menciones en títulos y descripciones de ", n_notas, " notas scrapeadas"),
    x = "Número de menciones",
    y = "",
    caption = paste0(google_news_caption,
                     "\nSe excluyen términos de búsqueda y palabras genéricas como 'México', 'OSC', 'ONG', 'organización', etc.")) +
  tema

for (device in devices){
  ggsave(lemmas_lollis,
         filename = paste0(paths$output, "lemmas_lollis.", device),
         width = 10, height = 12)
  
  
  walk(paste0(paths$output, "lemmas_lollis.", device),
       ~add_both_logos(.x, escala = 7))
}

# LEMMAS OVER TIME ====

lemmas_perc_by_year <- lemmas_for_count %>% 
  # Extract 4 digit year from date using regex
  mutate(year = str_extract(date, "\\d{4}") %>% as.numeric()) %>%
  group_by(year) %>% 
  mutate(total_notas_in_year = n_distinct(id_nota)) %>%
  group_by(year, lemmas) %>% 
  reframe(
    total_mentions_in_year = n(),
    perc_mentions = total_mentions_in_year/ total_notas_in_year) %>% 
  ungroup() %>% 
  distinct()

write_csv(
  lemmas_perc_by_year,
  paste0(paths$output, "/csv/lemmas_perc_by_year.csv"))

top_lemmas <- lemmas_perc_by_year %>%
  group_by(lemmas) %>% 
  summarize(total_mentions = sum(total_mentions_in_year)) %>% 
  slice_max(order_by = total_mentions, n = 25, with_ties = F) %>%
  pull(lemmas) %>% 
  unique()

lemmas_perc_by_year_graf <- lemmas_perc_by_year %>%
  filter(lemmas %in% top_lemmas) %>% 
  # Expand all combinations of year and lemmas to fill in 0s
  complete(year = seq(2019, 2025, 1), lemmas = top_lemmas,
           fill = list(perc_mentions = 0, total_mentions_in_year = 0)) %>%
  mutate(lemmas = factor(lemmas, levels = top_lemmas)) %>% 
  ggplot(aes(x = year, y = perc_mentions)) +
  facet_wrap(~ lemmas, ncol = 5, scales = "free_y") +
  geom_line(linewidth = 1, color = pal[2]) +
  labs(
    title = "Frecuencia de palabras en noticias sobre sociedad civil a lo largo del tiempo",
    subtitle = paste0("2019 a 2025: % de títulos y descripciones de notas que mencionan cada palabra"),
    x = "", y = "% de notas que mencionan la palabra",
    caption = paste0(google_news_caption,
                     "\nSe excluyen términos de búsqueda y palabras genéricas como 'México', 'OSC', 'ONG', 'organización', etc.")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2019, 2025, 1)) +
  tema +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1))

for (device in devices){
  ggsave(lemmas_perc_by_year_graf,
         filename = paste0(paths$output, "lemmas_perc_by_year.", device),
         width = 14, height = 10)
  
  
  walk(paste0(paths$output, "lemmas_perc_by_year.", device),
       ~add_both_logos(.x, escala = 7))
}

# NUMBER OF MENTIONS OVER TIME ====
notas_over_time <- news_w_lemmas %>% 
  group_by(date) %>%
  count()

notas_over_time_graf <- notas_over_time %>% 
  ggplot(aes(x = date, y = n)) +
  geom_line(linewidth = 1, color = pal[2]) +
  labs(
    title = "Número de notas sobre sociedad civil en Google News a lo largo del tiempo",
    subtitle = "**Ojo**: muchos medios retiran notas después de cierto tiempo, por lo que el número de notas de años anteriores<br>probablemente sea menor al número real publicado",
    x = "", y = "",
    caption = paste0(google_news_caption, " el 1 de septiembre de 2025.")) +
  scale_x_date(date_labels = "%b %Y",
               date_breaks = "4 months") +
  scale_y_continuous(breaks = seq(0, max(notas_over_time$n) + 5, 10)) +
  tema +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.subtitle = element_markdown())

for (device in devices){
  ggsave(notas_over_time_graf,
         filename = paste0(paths$output, "notas_over_time.", device),
         width = 14, height = 7)
  
  
  walk(paste0(paths$output, "notas_over_time.", device),
       ~add_both_logos(.x, escala = 7))
}

# NÚMERO DE NOTAS POR MEDIO (agosto 2024 - agosto 2025) ====

notas_by_medio <- news_w_lemmas %>% 
  filter(date >= as.Date("2024-08-01") & date <= as.Date("2025-08-31")) %>%
  group_by(media) %>% 
  count() %>% 
  filter(n >= 5) %>% 
  mutate(media = case_when(media == "Centro Mexicano de Derecho Ambiental" ~ "CEMDA",
                           T ~ media))

notas_by_medio_graf <- notas_by_medio %>% 
  ggplot(aes(y = fct_reorder(media, n), x = n)) +
  geom_col(fill = pal[2]) +
  geom_text(aes(label = n), hjust = 1.5, color = fondo,
            family = labels_font, fontface = "bold") +
  labs(
    title = "Número de notas sobre sociedad civil en Google News, por medio de publicación",
    subtitle = "De notas publicadas durante el año anterior al scrapeo (agosto 2024 - agosto 2025)",
    x = "", y = "",
    caption = paste0(google_news_caption,
                     "\nSolo se incluyen medios con al menos 5 notas en el periodo indicado.")) +
  tema +
  theme(
    axis.text.x = element_blank())

for (device in devices){
  ggsave(notas_by_medio_graf,
         filename = paste0(paths$output, "notas_by_medio.", device),
         width = 12, height = 8)
  
  
  walk(paste0(paths$output, "notas_by_medio.", device),
       ~add_both_logos(.x, escala = 7))
}

# LEMMAS BY MEDIO ====
lemmas_perc <- lemmas_for_count %>% 
  mutate(n_notas = n_distinct(id_nota)) %>% 
  group_by(lemmas) %>% 
  mutate(perc_mean = n_distinct(id_nota) / n_notas) %>% 
  select(lemmas, perc_mean, n_notas) %>% 
  distinct()

top_medios <- notas_by_medio %>% 
  arrange(desc(n)) %>%
  head(10) %>% 
  pull(media)

lemmas_by_medio <- lemmas_for_count %>% 
  filter(media %in% top_medios,
         lemmas != "josé") %>% 
  group_by(media) %>% 
  mutate(n_notas = n_distinct(id_nota)) %>% 
  group_by(media, lemmas) %>%
  reframe(perc = n_distinct(id_nota) / n_notas) %>% 
  left_join(lemmas_perc, by = "lemmas") %>% 
  mutate(diff_from_mean = perc - perc_mean) %>% 
  distinct() %>% 
  group_by(media) %>% 
  slice_max(order_by = diff_from_mean, n = 3, with_ties = F)

lemmas_by_medio_graf <- ggplot(lemmas_by_medio,
                                 aes(y = reorder_within(lemmas, perc, media), x = perc)) +
  geom_col(fill = pal[2]) +
  geom_text(aes(label = percent(perc, accuracy = 1)),
            hjust = 1.2,
            family = labels_font,
            color = fondo,
            fontface = "bold") +
  facet_wrap(~ media, ncol = 2, scales = "free_y") +
  scale_y_reordered() +
  labs(
    title = "Palabras más sobrerrepresentadas* en noticias sobre sociedad civil, por medio",
    subtitle = "% de notas de cada medio que mencionan la palabra",
    x = "",
    y = "",
    caption = paste0(google_news_caption,
                     "\n*Sobrerrepresentadas: palabras que aparecen en una proporción mayor de notas de un medio en comparación con el promedio de todos los medios",
                     "\nSolo se incluyen los 10 medios con más notas publicadas y las 3 palabras más sobrerrepresentadas en cada medio.")) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  tema +
  theme(axis.text.x = element_blank())

for (device in devices){
  ggsave(lemmas_by_medio_graf,
         filename = paste0(paths$output, "lemmas_by_medio.", device),
         width = 14, height = 10)
  
  
  walk(paste0(paths$output, "lemmas_by_medio.", device),
       ~add_both_logos(.x, escala = 7))
}

# done.
