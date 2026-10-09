library(readxl)
library(dplyr)
library(tidyverse)
library(zoo)
library(ggplot2)
library(scales)
library(lubridate)
library(magick)
library(grid)
library(showtext)
library(shadowtext)

# =======================
# IPSA (siguiendo el modelo DÓLAR)
# =======================

ipsa_pre <- read_xlsx("bolsas.xlsx", sheet = "ipsa") %>%
  mutate(code = "ipsa")

fecha_lastyr <- ipsa_pre %>%
  filter(!is.na(date), !is.na(value)) %>%
  mutate(anho = year(date)) %>%
  filter(anho == year(today()) - 1) %>%
  tail(1) %>%
  pull(date)

ipsa <- ipsa_pre %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= max(date, na.rm = TRUE) - years(1)) %>%
  filter(!is.na(date), !is.na(value))

anho <- year(last(ipsa$date))
ultimo_value <- last(ipsa$value)
value_max <- max(ipsa$value, na.rm = TRUE)
primera_date <- first(ipsa$date)
ultima_date <- last(ipsa$date)
ultima_date_format <- format(ultima_date, "%d-%m-%Y")

# -----------------------
# Foto fondo (como en dólar)
# -----------------------
imagen <- image_read("bolsa_stgo.jpg")
img_editada <- imagen %>%
  image_colorize(opacity = 80, color = "#D8DCF4") %>%
  image_blur(radius = 10, sigma = 20)

img_raster <- rasterGrob(img_editada, width = unit(1, "npc"), height = unit(1, "npc"))

# -----------------------
# Fuente
# -----------------------
font_add_google(name = "Antonio", family = "Antonio")
showtext_auto()
showtext_opts(dpi = 300)

# -----------------------
# Rango dinámico X e Y (idéntico)
# -----------------------
rango_x <- range(ipsa$date)
rango_y <- range(ipsa$value, na.rm = TRUE)

x_inicio <- rango_x[1]
x_final  <- rango_x[2]
x_total  <- as.numeric(x_final - x_inicio)

y_min   <- rango_y[1]
y_max   <- rango_y[2]
y_total <- y_max - y_min

tono <- "#061763"

# -----------------------
# Gráfico (idéntico a dólar en tamaños)
# -----------------------
grafico_para_foto <- ipsa %>%
  ggplot(aes(x = date, y = value)) +
  annotation_custom(img_raster, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  
  # "MSCI IPSA"
  annotate(
    "text",
    x = x_inicio + x_total * 0.02,
    y = y_max + y_total * 0.3,
    label = "\nMSCI IPSA",
    color = tono,
    size = 12,
    fontface = "bold",
    family = "Antonio",
    hjust = 0
  ) +
  
  # "12"
  annotate(
    "text",
    x = x_inicio + x_total * 0.407,
    y = y_max + y_total * 0.3,
    label = "\n12",
    color = tono,
    size = 12,
    fontface = "bold",
    family = "Antonio",
    hjust = 0
  ) +
  
  # "EN"
  annotate(
    "text",
    x = x_inicio + x_total * 0.352,
    y = y_max + y_total * 0.25,
    label = "\nEN",
    color = "#191919",
    size = 7,
    fontface = "bold",
    family = "Antonio",
    hjust = 0
  ) +
  
  # "MESES"
  annotate(
    "text",
    x = x_inicio + x_total * 0.489,
    y = y_max + y_total * 0.25,
    label = "\nMESES",
    color = "#191919",
    size = 7,
    fontface = "bold",
    family = "Antonio",
    hjust = 0
  ) +
  
  # Último value grande (con pts.)
  geom_shadowtext(
    data = tibble(x = x_inicio + x_total * 0.65, y = y_max + y_total * 0.3),
    aes(x = x, y = y),
    label = paste0(
      "\n",
      prettyNum(ultimo_value, big.mark = ".", decimal.mark = ",", nsmall = 2),
      " pts."
    ),
    color = "white",
    size = 9,
    fontface = "bold",
    family = "Antonio",
    hjust = 0
  ) +
  
  # Fecha
  annotate(
    "text",
    x = x_inicio + x_total * 0.75,
    y = y_max + y_total * 0.13,
    label = paste0("\n", ultima_date_format),
    color = "#191919",
    size = 4,
    fontface = "bold",
    family = "Antonio",
    hjust = 0
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  theme_void() +
  geom_line(color = tono, linewidth = 1.5) +
  geom_point(
    data = . %>% filter(date == max(date)),
    size = 3,
    color = tono,
    aes(x = date, y = value)
  )

# -----------------------
# Guardar 600x400 (como tu 6x4 @300dpi)
# -----------------------
ggsave(
  filename = "grafico_ipsa.png",
  plot = grafico_para_foto,
  width = 6,
  height = 4,
  dpi = 300
)

# Instagram

# 1) background tipo "cover": mantiene proporción y recorta (NO estira)
bg_cover <- function(path, px_w, px_h) {
  image_read(path) |>
    image_colorize(opacity = 80, color = "#D9FEEF") |>
    image_blur(radius = 10, sigma = 20) |>
    image_resize(paste0(px_w, "x", px_h, "^")) |>                 # cover
    image_crop(paste0(px_w, "x", px_h, "+0+0"), gravity = "center")  # recorte centrado
}

# 2) tu gráfico SIN background (y con fondo transparente)

  grafico_transparente <- ipsa %>%
  ggplot(aes(x = date, y = value)) +
    # (tus annotate / textos / línea / punto... aquí tal cual)
    geom_line(color = tono, linewidth = 1.5) +
    geom_point(
      data = ipsa %>% filter(date == max(date)),
      size = 3, color = tono
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )

# === Parámetros de salida ===
dpi <- 300
width_in  <- 10.8
height_in <- 14.4
px_w <- round(width_in * dpi)   # 3240
px_h <- round(height_in * dpi)  # 4320

ggsave(
  filename = "ipsa_transparente.png",
  plot = grafico_transparente,
  width = width_in,
  height = height_in,
  dpi = dpi,
  bg = "transparent"
)

rsconnect::deployApp()