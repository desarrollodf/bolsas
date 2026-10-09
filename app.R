library(shiny)
library(readxl)
library(dplyr)
library(tidyverse)
library(zoo)
library(ggplot2)
library(plotly)
library(scales)
library(lubridate)
library(purrr)
library(rsconnect)
library(shinyWidgets)
library(shinyBS)
library(magick)
library(grid)
library(showtext)
library(shadowtext)

series_bolsas <- c(
  "S&P IPSA" = "mxipsagc index",
  "Bovespa" = "ibov index",
  "S&P 500" = "spxt index",
  "Euro Stoxx 50" = "sx5t index"
)
colores_bolsas <- c("#C5172E", "#73AF6F", "#27548A", "#309898")

bolsas_bruto <- read_xlsx("datos.xlsx")

bolsas_local <- bolsas_bruto %>%
  filter(ticker %in% series_bolsas) %>%
  mutate(categoria = "local")

bolsas_dolares <- bolsas_bruto %>%
  pivot_wider(names_from = ticker, values_from = value) %>%
  arrange(date) %>%
  na.locf() %>%
  mutate(
    `mxipsagc index` = `mxipsagc index` / `clp curncy`,
    `ibov index` = `ibov index` / `brl curncy`,
    `sx5t index` = `sx5t index` * `eur curncy`
    ) %>%
  select(date, `mxipsagc index`, `ibov index`, `spxt index`, `sx5t index`) %>%
  pivot_longer(cols = -date, values_to = "value", names_to = "ticker") %>%
  mutate(categoria = "dolares")

# Tabla final

bolsas <- bind_rows(bolsas_local, bolsas_dolares)

# =======================
# Imagen del gráfico IPSA
# =======================

ipsa_pre <- bolsas_bruto %>%
  filter(ticker == "mxipsagc index")

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

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
    
    body, .container-fluid, .main-panel {
      background-color: transparent !important;
      margin: 0 !important;
      padding-bottom: 0 !important;
    }
  
  "))
  ),
  
  tags$h4(
    "Evolución de las Bolsas ",
    tags$span(
      icon("question-circle"), 
      id = "info_icon", 
      style = "cursor: pointer; color: #007BFF;"
    )
  ),
  bsTooltip(
    id = "info_icon",
    title = paste(
      "El S&P IPSA (Chile), el Bovespa (Brasil), el S&P 500 (Estados Unidos) y",
      "el Euro Stoxx 50 (eurozona) son índices bursátiles de referencia",
      "para medir el desempeño de sus respectivos mercados accionarios.<br><br>",
      "Los valores incluyen dividendos entregados (vale decir, miden retorno total),",
      "y la opción Dólares hace que en cada caso se tome en cuenta",
      "no sólo el desempeño de las acciones, sino también",
      "el de la moneda local frente al dólar estadounidense."
    ),
    placement = "right",
    trigger = "click"
  ),
  
    prettyRadioButtons(
      inputId = "medicion",
      label = NULL,
      choices = c(
        "Moneda local" = "local",
        "Dólares" = "dolares"
      ),
      selected = "local",
      inline = TRUE,
      animation = "smooth",
      status = "primary",
      shape = "round",
      outline = TRUE
      ),

  absolutePanel(
    top = 10, right = 10, width = "auto", fixed = TRUE, draggable = FALSE,
    class = "panel-periodo-mini",
    shinyWidgets::radioGroupButtons(
      inputId = "periodo",
      label = NULL,
      choices = c(
        "12 meses" = "1",
        "5 años" = "5",
        "10 años" = "10"
        ),
      selected = "1",
      direction = "vertical",
      size = NULL,
      justified = FALSE,
      status = "default"
    )
  ),
  
  fluidRow(
    column(width = 12,
           div(
             style = "position: relative; left: -20px;",
             plotlyOutput("grafico", height = "300px", width = "100%")
           ),
           tags$div(
             style = "display: flex; justify-content: space-between; align-items: center;
                    margin-top: -10px;",
             tags$div(
               style = "font-size: 11px; color: black; display: flex; flex-direction: column; align-items: flex-start;",
               tags$img(src = "icono_flecha.svg", height = "30px", style = "margin-bottom: 2px;"),
               "Fuente: Bloomberg"
             ),
             tags$img(src = "footer.png", height = "35px")
             )
           )
    )
)

server <- function(input, output) {
  
  output$grafico <- renderPlotly({

      filtrado <- bolsas %>%
        filter(
          categoria == input$medicion,
          date >= max(date) - years(as.numeric(input$periodo))
          ) %>%
        group_by(ticker) %>%
        mutate(value = value / first(value) * 100) %>%
        ungroup() %>%
        mutate(
          ticker = factor(ticker, levels = series_bolsas, labels = names(series_bolsas))
        )
      
      grafico <- plot_ly(
        data = filtrado,
        x = ~date,
        y = ~value,
        color = ~ticker,
        colors = setNames(colores_bolsas, names(series_bolsas)),
        hoverinfo = "text",
        type = "scatter",
        mode = "lines",
        hovertemplate = "%{fullData.name}: %{y:,.0f}<extra></extra>"
      ) %>%
      layout(
        dragmode = FALSE,
        hovermode = "x unified",
        legend = list(
          orientation = "h",
          x = 0,
          y = 1.3,
          xanchor = "left",
          font = list(size = 10)
        ),
        xaxis = list(title = ""),
        yaxis = list(title = "Base = 100"),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(
        displayModeBar = FALSE,
        showTips = FALSE,
        scrollZoom = FALSE,
        doubleClick = FALSE,
        staticPlot = FALSE,
        displaylogo = FALSE,
        responsive = TRUE,
        locale = "es"
      )
  })
}

shinyApp(ui, server)