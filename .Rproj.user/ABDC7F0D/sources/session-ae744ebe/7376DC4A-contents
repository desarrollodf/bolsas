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

# Datos

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