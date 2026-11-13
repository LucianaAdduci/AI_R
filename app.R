library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

datos <- read_excel(
  "Rendimientos_decada.xlsx",
  na = c("", "NA", "SD")
) |>
  mutate(
    Cultivo = if_else(is.na(Cultivo), "Sin dato", as.character(Cultivo)),
    Campaña = if_else(
      is.na(Campaña) | Campaña == "",
      "Sin dato",
      as.character(Campaña)
    )
  )

cultivos <- sort(unique(datos$Cultivo))

ui <- fluidPage(
  titlePanel("Superficie sembrada y cosechada por campaña"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "cultivo",
        "Seleccionar cultivo:",
        choices = c("Todos", cultivos),
        selected = "Todos"
      )
    ),
    mainPanel(
      plotOutput("grafico_superficies", height = "550px"),
      textOutput("nota_faltantes")
    )
  )
)

server <- function(input, output, session) {
  datos_filtrados <- reactive({
    if (input$cultivo == "Todos") {
      datos
    } else {
      filter(datos, Cultivo == input$cultivo)
    }
  })

  resumen_campania <- reactive({
    datos_filtrados() |>
      group_by(Campaña) |>
      summarise(
        `Sup. Sembrada` = if (all(is.na(`Sup. Sembrada`))) {
          NA_real_
        } else {
          sum(`Sup. Sembrada`, na.rm = TRUE)
        },
        `Sup. Cosechada` = if (all(is.na(`Sup. Cosechada`))) {
          NA_real_
        } else {
          sum(`Sup. Cosechada`, na.rm = TRUE)
        },
        .groups = "drop"
      ) |>
      mutate(
        orden = suppressWarnings(as.integer(substr(Campaña, 1, 4))),
        orden = if_else(is.na(orden), Inf, as.numeric(orden))
      ) |>
      arrange(orden, Campaña) |>
      mutate(Campaña = factor(Campaña, levels = unique(Campaña))) |>
      select(-orden) |>
      pivot_longer(
        cols = c(`Sup. Sembrada`, `Sup. Cosechada`),
        names_to = "Superficie",
        values_to = "Hectareas"
      )
  })

  output$grafico_superficies <- renderPlot({
    ggplot(
      resumen_campania(),
      aes(x = Campaña, y = Hectareas, color = Superficie, group = Superficie)
    ) +
      geom_line(na.rm = TRUE) +
      geom_point(na.rm = TRUE) +
      labs(
        title = paste("Cultivo:", input$cultivo),
        x = "Campaña",
        y = "Superficie (hectáreas)",
        color = "Variable"
      ) +
      scale_y_continuous(labels = function(x) {
        format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
      }) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$nota_faltantes <- renderText({
    paste(
      "Los valores faltantes no se grafican.",
      "Si todos los valores de una superficie en una campaña faltan,",
      "el total de esa campaña se deja sin dato."
    )
  })
}

shinyApp(ui = ui, server = server)
