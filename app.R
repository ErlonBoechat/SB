# app.R
library(shiny)
library(dplyr)
library(tidyverse)
library(R6)
library(ggplot2)

source("data_processing.R")
source("calculations.R")
source("visualizations.R")
source("export.R")
source("directory.R")

ui <- fluidPage(
  titlePanel("Análise de Dados de Redes Sociais - Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("process", "Processar Dados"),
      downloadButton("download_csv", "Baixar CSV"),
      downloadButton("download_excel", "Baixar Excel"),
      downloadButton("download_rds", "Baixar RDS")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Gráficos",
                 plotOutput("plot_followers"),
                 plotOutput("plot_engagement"),
                 plotOutput("plot_likes_comments")),
        tabPanel("Métricas", tableOutput("metrics")),
        tabPanel("Variações Percentuais", tableOutput("variations"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactiveVal()
  
  observeEvent(input$process, {
    req(final_data)
    
    calculator <- MetricsCalculator$new(final_data)
    data(list(
      dados = final_data,
      medias = calculator$calcular_medias(),
      variacoes = calculator$calcular_variacoes()
    ))
  })
  
  output$metrics <- renderTable({
    req(data())
    data()$medias
  })
  
  output$variations <- renderTable({
    req(data())
    data()$variacoes
  })
  
  output$plot_followers <- renderPlot({
    req(data())
    visualizer <- DataVisualizer$new(data()$dados)
    visualizer$plot_followers_growth()
  })
  
  output$plot_engagement <- renderPlot({
    req(data())
    visualizer <- DataVisualizer$new(data()$dados)
    visualizer$plot_engagement_rate()
  })
  
  output$plot_likes_comments <- renderPlot({
    req(data())
    visualizer <- DataVisualizer$new(data()$dados)
    visualizer$plot_avg_likes_vs_comments()
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {"dados_processados.csv"},
    content = function(file) {
      exporter <- DataExporter$new(data()$dados)
      exporter$export_to_csv(file)
    }
  )
  
  output$download_excel <- downloadHandler(
    filename = function() {"dados_processados.xlsx"},
    content = function(file) {
      exporter <- DataExporter$new(data()$dados)
      exporter$export_to_excel(file)
    }
  )
  
  output$download_rds <- downloadHandler(
    filename = function() {"dados_processados.rds"},
    content = function(file) {
      exporter <- DataExporter$new(data()$dados)
      exporter$export_to_rds(file)
    }
  )
  
  observeEvent(input$process, {
    req(final_data)
    
    # Exibir os nomes das colunas no console
    print(colnames(final_data))
    
    calculator <- MetricsCalculator$new(final_data)
    data(list(
      dados = final_data,
      medias = calculator$calcular_medias(),
      variacoes = calculator$calcular_variacoes()
    ))
  })
  
}

shinyApp(ui, server)
