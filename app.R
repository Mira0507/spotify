library(shiny)
library(tidyverse)
library(ggplot2)
library(DT)
library(plotly)

# Load input data
genr <- read.csv('genres_v2.csv')

ui <- fluidPage(
  titlePanel('Exploring Characteristics of Music Played on Spotify'),
  sidebarLayout(
    sidebarPanel(
      selectInput('genre', 'Select genre:', 
                  choices=unique(genr$genre), 
                  multiple=TRUE),
      radioButtons('vs',
                   'Select contrast:',
                   choices=c('danceability', 'duration_ms', 'acousticness', 'loudness')),
      radioButtons('log',
                   'X/Y-axis log-transformation:',
                   choices=c('Yes', 'No'))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Density Plot', plotly::plotlyOutput('density')),
        tabPanel('Box Plot', plotly::plotlyOutput('box')),
        tabPanel('Table', DT::DTOutput('table'))
      )
    )
  )
)

server <- function(input, output) {
  
  filtered <- reactive({
    genr %>%
      dplyr::filter(genre == input$genre) %>%
      dplyr::select(danceability,
                    duration_ms,
                    genre,
                    loudness,
                    acousticness)
  })
  
  msg <- reactive({
    validate(
      need(input$genre != "", "Be sure to select your genre of interest")
    )
  })
  
  plot.density <- reactive({
    if (input$vs == 'danceability') {
      plt <- ggplot(filtered(), aes(x=danceability, color=genre))
    } else if (input$vs == 'duration_ms') {
      plt <- ggplot(filtered(), aes(x=duration_ms, color=genre))
    } else if (input$vs == 'loudness') {
      plt <- ggplot(filtered(), aes(x=loudness, color=genre)) 
    } else {
      plt <- ggplot(filtered(), aes(x=acousticness, color=genre))  
    } 
    if (input$log == 'Yes') {
      plt <- plt + scale_x_continuous(trans='log10')
    }
    plt
  })
  
  plot.box <- reactive({
    if (input$vs == 'danceability') {
      plt <- ggplot(filtered(), aes(x=genre, y=danceability, color=genre))
    } else if (input$vs == 'duration_ms') {
      plt <- ggplot(filtered(), aes(x=genre, y=duration_ms, color=genre))
    } else if (input$vs == 'loudness') {
      plt <- ggplot(filtered(), aes(x=genre, y=loudness, color=genre)) 
    } else {
      plt <- ggplot(filtered(), aes(x=genre, y=acousticness, color=genre))  
    } 
    if (input$log == 'Yes') {
      plt <- plt + scale_y_continuous(trans='log10')
    }
    plt
  })
  
  output$density <- plotly::renderPlotly({
    msg()
    plot.density() + geom_density()
  })
  
  output$box <- plotly::renderPlotly({
    msg()
    plot.box() + geom_boxplot()
  })
  
  output$table <- DT::renderDT({
    msg()
    filtered()
  })
}

shinyApp(ui=ui, server=server)

