# Load required packages
library(shiny)
library(tidyverse)
library(ggplot2)
library(DT)
library(plotly)

# Load input data
genr <- read.csv('genres_v2.csv')


# Create user interface
ui <- fluidPage(

    # Add title
    titlePanel('Exploring Characteristics of Music Played on Spotify'),
    # Organize layout
    sidebarLayout(
        # Add input panels
        sidebarPanel(
            # Add input choosing music genre
            selectInput('genre', 'Select genre:', choices=unique(genr$genre), multiple=TRUE),
            # Add input exploring characteristics
            radioButtons('vs',
                         'Select contrast:',
                         choices=c('danceability', 'duration_ms', 'acousticness', 'loudness')),
            # Add input performing log transformation
            radioButtons('log', 'X/Y-axis log-transformation:', choices=c('Yes', 'No'))
            ),
        # Add output panels
        mainPanel(
            # Add tabs
            tabsetPanel(
                # Add a tab for density plot
                tabPanel('Density Plot', plotly::plotlyOutput('density')),
                # Add a tab for box plot
                tabPanel('Box Plot', plotly::plotlyOutput('box')),
                # Add tab for table
                tabPanel('Table', DT::DTOutput('table'))
      )
    )
  )
)

# Create server where interactive computation occurs
server <- function(input, output) {
  
    # Clean the data frame in reactive context
    filtered <- reactive({
        genr %>%
            # Subset rows based on  user input
            dplyr::filter(genre == input$genre) %>%
            # Slice columns of interest
            dplyr::select(danceability,
                        duration_ms,
                        genre,
                        loudness,
                        acousticness)
    })
  
    # Define a message presented by default when no genre is selected
    msg <- reactive({
        validate(
            need(input$genre != "", "Be sure to select your genre of interest")
    )
    })
  
    # Define a function creating a density plot depending on user input
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

    # Define a function creating a boxplot depending on user input
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

    # Add the interactive density plot to the output object
    output$density <- plotly::renderPlotly({
        msg() # Default msg
        plot.density() + geom_density() # Plot
    })

    # Add the interactive boxplot to the output object
    output$box <- plotly::renderPlotly({
        msg() # Default msg
        plot.box() + geom_boxplot() # Plot
      })

    # Add the interactive table to the output object
    output$table <- DT::renderDT({
        msg() # Default msg
        filtered() # Table
      })
}

shinyApp(ui=ui, server=server)

