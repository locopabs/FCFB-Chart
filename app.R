#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Do other data stuff here?----
library(shiny)
library(readr)
library(tidyverse)

library(ggplot2)
library(ggimage)

library(bslib)


avg_diff <- 'avg_diff.rds'

ad <- readRDS(avg_diff)


# Define UI for application that draws a histogram-----
ui <- page_sidebar(

    # Application title
    title = "First FCFB Shiny",

    # Sidebar with a slider input for number of bins 
    sidebar = sidebar(
            selectInput(
              "Season",
              "Select the season",
              list("Season 10" = 10, "Season 11" = 11)
            )
        ),

    # Show a plot of the generated distribution
    plotOutput("distPlot")
)

# Define server logic required to draw a histogram-----
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white',
        #     xlab = 'Waiting time to next eruption (in mins)',
        #     main = 'Histogram of waiting times')
      
        # First test FCFB
        #ggplot(pl, aes(play_call)) + geom_bar()
        
        #diffline = sum(ad[ad$season == input$season]$sumD.x) / sum(ad[ad$season == input$season]$Off_plays)
        
        ad1 <- ad[ad$season == input$Season,]
        
        ggplot(ad1, aes(x = Off_avg_diff, y = Def_avg_diff, tooltip = tooltip)) + geom_image(aes(image = logo),alpha = 0.5) +
          labs(x = 'Average Difference on Offense', y = 'Average Difference on Defense', title = 'Season 11 average difference') +
          theme(plot.title = element_text(hjust = 0.5)) + 
          theme(plot.subtitle = element_text(hjust = 0.5)) +
          xlim(max(ad1$Off_avg_diff),min(ad1$Off_avg_diff))
      
    })
}

# Run the application -----
shinyApp(ui = ui, server = server)
