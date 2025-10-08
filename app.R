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

library(scales)
library(ggplot2)
library(ggimage)
library(ggiraph)

library(bslib)


avg_diff <- 'avg_diff.rds'

ad <- readRDS(avg_diff)


# Define UI for application that draws a histogram-----
ui <- page_navbar(
  
  # Application title
  title = "FCFB Statistics!",
  
  # Sidebar with a slider input for number of bins 
  sidebar = sidebar(
    selectInput(
      "Season",
      "Select the season",
      list("Season 10" = 10, "Season 11" = 11), selected = max(ad$season), multiple = FALSE
    ),
    selectInput(
      "Conference",
      "Select a conference",
      list("All" = NA,
           "ACC" = "ACC",
           "American" = "AMERICAN",
           "Big 12" = "BIG_12",
           "Big Sky" = "BIG_SKY",
           "Big 10" = "BIG_TEN",
           "Colonial" = "COLONIAL",
           "Independent" = "FBS_INDEPENDENT",
           "Ivy League" = "IVY_LEAGUE",
           "MAC" = "MAC",
           "Missouri Valley" = "MISSOURI_VALLEY",
           "Mountain West" = "MOUNTAIN_WEST",
           "NEC" = "NEC",
           "PAC 12" = "PAC_12",
           "SEC" = "SEC",
           "Southland" = "SOUTHLAND",
           "Sun Belt" = "SUN_BELT"
      )
    )
  ),
  
  # Nav Panel 01
  nav_panel(title = 'Average Diff', 
            # Show a plot of the generated distribution
            girafeOutput("distPlot")),
  
  # Nav Panel 02
  nav_panel(title = 'Success Rate',
            #Show a plot of the success rate stuff
            girafeOutput("successPlot"))
)

# Define server logic required to draw a histogram-----
server <- function(input, output) {

  
  output$distPlot <- renderGirafe({
    #Filter to the season
    ad1 <- ad[ad$season == input$Season,]
    
    #Static chart parameters, shouldn't change as conferences change
    diffline <- sum(ad1$sumD.x) / sum(ad1$Off_plays)
    max_lim <- max(ad1$Off_avg_diff)
    min_lim <- min(ad1$Off_avg_diff)
    max_lym <- max(ad1$Def_avg_diff)
    min_lym <- min(ad1$Def_avg_diff)
    
    if (input$Conference != "NA") {
      ad1 <- ad1[ad1$conference == input$Conference,]
    }
    
    
    
    p1title <- paste('Season',input$Season,'average difference',sep = ' ')
    
    plot1 <- ggplot(ad1, aes(x = Off_avg_diff, y = Def_avg_diff, tooltip = tooltip)) + geom_image_interactive(aes(image = logo),alpha = 0.5) +
      labs(x = 'Average Difference on Offense', y = 'Average Difference on Defense', title = p1title) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.subtitle = element_text(hjust = 0.5)) +
      xlim(max_lim, min_lim) +
      ylim(min_lym, max_lym) +
      geom_vline(xintercept =  diffline, color = "red", linetype = "dashed", alpha=0.5) +
      geom_hline(yintercept = diffline, color = "red", linetype = "dashed", alpha=0.5)
    
    girafe(ggobj = plot1,options = list(opts_zoom(min = 1, max = 5) ) )
  })
  
  output$successPlot <- renderGirafe({
    #Filter to the season
    ad1 <- ad[ad$season == input$Season,]
    
    #Static chart parameters, shouldn't change when conference changes
    min_o_sp <- min(ad1$s_off_sp)
    max_o_sp <- max(ad1$s_off_sp)
    min_d_sp <- min(ad1$s_def_sp)
    max_d_sp <- max(ad1$s_def_sp)
    succline <- sum(ad1$s_off_succ)/sum(ad1$s_off_plays)
    dsuccline <- sum(ad1$s_def_succ)/sum(ad1$s_def_plays)
    
    if (input$Conference != "NA") {
      ad1 <- ad1[ad1$conference == input$Conference,]
    }
    

    
    p2title <- paste('Season',input$Season, 'success rates',sep = ' ')
    
    plot2 <- ggplot(ad1, aes(x = s_off_sp, y = s_def_sp, tooltip = stooltip)) + geom_image_interactive(aes(image = logo)) +
      labs(x = 'Success Rate on Offense', y = 'Success Rate on Defense', title = p2title) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(plot.subtitle = element_text(hjust = 0.5)) +
      geom_vline(xintercept =  succline, color = "red", linetype = "dashed", alpha=0.5) +
      geom_hline(yintercept = dsuccline, color = "red", linetype = "dashed", alpha=0.5) +
      scale_x_continuous(labels = scales::percent_format(scale = 100), limits = c(min_o_sp,max_o_sp)) +
      scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(min_d_sp,max_d_sp))
    
    girafe(ggobj = plot2,options = list(opts_zoom(min = 1, max = 5) ) )
  })
}

# Run the application -----
shinyApp(ui = ui, server = server)
