#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Fantasy Baseball Player Comparison"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
             selectInput("stat", "1.Select Statistic",
             choices = c("Batting Average" = "avg","RBI/Game" = "rbi","Hits/Game" = "hpg",
                                 "HR/Game" = "hr","SB/Game" = "sb"), selected = "avg"),
             out = "2. Select Players to Compare",
              checkboxInput("bh", "Bryce Harper", FALSE),
            #  
              checkboxInput("js", "Juan Soto", FALSE),
            # 
              checkboxInput("ja", "Jose Altuve", FALSE),
            #
              checkboxInput("aj", "Aaron Judge", FALSE),
            # 
              checkboxInput("tt", "Trea Turner", FALSE),
            # 
              checkboxInput("mt", "Mike Trout", FALSE),
              checkboxInput("pg", "Paul Goldschmidt", FALSE),
              checkboxInput("mb", "Mookie Betts", FALSE)
            ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
