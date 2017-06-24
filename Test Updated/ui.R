library(shiny)
library(shinythemes)
library(ggplot2)
fluidPage(theme = shinytheme("flatly"),
          titlePanel(h3("Chi-Squared Goodness-fit-Test and Simulation ")),
          fluidRow(
            column(3, offset = 0.5,wellPanel(
              sliderInput("n", "Number of Samples:", min = 10, max = 1000, value = 50 ,
                          step = 1),
              sliderInput("n2", "The number of Categories:", min = 1, max = 8, value = 5 ,
                          step = 1) ,
              sliderInput("n3", "The number of Simulation:", min = 1, max = 1000, value = 5 ,
                          step = 1, animate = TRUE),
              
              br(),
               tags$div(a(href="https://klosever.shinyapps.io/Version_1/", 
                          "Click here if you have real data to test "))
            )),
           
           
            column(width = 4,offset = 1, tableOutput("values")),
              
            column(6,height=500,plotOutput("plot1", width = 570, height = 430,click = "plot_click")),
            
            div(style = "position:absolute;bottom: 8em;left:60em;",
                tableOutput("plot_clickedpoints"))
        )
)





