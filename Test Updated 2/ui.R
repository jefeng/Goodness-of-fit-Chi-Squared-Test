library(shiny)
library(shinythemes)
library(ggplot2)
fluidPage(theme = shinytheme("flatly"),
          titlePanel(h3("Chi-Squared Goodness-fit-Test and Simulation ")),
          tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #1C2C5B}")),
          tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #1C2C5B}")),
          tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #1C2C5B}")),
          fluidRow(
            column(3, offset = 0.5,wellPanel(
              sliderInput("n", "Number of Samples:", min = 20, max = 1000, value = 50 ,
                          step = 1),
              sliderInput("n2", "The number of Categories:", min = 2, max = 8, value = 5 ,
                          step = 1) ,
              sliderInput("n3", "The number of Simulation:", min = 1, max = 1000, value = 5 ,
                          step = 1, animate = TRUE),
              
              br(),
              tags$div(a(href="https://klosever.shinyapps.io/Version_1/", 
                         "Click here if you have real data to test "))
            )),
            
            
            column(width = 4,offset = 1, tableOutput("values")),
            
            
            
            column(6,height=500,plotOutput("plot1", width = 570, height = 430,click = "plot_click")),
            
            absolutePanel(
              id = "controls", class = "panel panel-default", fixed = FALSE,
              draggable = TRUE, top = 40, left = "auto", right = 20, bottom = "auto",
              width = 473, height = "auto",
              tableOutput("plot_clickedpoints")),
            
            absolutePanel(
              id = "controls", class = "myClass", fixed = FALSE,
              draggable = TRUE, top = 400, left = "auto", right = 175, bottom = "auto",
              width = 117, height = "auto",
              textOutput("text"))
            
            
          )
          
          
          
          
)



