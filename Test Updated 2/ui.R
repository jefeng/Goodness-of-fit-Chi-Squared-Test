library(shiny)
library(shinythemes)
library(ggplot2)
library(shinyBS)
fluidPage(theme = shinytheme("flatly"),
          titlePanel(h3("Chi-Squared Goodness-fit-Test and Simulation ")),
          tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #1C2C5B}")),
          tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #1C2C5B}")),
          tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #1C2C5B}")),
          fluidRow(
            column(3, offset = 0.5,wellPanel(
              radioButtons("random", "Proportion in each category", choices = c("Same", "Different")),
              
              sliderInput("n", "Sample Size:", min = 20, max = 1000, value = 50 ,
                          step = 1),
              bsPopover("n", "", "Number of Observations", place="right"),
              
              sliderInput("n2", "The number of Categories:", min = 2, max = 8, value = 5 ,
                          step = 1) ,
              
              bsPopover("n2", "", "Number of Categories", place="right"),
              
              sliderInput("n3", "The number of Simulation:", min = 1, max = 1000, value = 5 ,
                          step = 1),
              
              bsPopover("n3", "", "For the first 50 simulations, you will see a P Value scatterplot; For the number of simulation greater than 50, you will see a P Value histogram."),
                
              br(),
              
              tags$div(a(href="https://klosever.shinyapps.io/Version_1/", 
                         "Click here if you have real data to test "))
            )),
            
            conditionalPanel(condition = "input.random == 'Different'",

            column(width = 4,offset = 1, tableOutput("values1")),
            bsPopover("values1","","An example of a summary table", placement = "top"),
            
            column(6,height=500,plotOutput("plot1", width = 570, height = 430,click = "plot_click")),
            bsPopover("plot1","","Click the points to see the table behind it.", place="top")
            ),
            
            
            conditionalPanel(condition = "input.random == 'Same'",
                             column(width = 4,offset = 1, tableOutput("values2")),
                             bsPopover("values2","","An example of a summary table", placement = "top"),
                             column(6,height=500,plotOutput("plot2", width = 570, height = 430,click = "plot_click")),
                             bsPopover("plot2","","Click the points to see the table behind it.", place="top")
                             ),
            
            absolutePanel(
              id = "controls", class = "panel panel-default", fixed = FALSE,
              draggable = TRUE, top = 40, left = "auto", right = 20, bottom = "auto",
              width = 473, height = "auto",
              conditionalPanel(condition = "input.random == 'Same'",
                               
              tableOutput("plot_clickedpoints")),
              conditionalPanel(condition = "input.random == 'Different'",
                               tableOutput("plot_clickedpoints2"))),
            
            absolutePanel(
              id = "controls", class = "myClass", fixed = FALSE,
              draggable = TRUE, top = 400, left = "auto", right = 175, bottom = "auto",
              width = 117, height = "auto",
              conditionalPanel(condition = "input.random == 'Same'",
              textOutput("text2")),
              conditionalPanel(condition = "input.random == 'Different'",
                               textOutput("text1")))
            
            
          )
          
          
          
          
)