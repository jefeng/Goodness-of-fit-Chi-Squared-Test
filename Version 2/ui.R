library(shiny)
library(shinythemes)
library(pander)
fluidPage(theme = shinytheme("flatly"),
          titlePanel(h3("Chi-Squared Goodness-fit-Test and Simulation ")),
          fluidRow(
            column(3, offset = 0.5,wellPanel(
              sliderInput("n", "Number of Samples:", min = 10, max = 1000, value = 50 ,
                          step = 1),
              sliderInput("n2", "The number of Categories:", min = 1, max = 8, value = 5 ,
                          step = 1) ,
              sliderInput("n3", "The number of Simulation:", min = 1, max = 1000, value = 5 ,
                          step = 1),
              submitButton("Submit")
            )),
            column(7,align="center", tableOutput("values")),
            
            column(5,offset=1, align="center",
                   plotOutput("plot1", click=" Click" , width = 600, height = 430),
                   verbatimTextOutput("coordinate")
           )
            
            
          )
)




