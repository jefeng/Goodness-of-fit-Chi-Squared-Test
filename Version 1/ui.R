library(shiny)
library(shinyjs)
library(shinythemes)
shinyUI(fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),
  titlePanel(h3("Chi-Square Goodness-of-Fit Test and Simulation for Real Data")),
  
  sidebarPanel(
    
    inputPanel(id="setup",
              
               textInput("names","Enter Level Names (separated by commas)",
                         ""),
               textInput("nulls","Null Probabilities (separated by commas)",
                         ""),
               textInput("obs","Enter Observed Counts (separated by commas)",
                         "")
               
    ),
   
    numericInput("sims","Number of Simulations at Once",1,min=0,step=1),
    actionButton("resample","Simulate Now"),
    conditionalPanel(
      condition="(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
      actionButton("reset","Start Over")
    )
  ),
  
  mainPanel(
    conditionalPanel(
      condition="input.resample == 0 || output.totalPrev == output.total",
      plotOutput("barGraphInitial"),
      p(textOutput("remarksInitial")),
      tableOutput("obsTable")
    ),
    conditionalPanel(
      condition="(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
      tabsetPanel(selected="Latest Simulation",
                  tabPanel("Latest Simulation",
                           plotOutput("barGraphLatest"),
                           p(textOutput("remarksLatest1")),
                           tableOutput("summary1")),
                           
                  tabPanel("P-Value Plot of Simulations",
                           plotOutput("pvalueplot",height=400,width=630)),                                                                                                                                                                                                                                                                                     
                           
                  
                  tabPanel("Probability Distribution",
                           plotOutput("chisqCurve"),
                           br(),
                           checkboxInput("compareDen",
                                         HTML("Compare with simulated <br>chi-square distribution")),
                           p(textOutput("remarksProb"))
                  ),
                  id="myPanel"
      )
    )
  )
))