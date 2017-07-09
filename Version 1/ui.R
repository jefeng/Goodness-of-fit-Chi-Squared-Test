library(shiny)
library(shinyjs)
library(shinythemes)
shinyUI(fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),
  titlePanel(h3("Chi-Square Goodness-of-Fit Test and Simulation for Real Dataset")),
  
  sidebarPanel(
  
    inputPanel(id="setup",
               helpText("Enter the null probabilities as decimal numbers.",
                        "If they do not sum to 1, then the",
                        "app will re-scale them for you."),
               textInput("names","Enter Level Names (separated by commas)",
                         "A, B, C, D "),
               textInput("nulls","Null Probabilities (separated by commas)",
                         " "),
               textInput("obs","Enter Observed Counts (separated by commas)",
                         " ")
               
    ),
    
    numericInput("sims","Number of Simulations at Once (Limit is 10000)",1,min=0, step=1),
    actionButton("resample","Simulate"),
    conditionalPanel(
      condition="(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
      actionButton("reset","Start Over")
    )
  ),
  
  mainPanel(
    conditionalPanel(
      condition="input.resample == 0 || output.totalPrev == output.total",
      plotOutput("barGraphInitial",height=400,width=860),
      p(textOutput("remarksInitial")),
      tableOutput("obsTable")
    ),
    conditionalPanel(
      condition="(input.resample > 0 && input.reset == 0) || output.total > output.totalPrev",
      tabsetPanel(selected="Latest Simulation",
                  tabPanel("Latest Simulation",
                           plotOutput("barGraphLatest",height=400,width=880),                                                                                                                                                                                                                                                                                     
                           p(textOutput("remarksProbBar"))),
                  tabPanel("P-Value Plot of Simulations",
                           plotOutput("pvalueplot",height=400,width=630),                                                                                                                                                                                                                                                                                     
                           p(textOutput("remarksPvalue"))),
                  tabPanel("Probability Distribution",
                           plotOutput("chisqCurve",height=430,width=610),
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
