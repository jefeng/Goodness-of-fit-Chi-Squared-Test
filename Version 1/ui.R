library(shiny)
library(shinyjs)
library(shinythemes)
shinyUI(fluidPage(
  useShinyjs(),
  theme = shinytheme("flatly"),
  titlePanel("Chi-Square Goodness-of-Fit Test and Simulation"),
 
  sidebarPanel(
    #   conditionalPanel(
    #      condition="input.resample == 0 || output.totalPrev == output.total",
    inputPanel(id="setup",
               helpText("Enter the null probabilities as decimal numbers.",
                        "If they do not sum to 1, then the",
                        "application will re-scale them for you."),
               textInput("nulls","Null Probabilities (separated by commas)",
                         "0.2, 0.2, 0.2, 0.2, 0.2"),
               textInput("obs","Enter Observed Counts (separated by commas)",
                         "20, 16, 28, 11, 25"),
               textInput("names","Enter Level Names (separated by commas)",
                         "One,Two,Three,Four,Five")
    ),
    
    numericInput("sims","Number of Simulations at Once (Limit is 10000)",1,min=0,step=1),
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
                 tabPanel("Density Plot of Simulations",
                           plotOutput("densityplot",height=400,width=600),
                           tableOutput("summary2"),
                           p(textOutput("remarksProbDensity"))),
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