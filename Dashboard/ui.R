library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyBS)
dashboardPage(skin="blue",
              
              #Title
              dashboardHeader(title="Chi-Square Goodness-fit-Test and Simulation ",titleWidth=450),
              
              #Sidebar
              dashboardSidebar(
                width = 260,
                sidebarMenu(
                  menuItem("Overview and Instruction", tabName = "over", icon = icon("calendar")),
                  menuItem("App", tabName = "first", icon = icon("book"))
                )),
              
              #Content within the tabs
              dashboardBody(
                tabItems(
                  
                  tabItem(tabName = "over",
                          
                          fluidRow(
                            #column of length 12 which is the whole width
                            #I include everthing in a column though because this way there are margins and it looks better
                            column(12,
                                   h3("About:"),
                                   h4("In this app you will explore Chi-Square Goodness-fit-Test and Simulation.
                                        The test is applied when you have categorical variables from a population. 
                                      It is used to determine whether sample data are consistent with a hypothesized distribution." ),
                                   
                                   h3("Background:"),
                                   h4(tags$div("When an analyst attempts to fit a statistical model to observed data, he or she may wonder
                                  how well the model actually reflects the data. How close are the observed values to those which would be expected under the fitted model? 
                                  One statistical test that addresses this issue is the chi-square goodness of fit test. 
                                      This test is commonly used to test association of variables in contingency tables, 
                                      where the assumed model of independence is evaluated against 
                                      the observed data. In general, the chi-square test statistic is of the form:"
                                       ),
                                      br(),
                               tags$img(src = "fit.jpg", width = "384px", height = "86px", style="display: block; margin-left: auto; margin-right: auto;"),
                                      br(),
                                              tags$div("If the computed test statistic is large, then the observed and expected 
                                               values are not close and the model is a poor fit to the data. "
                                      
                                               )),
                                   
                                   
                                   h3("Instruction:"),
                                   h4("1. Select one of the scenarios in terms of proportion in each category"),
                                   h4("2. Move the sliders to change the values of number of observations, number of categories and number of simulations"),
                                   h4("3. Click the link below 'simulation' if you have real data to test"),
                                   h4(tags$div("4. If the number of simulation you select is ",
                                               tags$strong("less than or equal to 50"), 
                                               "you can", tags$strong( "click any points"), "on the scatterplot 
                                      to see the corresponding summary table as well as the p-value "))
                                 
                                  
                                    
                                    
                            )
                          )
                  ),
                  
                  #Define the content contained within part 1 ie. tabname "first"
                  tabItem(tabName = "first",
                          fluidRow(
                            
                            
                            withMathJax(),
                            column(4,
                                   h3("Introduction:"),
                                   box(width ="12.5%",background = "blue",
                                      "You can either see some randomly generated examples based on your choices on 
                                      the proportion in each category, the number of observations, the number of categories and the 
                                      number of simulation or test if you have real data"),
                                   
                                   tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #1C2C5B}")),
                                   tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #1C2C5B}")),
                                   tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #1C2C5B}")),
                              
                                   radioButtons("random", "Proportion in each category", choices = c("Equiprobable Null", "Different Null Probabilities")),
                                   
                                   sliderInput("n", "Sample Size:", min = 20, max = 1000, value = 50 ,
                                               step = 1),
                                   bsPopover("n", "", "Number of Observations", place="right"),
                                   
                                   sliderInput("n2", "The number of Categories:", min = 2, max = 8, value = 5 ,
                                               step = 1) ,
                                   
                                   bsPopover("n2", "", "Number of Categories", place="right"),
                                   
                                   sliderInput("n3", "The number of Simulation:", min = 1, max = 1000, value = 5 ,
                                               step = 1),
                                   
                                   bsPopover("n3", "", "For the first 50 simulations, you will see a P Value scatterplot; For the number of simulation greater than 50, you will see a P Value histogram.", place="right", options = list(container = "body")),
                                   
                                 
                                   h4(tags$div(a(href="https://klosever.shinyapps.io/Version_1/", 
                                              tags$strong("Click here if you have real data to test "))))),
                          column(7,
                                 h3("Table and Plot:"),
                                 conditionalPanel(condition = "input.random == 'Equiprobable Null'",
                                                  tableOutput("values2"),
                                                  bsPopover("values2","","An example of a summary table of population values", placement = "top", options = list(container = "body")),
                                                  tableOutput("plot_clickedpoints"), 
                                                  htmlOutput("text2", class="text-center"),
                                                  plotOutput("plot2", width="90%", click = "plot_click"),
                                                  bsPopover("plot2","","For the number of simulation less than or equal to 50, click the points on the scatterplot to see the table behind it; For the number of simulation greater than 50, you will see a histogram with a red line (uniform density of p value under null)", place="right", options = list(container = "body"))
                          )),
                          
                          column(7,
                                 
                                 conditionalPanel(condition = "input.random == 'Different Null Probabilities'",
                                                  tableOutput("values1"),
                                                  bsPopover("values1","","An example of a summary table of population values", placement = "top", options = list(container = "body")),
                                                  tableOutput("plot_clickedpoints2"), 
                                                  htmlOutput("text1", class="text-center"),
                                                  plotOutput("plot1", width="90%", click = "plot_click"),
                                                  bsPopover("plot1","","For the number of simulation less than or equal to 50, click the points on the scatterplot to see the table behind it; For the number of simulation greater than 50, you will see a histogram with a red line (uniform density of p value under null)", place="right", options = list(container = "body"))
                                 
                                 )
                          
                          )
                          
                  
                  
                 
                                   
                                   
                            )
                          )
                  )
)
)
                  
            
    