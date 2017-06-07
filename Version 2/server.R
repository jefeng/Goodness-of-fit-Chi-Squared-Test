library(shiny)
library(shinythemes)
library(pander)
function(input, output) {
  var <- reactiveValues(x1 = NULL, y = NULL)
  
  observe({
    # Initially will be empty
    if (is.null(input$Click)){
      return()
    }
    
    isolate({
      var$x1 <- c(var$x1, input$Click$x1)
      var$y <- c(var$y, input$Click$y)
    })
  })
  
  output$plot1 <- renderPlot({
    num_of_samples = input$n
    nn= input$n2
    ss= input$n3
    pp=numeric(0)
    for(i in 1:ss){
      x <- sample(1:nn,num_of_samples,replace=T)
      
      # p1 <- hist(x,breaks=nn+1,  right=FALSE)
      nulls=1/(1:nn)
      total=table(x)
      expected=nulls*total
      a <- chisq.test(table(x), correct= FALSE, rescale.p=TRUE )
      pp[i]=a$p.value
      
    }
    
   
    
    if (ss<=50) {stripchart(pp,method = "stack",offset = 0.8, at = .15, pch = 19,
               main="P-value Distribution of Chi-Squared Test", xlab="P Value")}
    
    else {hist(pp,breaks=5,main="P-value Distribution of Chi-Squared Test", xlab="P Value")}
    
     lines(var$x1,var$y )
 
  })
  
   
  sliderValues <- reactive({
    num_of_samples = input$n
    nn= input$n2
    # pp=numeric(0)
    x <- sample(1:nn,num_of_samples,replace=T)
    
    # Compose data frame
    xx=cbind(paste0(LETTERS[1:nn]),table(x ),round(rep(num_of_samples/nn,nn),2)) 
    xx=as.data.frame(xx)
    colnames(xx)=c("Categories","Observed Value","Expected Value")
    xx
  })
  
  
  output$values <- renderTable({
    sliderValues()},
    align="c"
  )
  
  
}



