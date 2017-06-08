library(shiny)
library(shinythemes)
library(ggplot2)
function(input, output) {
  
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
    
   
    
    if (ss<=50) {index=seq(1,length(pp))
    data<-data.frame(index,pp)
      plot(data, type="p", xlab="Times of Simulation", ylab="P Value",
           main="P-value Distribution of Simulation")}
               
    
    else {hist(pp,breaks=5,main="P-value Distribution of Simulation", xlab="P Value")}
    
    
  })
  
 
  output$coordinate<- renderText({
    paste0("x=", input$Click$x, "\ny=", input$Click$y)
  })
  
 
  sliderValues <- reactive({
    num_of_samples = input$n
    nn= input$n2
    # pp=numeric(0)
    x1 <- sample(1:nn,num_of_samples,replace=T)
    
    # Compose data frame
    xx=cbind(paste0(LETTERS[1:nn]),table(x1 ),round(rep(num_of_samples/nn,nn),2)) 
    xx=as.data.frame(xx)
    colnames(xx)=c("Categories","Observed Value","Expected Value")
    xx
  })
  
  
  output$values <- renderTable({
    sliderValues()},
    align="c"
  )
  
  
}



