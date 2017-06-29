library(shiny)
library(shinythemes)
library(ggplot2)
function(input, output) {
  
  firstdata<-reactive({
    num_of_samples = input$n
    nn= input$n2
    ss= input$n3
    mytable<-list(0)
    for(i in 1:ss){
      x <- sample(1:nn,num_of_samples,replace=T)
      mytable[i]<-list(x)
    }
    mytable
  })
  

  plotdata<-reactive({
    num_of_samples = input$n
    nn= input$n2
    ss= input$n3
    pp=numeric(0)
    xx<-firstdata()
    for(i in 1:ss){
      x<-unlist(xx[i])
      nulls=1/(1:nn)
      total=table(x)
      expected=nulls*total
      a <- chisq.test(table(x), correct= FALSE, rescale.p=TRUE )
      pp[i]=a$p.value
      
    }
    if (ss<=50) {
      index=seq(1,length(pp))
      data=data.frame(index,pp)
    }
    else
    {
      data=data.frame(pp)
    }
    data
  })
  
  output$plot1 <- renderPlot({
    ss= input$n3
    nn= input$n2
    d<-plotdata()
    if (ss<=50)
    {
      plot(d,xlab="Simulation Index", ylab="P Value",
           main="P-value Distribution of Simulation", pch=18, cex=2, col="#1C2C5B")
    }
    else {hist(d$pp,breaks=5,main="P-value Distribution of Simulation", xlab="P Value")
      abline(h = ss/nn, col = "red")}
  })
  
    clickedpoints<- reactive({
    # For base graphics, I need to specify columns, though for ggplot2,
    # it's usually not necessary.
    num_of_samples = input$n
    nn= input$n2
    mytable<-firstdata()  
    
    data<-plotdata()
    res <- nearPoints(data, input$plot_click, "index", "pp")
    if (nrow(res) == 0)
      return()
    i<-res$index
    pvalue<-round(res$pp,3)
    x1<-unlist(mytable[i])
    xx=cbind(paste0(LETTERS[1:nn]),table(x1),round(rep(num_of_samples/nn,nn),2)) 
    xx=as.data.frame(xx)
    colnames(xx)=c("Categories","Observed Value","Expected Value")
    xx
    
  })
  
    clickedpoints2<- reactive({
      # For base graphics, I need to specify columns, though for ggplot2,
      # it's usually not necessary.
      num_of_samples = input$n
      nn= input$n2
      mytable<-firstdata()  
      
      data<-plotdata()
      res <- nearPoints(data, input$plot_click, "index", "pp")
      if (nrow(res) == 0)
        return()
      i<-res$index
      pvalue<-round(res$pp,3)
      paste("P-value =  ",as.character(pvalue) )
      
    })
  
    output$plot_clickedpoints<-renderTable({
    clickedpoints()},
    align="c"
  )
    
    output$text<- renderText({
      clickedpoints2()
       
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


