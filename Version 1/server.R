library(shiny)

source("chisqplot.R")

shinyServer(function(input, output,session) {
  simLimit <- 10000
  
  
  numberSims <- 0
  chisqSims <- numeric()
  latestSim <- NULL
  fullSim <-character()
  
  total <- 0 #total number of sims over all set-ups including current one
  totalPrev <- 0 #total number of sims over all set-ups excluding current one
  
  
  nullsInput <- reactive({
    probs <- as.numeric(unlist(strsplit(input$nulls,split=",")))
    probs
  })
  
  obsInput <- reactive({
    observed <- as.integer(unlist(strsplit(input$obs,split=",")))
    observed 
  })
  
  namesInput <- reactive({
    unlist(strsplit(input$names,split=","))  
  })
  
  
  goodNulls <- reactive({
    nulls <- nullsInput()
    goodNulls <- TRUE
    if (length(nulls) <= 1) goodNulls <- FALSE
    anyMissing <- any(is.na(nulls))
    if (anyMissing) goodNulls <- FALSE
    if (!anyMissing && any(nulls <= 0)) goodNulls <- FALSE
    if (!goodNulls) disable("resample")
    goodNulls     
  })
  
  goodObs <- reactive({
    obs <- obsInput()
    goodObs <- TRUE
    if (length(obs) <= 1) goodObs <- FALSE
    if (any(is.na(obs))) goodObs <- FALSE
    if (any(obs < 0)) goodObs <- FALSE
    if (!goodObs) disable("resample")
    goodObs     
  })
  
  goodNames <- reactive({
    names <- namesInput()
    goodNames <- TRUE
    if (length(names) <= 1) goodNames <- FALSE
    if (any(is.na(names))) goodNames <- FALSE
    if (!goodNames) disable("resample")
    goodNames     
  })
  
  
  obschisqInput <- reactive({
    nulls <- nullsInput()/sum(nullsInput())
    totalCounts <- sum(obsInput())
    expected <- nulls*totalCounts
    sum(obsInput()^2/expected)-totalCounts
  })
  
  simsUpdate <- reactive({
    if (input$resample > 0) {
      nullProbs <- isolate(nullsInput()/sum(nullsInput()))
      totalCounts <- isolate(sum(obsInput()))
      expCounts <- nullProbs*totalCounts
      reps <- min(simLimit,isolate(input$sims))
      newSims <- rmultinom(n=reps,size=totalCounts,prob=nullProbs)
      chisqNew <- colSums(newSims^2/expCounts)-totalCounts
      chisqSims <<- c(chisqSims,chisqNew)
      latestSim <<- newSims[,reps]
      numberSims <<- numberSims + reps
      total <<- total+reps
      
      
      hide(id="setup",anim=T,animType="slide")
      
      if (total - totalPrev == 1) {
        updateTabsetPanel(session,"myPanel",selected="Latest Simulation")
      }
      
      #now build fake list of outcomes for each trial, on the last sim
      varLevels <- isolate(namesInput())
      namesList <- rep(varLevels,times=latestSim)
      fullSim <<- sample(namesList,size=totalCounts,replace=FALSE)
      list(numberSims,latestSim)
    }
  })
  
  
  #this erases the simulation history and puts user back to initial graph
  simsReset <- reactive({
    input$reset
    totalPrev <<- totalPrev + numberSims
    numberSims <<- 0
    chisqSims <<- numeric()
    latestSim <<- NULL
    
    show(id="setup",anim=T,animType="slide")
    
    return(totalPrev)
  })
  
  
  
  dfInput <- reactive({
    length(obsInput())-1
  })
  
  
  xmaxInput <- reactive({
    qchisq(0.999,df=dfInput())
  })
  
  
  output$totalPrev <- reactive({
    simsReset()
  })
  
  outputOptions(output, 'totalPrev', suspendWhenHidden=FALSE)
  
  output$total <- reactive({
    simsUpdate() 
    total
  })
  
  
  outputOptions(output, 'total', suspendWhenHidden=FALSE)
  
  output$barGraphInitial <- renderPlot({
    
    if (goodNulls()) enable("resample") else disable("resample")
    validate(
      need(goodNulls(),"Enter at least two null probabilities.  They should all be positive numbers.")
    )
    if (goodObs()) enable("resample") else disable("resample")
    validate(
      need(goodObs(),"Enter at least two counts.  All counts should be non-negative integers.")
    )
    if (goodNames()) enable("resample") else disable("resample")
    validate(
      need(goodNames(),"Enter a name for each possible outcome being tallied.")
    )
    
    observed <- obsInput()
    nulls <- nullsInput()/sum(nullsInput())
    names <- namesInput()
    
    lengthCheck <- (length(nulls) == length(observed)) && (length(observed)==length(names))
    if (lengthCheck) enable("resample") else disable("resample")
    validate(
      need(lengthCheck,
           "Make sure that you enter the same number of null probabilities, counts and names.")
    )
    
    
    
    observed <- obsInput()
    expected <- nulls*sum(observed)
    tab <- rbind(observed,expected)
    rownames(tab) <-c("Observed","Expected")
    colnames(tab) <- names
    barplot(tab,beside=T,col=c("dark green","yellow"),
            main="Barchart of Observed and Expected Counts",xlab="",ylab="Counts",
            legend.text=TRUE)
    
  })
  
  output$remarksInitial <- renderText({
    
    observed <- obsInput()
    nulls <- nullsInput()/sum(nullsInput())
    names <- namesInput()
    
    allGood <- (goodNulls() && goodObs()) && goodNames()
    lengthCheck <- (length(nulls) == length(observed)) && (length(observed)==length(names))
    
    validate(
      need(allGood && lengthCheck,"")
    )
    
    chisq <- obschisqInput()
    rounded1 <- round(chisq,2)
    p.value <- pchisq(chisq, length(nulls)-1, lower.tail=FALSE) 
    rounded2<- round(p.value,3)
    paste("Observed Chi-Square Statistic =  ",as.character(rounded1),sep="\n",",",
          "P-value =  ",as.character(rounded2) )
  })
  
  output$obsTable <- renderTable({
    
    observed <- obsInput()
    nulls <- nullsInput()/sum(nullsInput())
    names <- namesInput()
    
    allGood <- (goodNulls() && goodObs()) && goodNames()
    lengthCheck <- (length(nulls) == length(observed)) && (length(observed)==length(names))
    
    validate(
      need(allGood && lengthCheck,"")
    )
    
    expected <- nulls*sum(observed)
    contribs <- (observed-expected)^2/expected
    df <- data.frame(Levels=names,
                     Observed=observed,
                     Expected=round(expected,2),
                     cont=round(contribs,2)
    )
    names(df)[4] <- c("Contribution to Chi-Square")
    df
  }, align="c")
  
  output$barGraphLatest <- renderPlot({
    input$resample
    if (length(chisqSims) > 0) {
      totalCounts <- isolate(sum(obsInput()))
      nulls <- isolate(nullsInput()/sum(nullsInput()))
      expected <- totalCounts*nulls
      tab <- rbind(obsInput(),expected,latestSim)
      rownames(tab) <-c("Observed","Expected","Resampled")
      colnames(tab) <- isolate(namesInput())
      barplot(tab,beside=T,col=c("dark green","yellow","grey"),
              main="Barchart of Observed, Expected, and Latest Resample",xlab="",
              ylab="Counts",
              legend.text=TRUE)
    }
    
  })
  
  chisqDensities <- reactive({
    input$resample
    if (length(chisqSims)==1) band <- 1 else band <- "nrd0"
    density(chisqSims,n=500,from=0,to=xmaxInput(),bw=band)
  })
  
  
  output$pvalueplot <-
    renderPlot({
      input$resample
      nulls <- nullsInput()/sum(nullsInput())
      observed <- obsInput()
      chisq <- obschisqInput()
      obs <- isolate(obschisqInput())
      n <- length(chisqSims)
      latest <- chisqSims[n]
      p.value <- pchisq(chisqSims, length(observed)-1, lower.tail=FALSE)
      hist(p.value,breaks=10,main="P-value Distribution of Simulation", xlab="P Value")
      
    })  
  
  output$chisqCurve <- renderPlot({
    obs <- obschisqInput()
    degFreedom <- dfInput()
    chisqGraph(bound=obs,region="above",df=degFreedom,xlab="Chi-Square Values",
               graph=TRUE)
    abline(v=obs)
    if (input$compareDen) {
      lines(chisqDensities(),col="blue",lwd=4)
      
    }
  })
  
  output$remarksProb <- renderText({
    obs <- obschisqInput()
    paste0("The more simulation you take the better this approximations will be!")
    
  })
  
  
})
