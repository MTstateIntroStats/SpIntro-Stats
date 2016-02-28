### global.R

pvalue2print <- function(extremes, nreps, direction, cutoff, pvalue){
  if(extremes > 0){
    paste(extremes," of ",nreps," values are ",direction," than ", cutoff,
          ". P-value = ", round(pvalue,5))
  } else {
    paste(extremes," of ",nreps," values are ",direction," than ", cutoff,
          ". P-value < 1/", nreps, " or P-value < ", round(1/nreps, ceiling(log10(nreps))+1))    
  }
}

## module definitions

## Pvalue: prompt, computation, output.

PvalueUI <- function(id){
  ns <- NS(id)
  ## this function holds the parts to repeat using ns("parts2refer2") 
  tagList(
     h4("Count values equal to or"),
     selectInput(inputID = ns("testDirection"), label=" ", 
                 choices= c(less="less",'more extreme' ="more extreme",greater="greater"), 
                 selected = "more extreme",  selectize = TRUE, width = '200px'),
      h4(" than "),
      textInput(inputId = ns("test_cutoff"), label=" than ", value = NA, width = '50px'),
      actionButton(ns('test_countXtremes'), "Go", class="btn btn-success")
      )
}  

Pvalue <- function(input, output, session, parms, nullParm ){
  ## what to do on the server end
  ## input$parts2refer2  references the pieces in verbUI
  ## return a list of reactive expressions
  x <- sort(as.numeric(parms()))
  nsims <- length(x)
  parm0 <- as.numeric(nullParm)
  colors <- rep(blu, nsims)
  threshold <- as.numeric(input$test_cutoff)
  direction <- input$testDirection
  if(nsims > 1 & !is.na(direction)){
    redValues <-  switch(direction,
                         "less" = which(x < threshold + 1.0e-10),
                         "greater" = which(x > threshold - 1.0e-10),
                         "more extreme" = which(abs(x - nullParm) > abs(threshold - nullParm) - 1.0e-10 )) 
    colors[redValues] <- rd     
    moreExtremeCount  <- length(redValues)
    pvalue <- moreExtremeCount/nsims
    simCount <- nsims
  }
  return( reactive({ 
    list(redValues, colors, moreExtremeCount, pvalue, simCount)
    })
  )
}    

## In ui.R refer to the module as
## verbUI("name4thisInstance")  ##  placement: where you want to see the UI appear

##In server.R use callModule
## newVerbFunction <- callModule(verb, "name4thisInstance", args,  reactive(input$checkbox1) )

