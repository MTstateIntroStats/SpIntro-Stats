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
  taglist(
    fluidRow(
      column(3,  
             h4("Count values equal to or")
      ),
      column(4,
             tags$div(style="width: 200px",
                      tags$select(id=ns('testDirection'), class="form-control",
                                  tags$option( value = "less", "less"),
                                  tags$option( value = "more extreme", "more extreme", selected = TRUE),
                                  tags$option( value = "greater", "greater"))
             )),
      column(1, h4(" than ")),
      column(2,
             tags$div( 
               tags$input(id = ns("test_cutoff"), type = "text", class = "form-control", value = NA))
      ),
      column(1,
             actionButton(ns('test_countXtremes'), "Go", class="btn btn-success")
      )
    )#,
    #h4(pvalue2print(moreExtremeCount, sampleCount, direction, cutoff, pvalue))
  )
}  

Pvalue <- function(input, output, session, stats, nullParm ){
  ## what to do on the server end
  ## input$parts2refer2  references the pieces in verbUI
  ## return a list of reactive expressions
  
  x <- sort(as.numeric(stats))
  nsims <- length(x)
  parm0 <- as.numeric(nullParm)
  colors <- rep(blu, nsims)
  cutoff <- threshold <- as.numeric(input$test_cutoff)
  direction <- input$testDirection
  if(nsims > 1 & !is.na(direction)){
    redValues <-  switch(direction,
                         "less" = which(x < threshold + 1.0e-10),
                         "greater" = which(x > threshold - 1.0e-10),
                         "more extreme" = which(abs(x - parm0) > abs(threshold - parm0) - 1.0e-10 )) 
    colors[redValues] <- rd       
    moreExtremeCount  <- length(redValues)
    pvalue <- moreExtremeCount/nsims
    simCount <- nsims
  }
  return( reactive({ 
    list(redValues, colors, moreExtremeCount, pvalue, simCount)
  }))
}    

## In ui.R refer to the module as

verbUI("name4thisInstance")  ##  placement: where you want to see the UI appear


##In server.R use callModule

newVerbFunction <- callModule(verb, "name4thisInstance", args,  reactive(input$checkbox1) )

