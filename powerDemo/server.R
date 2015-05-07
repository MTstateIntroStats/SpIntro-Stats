## server.R  for power demo
library(shiny)

# Define server logic for slider examples
shinyServer(function(input, output) {
  
  # Reactive expression to compose a data frame containing all of the values
  sliderValues <- reactive({
    
    # Compose data frame
    data.frame(
      Setting = c("Sample Size", 
               "Standard Deviation",
               "Shift in Mean"),
      Value = as.character(c(input$n, 
                             input$sd,
                             input$altMean)),
      Name = c( "Significance Level (alpha)",
                                     "Effect Size",  "Power"
                           ),
      Output =as.character(c(input$alpha, round(input$altMean/input$sd,3),
                round(
             power.t.test(n=input$n, delta=input$altMean,
                          sd=input$sd, sig.level=input$alpha,
                          type="one.sample",
                          alternative="one")$power,3))), 
      stringsAsFactors=FALSE)
  }) 
  grn <- rgb(0,1,0,alpha=.4)
  rd <- rgb(1,0,0,alpha=.5)
  output$powerPlot <- renderPlot({
    x <- seq(-4,10,length=200)
    plot(x, dt(x, input$n-1),bty='l', type="l", xlab="",ylab="")
    lines(x,dt(x,input$n-1,ncp=input$altMean/input$sd *sqrt(input$n)))
    abline(h=0)
    qt1 <- qt(1-input$alpha, input$n-1)
    xrr <- c(qt1, qt1, x[x>=qt1],max(x))
    yrr <- c(0, dt(c(qt1,  x[x>=qt1]), input$n -1),0)
    polygon(xrr,yrr, col = rd)
    abline(v = c(0, qt1))
    xpwr <- c(qt1,x[x>=qt1])
    ypwr <- c(0, dt(xpwr,df=input$n-1, ncp=input$altMean/input$sd *sqrt(input$n) ),0)
    xpwr <- c(qt1, xpwr, max(x))
    ##cat(length(xpwr), length(ypwr))
    polygon(xpwr,ypwr, col = grn)
    text(weighted.mean(xrr,w=yrr^2),weighted.mean(yrr,w=yrr^.5),cex=1.5, expression(alpha))
    text(weighted.mean(xpwr,w=ypwr^4),weighted.mean(ypwr,w=ypwr^.5),cex=1.5,"Power")
    mtext(side=1,at=0,line=2, expression(H[0]: mu == 0))
    mtext(side=1,at= xpwr[which.max(ypwr)], line=2, expression(H[a]: mu > 0))
  })

  # Show the values using an HTML table
  output$values <- renderTable({
    sliderValues()
  })
})

