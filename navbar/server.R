shinyServer(function(input, output, session) {
  ## Data Input    -----------------------------------------------------------
  
  ## 1 Categorical  -----------------------------------------------------------
  
  ## 1 Quantitative -----------------------------------------------------------
  
  ## 2 Categorical -----------------------------------------------------------
  
  ## 2 Quantitative -----------------------------------------------------------
  
  ## 1 categorical & 1 quantitative   ---------------------------------------
  
  ##  Other Tools -----------------------------------------------------------
  ## Probability plot for t & normal distributions  -------------------------
  output$probPlot <- renderPlot({ 
    par(mar=c(24,1,1,1)/10)
    z <- absz <- prob <- yrr <- xrr <- NA
    x <- -300:300 / 50
    df <- input$prb_df
    
    ## setup transparent colors
    grn <- rgb(0, 1, 0, alpha=.4)
    rd  <- rgb(1, 0, 0, alpha=.5)
    blu <- rgb(0, 0, 1, alpha=.4)

    ## convert text to numeric  
    prob <- as.numeric(input$prb_prob_txt)
    z <- as.numeric(input$prb_z_txt)
    
    ##  this loop should run when user changes prob
    if(!is.na(prob) ){
      ## given prob, find z
      if(input$prb_area == "Lower"){ ##  left tail
        z <- if(input$prb_dist=='Normal') qnorm(prob)else qt(prob, df)
        if(z < min(x))  x <- c(1.01 * z, x)
        ## rejection region in x dimension
        xrr <- c(x[x < z], z, z)
        ## denstiy curve over xrr:
        yrr <- c( if(input$prb_dist=='Normal') dnorm(xrr[-length(x)]) else 
          dt(xrr[-length(xrr)], df = df), 0)
      } else if(input$prb_area == "Upper"){   ## right tail        
        z <- if(input$prb_dist=='Normal') qnorm(1 - prob) else qt(1-prob, df)
        if(z > max(x))  x <- c(x, 1.01 * z)
        xrr <- c(z, z, x[x > z])
        yrr <- c(0, if(input$prb_dist=='Normal') dnorm(xrr[-1]) else dt(xrr[-1],df))
      } else if(input$prb_area == "Center"){
        z <- abs(if(input$prb_dist=='Normal') qnorm( (1 - prob)/2) else qt((1-prob)/2,df))
        if(z < min(x))  x <- c(1.01 * z, x)       
        if(z > max(x))  x <- c(x, 1.01 * z)
        xrr <- seq(-z, z, length=100)
        yrr <- c(0, if(input$prb_dist=='Normal') dnorm(xrr[2:99]) else dt(xrr[2:99],df), 0)
      } else if(input$prb_area == "Extremes"){
        z <- abs(if(input$prb_dist=='Normal') qnorm(1-prob/2) else qt(1-prob/2,df))
        if(z < min(x))  x <- c(1.01 * z, x)       
        if(z > max(x))  x <- c(x, 1.01 * z)
        xrr <- c(-z, x[x < -z], -z)
        yrr <- c(0, if(input$prb_dist=='Normal') dnorm(xrr[-1]) else dt(xrr[-1],df))
      }
      absz <- abs(z)
      
    }
    if(!is.na(z)){
      ##  find probability
      absz <- abs(z)
      maxX <- pmax(z, 6)
      minX <- pmin(z, -6)
      x <- seq(minX, maxX, length=200)
      prob <-  1 - if(input$prb_dist=='Normal') pnorm(z) else pt(z, df)
      xrr <- c(z, z, x[x > z])  ## right tail
      yrr <- c(0, if(input$prb_dist=='Normal') dnorm(xrr[-1]) else dt(xrr[-1], df))
      if(input$prb_area == "Lower"){         ##  left tail
        prob =  if(input$prb_dist=='Normal') pnorm(z) else pt(z, df)
        xrr <- c(z, x[x < z], z)
        yrr <- c(0, if(input$prb_dist=='Normal') dnorm(xrr[-1]) else dt(xrr[-1],df))
      } else if (input$prb_area == "Extremes"){ ##  extremes
        xrr <- c( -absz, x[x < -absz], -absz)
        yrr <- c(0, if(input$prb_dist=='Normal') dnorm(xrr[-1]) else dt(xrr[-1], df))
        prob = if(input$prb_dist=='Normal') pnorm(-absz) else pt(-absz, df)
        #yrr <- c(yrr, NA,NA, rev(yrr))
        #xrr <- c(xrr, NA,NA, rev(-xrr))
      } else if (input$prb_area == "Center"){   ##  center
        xrr <- seq(-absz, absz, length=100)
        yrr <- c(0, if(input$prb_dist=='Normal') dnorm(xrr) else dt(xrr, df), 0)
        xrr <- c(-absz, xrr, absz)
        prob <- diff(if(input$prb_dist=='Normal') pnorm(c(-absz,absz)) else pt(c(-absz,absz), df))
      }
    }
    plot(x, if(input$prb_dist=='Normal') dnorm(x) else dt(x, df), type = "l", bty='n', xlab="Z Score",
         ylab="", yaxt="n")
    abline(h=0)
    max.height <- ( if(input$prb_dist=='Normal') dnorm(0) else dt(0, df) ) *.9
    text.height <- pmin(max.height, (max(yrr) +  max.height)/2)
    segments(x0= z, y0 = 0, x1= z, y1= text.height*.95)
    
    if(input$prb_area == "Extremes") {  ## extremes
      polygon(xrr, yrr, col = rd)
      polygon(-xrr, yrr, col = rd)
      segments(x0= -z, y0 = 0, x1 = -z, y1 = text.height*.9)
      text(x= c(-absz - 4, absz + 4)/2 , y= max(yrr)/2 + .02, 
           round(prob, 3), col = "darkblue")
      place.x <- c(-absz, absz)
      if(absz < 1) place.x <- place.x/absz * .8
      text(place.x, y = text.height, round(c(-absz,absz),3))
    } else if (input$prb_area == "Center") {   ## fill & label center
      polygon(xrr, yrr, col = grn)
      text(x=0, y= text.height, round(prob,3))
      segments(x0= -z, y0 = 0, x1 = -z, y1 = text.height*.9)
      place.x <- c(-absz, absz)
      if(absz < 1) place.x <- place.x/absz * .8
      text(place.x, y=text.height, round(c(-absz,absz),3))
    } else {          ## show tails
      polygon(xrr, yrr, col = rd)
      text( z, y = text.height, round(z, 3))
      text(x= sign(z) * (absz+4) / 2 , y= max(yrr) / 2 + 0.02, 
           round(prob,3), col = "darkblue")
    }
    
  }, height=300)
  
})
  ## End of code for z-t probabilities and quantiles. ------------------------

  ##  Start of code for Power web app  ---------------------------------------
  ## Reactive expression to create a data frame containing all of the values for POWER
sliderValues <- reactive({
  # Create output
  data.frame(
    Setting = c("Sample Size", 
                "Standard Deviation",
                "Shift in Mean"),
    Value = as.character(c(input$pwr_n, 
                           input$pwr_sd,
                           input$pwr_altMean)),
    Name = c( "Significance Level (alpha)",
              "Effect Size",  "Power"
    ),
    Output =as.character(c(input$pwr_alpha, 
                           round(input$pwr_altMean/input$pwr_sd, 3),
                           round(power.t.test(n=input$pwr_n, 
                                              delta=input$pwr_altMean,
                                              sd=input$pwr_sd, 
                                              sig.level=input$pwr_alpha,
                                              type="one.sample",
                                              alternative="one")$power,
                             3))), 
    stringsAsFactors=FALSE)
  }
)
  ## set colors with transparency
 grn <- rgb(0, 1, 0, alpha=.4)
 rd <- rgb(1, 0, 0, alpha=.5)

## Plot of power to send to window
output$powerPlot <- renderPlot({
  x <- seq(-4, 10, length=200)
  plot(x, dt(x, input$pwr_n-1),bty='l', type="l", xlab="", ylab="")
  lines(x,dt(x, input$pwr_n-1, ncp=input$pwr_altMean/input$pwr_sd *sqrt(input$pwr_n)))
  abline(h=0)
  qt1 <- qt(1-input$pwr_alpha, input$pwr_n-1)
  ## rejection region in x and y (density)
  xrr <- c(qt1, qt1, x[x>=qt1], max(x))
  yrr <- c(0, dt(c(qt1,  x[x>=qt1]), input$pwr_n -1), 0)
  
  polygon(xrr, yrr, col = rd)
  abline(v = c(0, qt1))
  
  xpwr <- c(qt1, x[x>=qt1])
  ypwr <- c(0, dt(xpwr, df=input$pwr_n-1, ncp=input$pwr_altMean/input$pwr_sd *
                    sqrt(input$pwr_n) ), 0)
  xpwr <- c(qt1, xpwr, max(x))
  ## add alternative density
  polygon(xpwr, ypwr, col=grn)
  ## add alpha in center of rejection region
  text(weighted.mean(xrr, w=yrr^2), weighted.mean(yrr,w=yrr^.5), cex=1.5, 
       expression(alpha))
  ## add 'Power' in center of its region
  text(weighted.mean(xpwr, w=ypwr^4), weighted.mean(ypwr, w=ypwr^.5), cex=1.5, "Power")
  mtext(side=1, at=0, line=2, expression(H[0]: mu == 0))
  mtext(side=1, at= xpwr[which.max(ypwr)], line=2, expression(H[a]: mu > 0))
})

# Show the power values using an HTML table
output$values <- renderTable({
  sliderValues()
})    

 ## End of power code ---------------------------------------------------------

})
