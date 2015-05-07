library(shiny)
# library(dichromat)
# library(scales)
# library(ggplot2)


shinyServer(function(input, output) {
#  
# input <- list(df = 10, area="Lower",dist='Normal',z.txt="1.2", prob.txt=NA)
  output$plot <- renderPlot({ 
    par(mar=c(24,1,1,1)/10)
    z <- absz <- prob <- yrr <- xrr <- NA
    x <- -300:300 / 50
    df <- input$df
    grn <- rgb(0, 1, 0, alpha=.4)
    rd  <- rgb(1, 0, 0, alpha=.5)
    blu <- rgb(0, 0, 1, alpha=.4)
    prob <- as.numeric(input$prob.txt)
    z <- as.numeric(input$z.txt)
    if(!is.na(prob) ){
      ## given prob, find z
      if(input$area == "Lower"){ ##  left tail
        z <- if(input$dist=='Normal') qnorm(prob)else qt(prob, df)
        if(z < min(x))  x <- c(1.01 * z, x)
        xrr <- c(x[x < z], z, z)
        yrr <- c( if(input$dist=='Normal') dnorm(xrr[-length(x)]) else 
                         dt(xrr[-length(xrr)], df = df),0)
      } else if(input$area == "Upper"){   ## right tail        
        z <- if(input$dist=='Normal') qnorm(1 - prob) else qt(1-prob, df)
        if(z > max(x))  x <- c(x, 1.01 * z)
        xrr <- c(z, z, x[x > z])
        yrr <- c(0, if(input$dist=='Normal') dnorm(xrr[-1]) else dt(xrr[-1],df))
      } else if(input$area == "Center"){
        z <- abs(if(input$dist=='Normal') qnorm( (1 - prob)/2) else qt((1-prob)/2,df))
        if(z < min(x))  x <- c(1.01 * z, x)       
        if(z > max(x))  x <- c(x, 1.01 * z)
        xrr <- seq(-z, z, length=100)
        yrr <- c(0, if(input$dist=='Normal') dnorm(xrr[2:99]) else dt(xrr[2:99],df), 0)
      } else if(input$area == "Extremes"){
        z <- abs(if(input$dist=='Normal') qnorm(1-prob/2) else qt(1-prob/2,df))
        if(z < min(x))  x <- c(1.01 * z, x)       
        if(z > max(x))  x <- c(x, 1.01 * z)
        xrr <- c(-z, x[x < -z], -z)
        yrr <- c(0, if(input$dist=='Normal') dnorm(xrr[-1]) else dt(xrr[-1],df))
      }
      absz <- abs(z)
      
    }
    if(!is.na(z)){
      ##  find probability
      absz <- abs(z)
      maxX <- pmax(z,6)
      minX <- pmin(z,-6)
      x <- seq(minX,maxX, length=200)
      prob <-  1 - if(input$dist=='Normal') pnorm(z) else pt(z,df)
      xrr <- c(z, z, x[x > z])  ## right tail
      yrr <- c(0, if(input$dist=='Normal') dnorm(xrr[-1]) else dt(xrr[-1], df))
      if(input$area == "Lower"){         ##  left tail
        prob =  if(input$dist=='Normal') pnorm(z) else pt(z, df)
        xrr <- c(z, x[x < z], z)
        yrr <- c(0, if(input$dist=='Normal') dnorm(xrr[-1]) else dt(xrr[-1],df))
      } else if (input$area == "Extremes"){ ##  extremes
        xrr <- c( -absz, x[x < -absz], -absz)
        yrr <- c(0, if(input$dist=='Normal') dnorm(xrr[-1]) else dt(xrr[-1], df))
        prob = if(input$dist=='Normal') pnorm(-absz) else pt(-absz, df)
        #yrr <- c(yrr, NA,NA, rev(yrr))
        #xrr <- c(xrr, NA,NA, rev(-xrr))
      } else if (input$area == "Center"){   ##  center
        xrr <- seq(-absz, absz, length=100)
        yrr <- c(0, if(input$dist=='Normal') dnorm(xrr) else dt(xrr, df), 0)
        xrr <- c(-absz, xrr, absz)
        prob <- diff(if(input$dist=='Normal') pnorm(c(-absz,absz)) else pt(c(-absz,absz), df))
      }
    }
    plot(x, if(input$dist=='Normal') dnorm(x) else dt(x, df), type = "l", bty='n', xlab="Z Score",
         ylab="",yaxt="n")
    abline(h=0)
    max.height <- ( if(input$dist=='Normal') dnorm(0) else dt(0, df) ) *.9
    text.height <- pmin(max.height, (max(yrr) +  max.height)/2)
    segments(x0= z, y0 = 0, x1= z, y1= text.height*.95)
    
    if(input$area == "Extremes") {  ## extremes
      polygon(xrr, yrr, col = rd)
      polygon(-xrr, yrr, col = rd)
      segments(x0= -z, y0 = 0, x1 = -z, y1 = text.height*.9)
      text(x= c(-absz - 4, absz + 4)/2 , y= max(yrr)/2 + .02, 
           round(prob, 3), col = "darkblue")
      place.x <- c(-absz, absz)
      if(absz < 1) place.x <- place.x/absz * .8
      text(place.x, y = text.height, round(c(-absz,absz),3))
    } else if (input$area == "Center"){   ## fill & label center
       polygon(xrr, yrr, col = grn)
       text(x=0, y= text.height, round(prob,3))
       segments(x0= -z, y0 = 0, x1 = -z, y1 = text.height*.9)
       place.x <- c(-absz, absz)
       if(absz < 1) place.x <- place.x/absz * .8
       text(place.x, y = text.height, round(c(-absz,absz),3))
    } else {          ## show tails
      polygon(xrr, yrr, col = rd)
      text( z, y = text.height, round(z, 3))
      text(x= sign(z)*(absz+4)/2 , y= max(yrr)/2+.02, 
           round(prob,3), col = "darkblue")
    }
    
  }, height=300)
  
})