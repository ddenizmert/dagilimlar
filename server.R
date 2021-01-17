library(vioplot)

library(shiny)

rt2 <- function(n = 1000, dft = 15) {
  rt(n = n, df = dft) 
}

formals(rgamma)[1:2] <- c(1000, 1)

rchisq2 <- function(n = 1000, dfx = 1) {
  rchisq(n = n, df = dfx)
}

formals(rf)[1:3] <- c(1000, 1, 15)

rexp2 <- function(n = 1000, rate2 = 1) {
  rexp(n = n, rate = rate2)
}

formals(rbeta)[1:3] <- c(1000, 2, 2)

rcauchy2<-function(n = 1000, location2 = 0, scale2 = 1) {
  rcauchy(n = n, location = location2, scale = scale2)
}

rweibull2<-function(n = 1000, shape2 = 1, scale2 = 1) {
  rweibull(n = n, shape = shape2, scale = scale2)
}


server <- shinyServer(function(input, output){
  dat <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   t_dist = rt2,
                   F_dist = rf,
                   gam = rgamma,
                   exp = rexp2,
                   chisq = rchisq2,
                   lnorm = rlnorm,
                   beta = rbeta,
                   cauchy = rcauchy2,
                   weibull = rweibull2)
    
    def.args <- switch(input$dist,
                       norm = c(input$mean, input$sd),
                       unif = c(input$min, input$max),
                       t_dist = c(input$dft),
                       F_dist = c(input$df1, input$df2),
                       gam = c(input$shape, input$rate),
                       exp = c(input$rate2),
                       chisq = c(input$dfx),
                       lnorm = c(input$meanlog, input$sdlog),
                       beta = c(input$shape1, input$shape2),
                       cauchy = c(input$location2, input$scale2),
                       weibull=c(input$shape2, input$scale2))
    
    f <- formals(dist);	f <- f[names(f)!="n"]; len <- min(length(f),3-1); f <- f[1:len]
    argList <- list(n = input$n)
    for(i in 1:len) argList[[names(f)[i]]] <- def.args[i]
    return(list(do.call(dist, argList), names(f)))
  })
  
  output$dist1 <- renderUI({
    input$dist
    isolate({
      lab <- switch(input$dist,
                    norm = "Ortalama:",
                    unif = "Minimum:",
                    t_dist = "Serbestlik Derecesi:",
                    F_dist = "Ust Serbestlik Derecesi:",
                    gam = "Sekil:",
                    exp = "Oran:",
                    chisq = "Serbestlik Derecesi:",
                    lnorm = "Ortalama(log):",
                    beta = "Alpha:",
                    cauchy = "Konum" ,
                    weibull = "Olcek")
      
      ini <- switch(input$dist,
                    norm = 0,
                    unif = 0, 
                    t_dist = 15, 
                    F_dist = 1, 
                    gam = 1, 
                    exp = 1, 
                    chisq = 1, 
                    lnorm = 0, 
                    beta = 2,
                    cauchy = 0, 
                    weibull = 1)
      numericInput(dat()[[2]][1], lab, ini)
    })
  })
  
  output$dist2 <- renderUI({
    input$dist
    isolate({
      lab <- switch(input$dist,
                    norm = "Standart Sapma:",
                    unif = "Maksimum:",
                    F_dist = "Alt Serbestlik Derecesi:", 
                    gam = "Oran:", 
                    lnorm = "Standart Sapma(log):", 
                    beta = "Beta:",
                    cauchy = "Olcek:",
                    weibull = "Sekil:")
      ini <- switch(input$dist,
                    norm = 1,
                    unif = 1,
                    F_dist = 15, 
                    gam = 1, 
                    lnorm = 1, 
                    beta = 2,
                    cauchy = 1,
                    weibull = 1)
      if(any(input$dist==c("norm", "unif", "F_dist", "gam", "lnorm", "beta", "cauchy", "weibull")))numericInput(dat()[[2]][2], lab, ini)
    })
  })
  
  
  
  
  output$hist <- renderPlot({
    dist <- input$dist
    n <- input$n
    hist(dat()[[1]], main = "HISTOGRAM GRAFIGI", xlab = "GOZLEMLER",
         ylab = "FREKANS", col = "lightseagreen",
         cex.axis = 1.2, cex.lab = 1.2, prob = T)
    if(input$density) lines(density(dat()[[1]], adjust = input$bw), lwd = 2)
  })
  
  
  output$boxplot <- renderPlot({
    dist <- input$dist
    n <- input$n
    boxplot(dat()[[1]],main = "KUTU GRAFIGI", ylab = "FREKANS", xlab = "GOZLEMLER", col = "palevioletred3")
    if(input$density) lines(density(dat()[[1]], adjust = input$bw), lwd = 2)
  }) 
  
  library(vioplot)
  
  output$vioplot <- renderPlot({
    dist <- input$dist
    n <- input$n
    vioplot(dat()[[1]], main = "VIOLIN GRAFIGI", ylab = "FREKANS", xlab = "GOZLEMLER", col = "salmon2")
    if(input$density) lines(density(dat()[[1]], adjust = input$bw), lwd = 2)
  }) 
  
  
  library(qualityTools)
  output$dotplot <- renderPlot({
    dist <- input$dist
    n <- input$n
    dotPlot(dat()[[1]], main = "DOT PLOT", ylab = "FREKANS", xlab = "GOZLEMLER", col = "green")
    if(input$density) lines(density(dat()[[1]], adjust = input$bw), lwd = 2)
  }) 
  
  
  output$summary <- renderPrint({
    summary(dat()[[1]])
  })
  
  output$table <- renderTable({
    data.frame(x = dat()[[1]])
  })
  
  
  
})
