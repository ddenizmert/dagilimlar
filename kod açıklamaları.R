## KODLARIN AÇIKLAMALARI


## ui Bölümü:

library(shiny) ## shiny paketi yüklenmektedir.

library(shinythemes) ## shinytehemes paketi yüklenmektedir.
ui <- shinyUI(fluidPage(theme=shinytheme("flatly"), ## theme argümaný kullaýlarak arayüzün temasý ayarlanmaktadýr.
                        headerPanel("SUREKLI OLASILIK DAGILIMLARI"), ## headerpanel ile baþlýk belirlenmiþtir.
                        fluidRow( ## Sayfa düzeni ayarlanmýþtýr.
                          column(4, ## ilk kolonun geniþliði 4 olarak belirlenmiþtir.
                                 wellPanel( ## Sol taraftaki panel için düzenlemeler yapýlmýþtýr.
                                   
                                   ## radiobuttons ile tekli seçim kutularý eklenmiþtir. Burada daðýlýmlar belirlenmiþtir.
                                   radioButtons("dist","DAGILIMLAR:",
                                                list("Normal Dagilim"="norm",
                                                     "Tekduze Dagilim"="unif",
                                                     "t Dagilimi"="t_dist",
                                                     "F Dagilimi"="F_dist",
                                                     "Gamma Dagilimi"="gam",
                                                     "Ustel Dagilim"="exp",
                                                     "Ki-Kare Dagilimi"="chisq",
                                                     "Log-normal Dagilim"="lnorm",
                                                     "Beta Dagilimi"="beta",
                                                     "Cauchy Dagilimi"="cauchy",
                                                     "Weibull Dagilimi"="weibull")),
                                   textInput("n", "Orneklem Buyuklugu:", value = 25), ## Örneklem geniþliði için metin kutusu eklenmiþtir.
                                   uiOutput("dist1"), ## Daðýlýmlarýn 1. parametre deðerleri için çýktý tanýmlanýr.
                                   uiOutput("dist2"), ## Daðýlýmlarýn 2. parametre deðerleri için çýktý tanýmlanýr.
                                   checkboxInput("density", "Frekans (Yogunluk) Egrisini Goster", FALSE), ## Eðri eklemek için çoklu seçim kutusu tanýmlanmýþtýr.
                                   
                                   ## Koþullu panel ile frekans yoðunluk eðrisinin bant geniþliði tanýmlanmýþtýr.
                                   conditionalPanel(
                                     condition="input.density==true",
                                     numericInput("bw","Bant Genisligi:", 1)
                                   )
                                 )
                          ),
                          column(8, ## ikinci kolonun geniþliði 8 olarak belirlenmiþtir.
                                 tabsetPanel( ## Sað taraftaki panel için düzenlemeler yapýlmýþtýr.
                                   
                                   ## Panele 6 Farklý sekme eklenmiþtir. Height argümaný bu sekmelerdeki çýktýlarýn yüksekliðini belirtir.
                                   ## Grafikler için plotOutput kullanýlýr.
                                   ## Özet istatistikler için verbatimTextOutput, veriler için tableOutput kullanýlmýþtýr.
                                   tabPanel("Histogram", plotOutput("hist", height="600px")),
                                   tabPanel("Box Plot", plotOutput("boxplot", height="600px")),
                                   tabPanel("Violin Plot", plotOutput("vioplot", height="600px")),
                                   tabPanel("Dot Plot", plotOutput("dotplot", height="600px")),
                                   tabPanel("Summary", verbatimTextOutput("summary")),
                                   tabPanel("Table", tableOutput("table"))
                                   
                                 )
                          )
                        )
))

#Daðýlým Parametreleri;

#Normal Daðýlým: rnorm(n, mean = 0, sd = 1)
#Gama Daðýlýmý: rgamma(n, shape, rate = 1, scale = 1/rate)
#Tekdüze Daðýlým: runif(n, min = 0, max = 1)
#Ustel Daðýlým: rexp(n, rate = 1)
#Cauchy Daðýlýmý : rcauchy(n, location = 0, scale = 1)
#Weibull Daðýlýmý : rweibull(n, shape, scale = 1)
#Beta Daðýlýmý : rbeta(n, shape1, shape2, ncp = 0)
#Lognormal Daðýlýmý : rlnorm(n, meanlog = 0, sdlog = 1)
#T Daðýlýmý: rt(n, df)
#Ki Kare Daðýlýmý: rchisq(n, df)
#F Daðýlýmý: rf(n, df1, df2)


#Daðýlýmlarýn eksik parametreleri eklendi:
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


## Server Bölümü:

server <- shinyServer(function(input, output){
  dat <- reactive({
    ## Daðýlýmlar için R da ki fonksiyonlar kullanýlmýþtýr.
    ## Eksik parametreleri içeren fonksiyonlar deðiþtirilmiþtir.
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
    
    ## Daðýlýmlarýn tümü için parametreler çaðýrýlmýþtýr.
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
  
  output$dist1 <- renderUI({ ##ui daki outputlarýn çýktýsýný almak için renderUI fonksiyonu kullanýlýr.
    input$dist
    isolate({
      ## Daðýlýmlarýn ilk parametre deðerlerinin atamasý yapýlmaktadýr.
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
      
      ## Daðýlýmlarýn ilk parametre deðerleri için varsayýlan deðerler girilmektedir.
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
  
  output$dist2 <- renderUI({ ##ui daki outputlarýn çýktýsýný almak için renderUI fonksiyonu kullanýlýr.
    input$dist
    isolate({
      ## Daðýlýmlarýn ikinci parametre deðerlerinin atamasý yapýlmaktadýr.
      lab <- switch(input$dist,
                    norm = "Standart Sapma:",
                    unif = "Maksimum:",
                    F_dist = "Alt Serbestlik Derecesi:", 
                    gam = "Oran:", 
                    lnorm = "Standart Sapma(log):", 
                    beta = "Beta:",
                    cauchy = "Olcek:",
                    weibull = "Sekil:")
      
      ## Daðýlýmlarýn ikinci parametre deðerleri için varsayýlan deðerler girilmektedir.
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
  
  
  
  ## Histogram grafiðini çizdirmek için renderPlot içerisinde hist fonksiyonu kullanýlmýþtýr.
  output$hist <- renderPlot({
    dist <- input$dist
    n <- input$n
    hist(dat()[[1]], main = "HISTOGRAM GRAFIGI", xlab = "GOZLEMLER",
         ylab = "FREKANS", col = "lightseagreen",
         cex.axis = 1.2, cex.lab = 1.2, prob = T)
    if(input$density) lines(density(dat()[[1]], adjust = input$bw), lwd = 2)
  })
  
  ## Boxplot grafiðini çizdirmek için renderPlot içerisinde boxplot fonksiyonu kullanýlmýþtýr.
  output$boxplot <- renderPlot({
    dist <- input$dist
    n <- input$n
    boxplot(dat()[[1]],main = "KUTU GRAFIGI", ylab = "FREKANS", xlab = "GOZLEMLER", col = "palevioletred3")
    if(input$density) lines(density(dat()[[1]], adjust = input$bw), lwd = 2)
  }) 
  
  library(vioplot) ## violinPlot için vioplat paketi yüklenmiþtir.
  
  ## Violin Plot grafiðini çizdirmek için renderPlot içerisinde vioplot fonksiyonu kullanýlmýþtýr.
  output$vioplot <- renderPlot({
    dist <- input$dist
    n <- input$n
    vioplot(dat()[[1]], main = "VIOLIN GRAFIGI", ylab = "FREKANS", xlab = "GOZLEMLER", col = "salmon2")
    if(input$density) lines(density(dat()[[1]], adjust = input$bw), lwd = 2)
  }) 
  
  
  library(qualityTools) ## dotPlot için qualityTools paketi yüklenmiþtir.
  
  ## Nokta Grafiðini çizdirmek için renderPlot içerisinde dotPlot fonksiyonu kullanýlmýþtýr.
  output$dotplot <- renderPlot({
    dist <- input$dist
    n <- input$n
    dotPlot(dat()[[1]], main = "DOT PLOT", ylab = "FREKANS", xlab = "GOZLEMLER", col = "green")
    if(input$density) lines(density(dat()[[1]], adjust = input$bw), lwd = 2)
  }) 
  
  ## Özet istatistikleri görmek için renderPrint kullanýlýr.
  output$summary <- renderPrint({
    summary(dat()[[1]])
  })
  
  ## Rassal olarak üretilen verileri görmek için renderTable kullanýlýr.
  output$table <- renderTable({
    data.frame(x = dat()[[1]])
  })
  
  
  
})

shinyApp(ui, server)