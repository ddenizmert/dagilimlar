library(shiny)

library(shinythemes)
ui <- shinyUI(fluidPage(theme=shinytheme("flatly"),
                        headerPanel("SUREKLI OLASILIK DAGILIMLARI"),
                        fluidRow(
                          column(4,
                                 wellPanel(
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
                                   textInput("n", "Orneklem Buyuklugu:", value = 25),
                                   uiOutput("dist1"),
                                   uiOutput("dist2"),
                                   checkboxInput("density", "Frekans (Yogunluk) Egrisini Goster", FALSE),
                                   conditionalPanel(
                                     condition="input.density==true",
                                     numericInput("bw","Bant Genisligi:", 1)
                                   )
                                 )
                          ),
                          column(8,
                                 tabsetPanel(
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
