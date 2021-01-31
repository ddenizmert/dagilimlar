
server <- function(input, output) {
  ## Normal Distribution
  ##Asagidaki kod sayesinde olusturdugumuz grafiklere ekleyeceğim renkleri sectik. Renderplot fonksiyonuyla ust kısımda grafik icin yazdigimiz plotoutput'un icine yazdigimiz girisi cagirarak grafikleri olustururuz.
  library(RColorBrewer)
  cols <- brewer.pal(9, "Pastel1")
  cols
  
  
  output$hist_norm <- renderPlot({
    title <- c(input$size_norm, "Orneklem Genisligi" , "Rassal Normal Degerler (Histogram)")
    hist(rnorm(input$size_norm, input$mean_norm, input$sd_norm), main = title, xlab = "Veriler", col = "#8C96C6")
  })
  
  output$box_norm <- renderPlot({
    title <- c(input$size_norm, "Orneklem Genisligi" , "Rassal Normal Degerler (Box Plot)")
    boxplot(rnorm(input$size_norm, input$mean_norm, input$sd_norm), main = title, xlab = "Veriler", col= "#35978F")
  })
  
  
  library(vioplot)
  output$violin_norm <- renderPlot({
    title <- c(input$size_norm, "Orneklem Genisligi" , "Rassal Normal Degerler (Violin Plot)")
    vioplot(rnorm(input$size_norm, input$mean_norm, input$sd_norm), main = title, xlab = "Veriler", col= "#FBB4AE")
  })
  
  library(qualityTools)
output$dot_norm <- renderPlot({ 
  title <- c(input$size_norm, "Orneklem Genisligi" , "Rassal Normal Degerler (Dot Plot)")
  dotPlot(rnorm(input$size_norm, input$mean_norm, input$sd_norm), main = title, xlab = "Veriler",col = "#4D004B")
})
  
  output$summary_norm <- renderPrint({
    summary(rnorm(input$size_norm, input$mean_norm, input$sd_norm))
  })
  
  ## Uniform Distribution
  output$hist_unif <- renderPlot({
    title <- c(input$size_unif, "Orneklem Genisligi" , "Rassal Tekduze Degerler (Histogram)")
    hist(runif(input$size_unif, input$min_unif, input$max_unif), main = title, xlab = "Veriler", col = "#8C96C6")
  })
  
  output$box_unif <- renderPlot({
    title <- c(input$size_unif, "Orneklem Genisligi" , "Rassal Tekduze Degerler (Box Plot)")
    boxplot(runif(input$size_unif, input$min_unif, input$max_unif), main = title, xlab = "Veriler", col= "#35978F")
  })
  
  output$violin_unif <- renderPlot({
    title <- c(input$size_unif, "Orneklem Genisligi" , "Rassal Tekduze Degerler (Violin Plot)")
    vioplot(runif(input$size_unif, input$min_unif, input$max_unif), main = title, xlab = "Veriler", col= "#FBB4AE")
  })
  
  output$dot_unif <- renderPlot({ 
    title <- c(input$size_unif, "Orneklem Genisligi" , "Rassal Tekduze Degerler (Dot Plot)")
    dotPlot(runif(input$size_unif, input$min_unif, input$max_unif), main = title, xlab = "Veriler",col = "#4D004B")
  })
  
  
  output$summary_unif <- renderPrint({
    summary(runif(input$size_unif, input$min_unif, input$max_unif))
  })
  
  ## T Distribution
  output$hist_t <- renderPlot({
    title <- c(input$size_t, "Orneklem Genisligi" , "Rassal T Degerler (Histogram)")
    hist(rt(input$size_t, input$df_t), main = title, xlab = "Veriler", col = "#8C96C6")
  })
  
  output$box_t <- renderPlot({
    title <- c(input$size_t, "Orneklem Genisligi" , "Rassal T Degerler (Box Plot)")
    boxplot(rt(input$size_t, input$df_t), main = title, xlab = "Veriler", col= "#35978F")
  })
  
  output$violin_t <- renderPlot({
    title <- c(input$size_t, "Orneklem Genisligi" , "Rassal T Degerler (Violin Plot)", col= "#FBB4AE")
    vioplot(rt(input$size_t, input$df_t), main = title, xlab = "Veriler")
  })
  
  
  output$dot_t <- renderPlot({ 
    title <- c(input$size_t, "Orneklem Genisligi" , "Rassal T Degerler (Dot Plot)")
    dotPlot(rt(input$size_t, input$df_t), main = title, xlab = "Veriler",col = "#4D004B")
  })
  
  
  
  output$summary_t <- renderPrint({
    summary(rt(input$size_t, input$df_t))
  })
  
  ## F Distribution
  output$hist_f <- renderPlot({
    title <- c(input$size_f, "Orneklem Genisligi" , "Rassal F Degerler (Histogram)")
    hist(rf(input$size_f, input$df_f_1, input$df_f_2), main = title, xlab = "Veriler", col = "#8C96C6")
  })
  
  output$box_f <- renderPlot({
    title <- c(input$size_f, "Orneklem Genisligi" , "Rassal F Degerler (Box Plot)")
    boxplot(rf(input$size_f, input$df_f_1, input$df_f_2), main = title, xlab = "Veriler", col= "#35978F")
  })
  output$violin_f <- renderPlot({
    title <- c(input$size_f, "Orneklem Genisligi" , "Rassal F Degerler (Violin Plot)")
    vioplot(rf(input$size_f, input$df_f_1,input$df_f_2), main = title, xlab = "Veriler", col= "#FBB4AE")
  })
  
  
  output$dot_f <- renderPlot({ 
    title <- c(input$size_f, "Orneklem Genisligi" , "Rassal F Degerler (Dot Plot)")
    dotPlot(rf(input$size_f, input$df_f_1,input$df_f_2), main = title, xlab = "Veriler",col = "#4D004B")
  })
  
  
  output$summary_f <- renderPrint({
    summary(rf(input$size_f, input$df_f_1,input$df_f_2))
  })
  
  ## Gamma Distribution
  output$hist_gamma <- renderPlot({
    title <- c(input$size_gamma, "Orneklem Genisligi" , "Rassal Gamma Degerler (Histogram)")
    hist(rgamma(input$size_gamma, input$shape_gamma, input$rate_gamma), main = title, xlab = "Veriler", col = "#8C96C6")
  })
  
  output$box_gamma <- renderPlot({
    title <- c(input$size_gamma, "Orneklem Genisligi" , "Rassal Gamma Degerler (Box Plot)")
    boxplot(rgamma(input$size_gamma, input$shape_gamma, input$rate_gamma), main = title, xlab = "Veriler", col= "#35978F")
  })
  output$violin_gamma <- renderPlot({
    title <- c(input$size_gamma, "Orneklem Genisligi" , "Rassal Gamma Degerler (Violin Plot)")
    vioplot(rgamma(input$size_gamma, input$shape_gamma, input$rate_gamma), main = title, xlab = "Veriler", col= "#FBB4AE")
  })
  
  
  output$dot_gamma <- renderPlot({ 
    title <- c(input$size_gamma, "Orneklem Genisligi" , "Rassal Gamma Degerler (Dot Plot)")
    dotPlot(rgamma(input$size_gamma, input$shape_gamma, input$rate_gamma), main = title, xlab = "Veriler",col = "#4D004B")
  })
  
  
  output$summary_gamma <- renderPrint({
    summary(rgamma(input$size_gamma, input$shape_gamma, input$rate_gamma))
  })
  
  ## Exponential Distribution
  output$hist_exp <- renderPlot({
    title <- c(input$size_exp, "Orneklem Genisligi" , "Rassal Ustel Degerler (Histogram)")
    hist(rexp(input$size_exp, input$rate_exp), main = title, xlab = "Veriler", col = "#8C96C6")
  })
  
  output$box_exp <- renderPlot({
    title <- c(input$size_exp, "Orneklem Genisligi" , "Rassal Ustel Degerler (Box Plot)")
    boxplot(rexp(input$size_exp, input$rate_exp), main = title, xlab = "Veriler", col= "#35978F")
  })
  output$violin_exp <- renderPlot({
    title <- c(input$size_exp, "Orneklem Genisligi" , "Rassal Ustel Degerler (Violin Plot)")
    vioplot(rexp(input$size_exp, input$rate_exp), main = title, xlab = "Veriler", col= "#FBB4AE")
  })
  
  
  output$dot_exp <- renderPlot({ 
    title <- c(input$size_exp, "Orneklem Genisligi" , "Rassal Ustel Degerler (Dot Plot)")
    dotPlot(rexp(input$size_exp, input$rate_exp), main = title, xlab = "Veriler",col = "#4D004B")
  })
  
  
  output$summary_exp <- renderPrint({
    summary(rexp(input$size_exp, input$rate_exp))
  })
  ## Chi - Square Distribution
  output$hist_chi <- renderPlot({
    title <- c(input$size_chi, "Orneklem Genisligi" , "Rassal Ki - Kare Degerler (Histogram)")
    hist(rchisq(input$size_chi, input$df_chi), main = title, xlab = "Veriler", col = "#8C96C6")
  })
  
  output$box_chi <- renderPlot({
    title <- c(input$size_chi, "Orneklem Genisligi" , "Rassal Ki - Kare Degerler (Box Plot)")
    boxplot(rchisq(input$size_chi, input$df_chi), main = title, xlab = "Veriler", col= "#35978F")
  })
  output$violin_chi <- renderPlot({
    title <- c(input$size_chi, "Orneklem Genisligi" , "Rassal Ki - Kare Degerler (Violin Plot)")
    vioplot(rchisq(input$size_chi, input$df_chi), main = title, xlab = "Veriler", col= "#FBB4AE")
  })
  
  
  output$dot_chi <- renderPlot({ 
    title <- c(input$size_chi, "Orneklem Genisligi" , "Rassal Ki - Degerler (Dot Plot)")
    dotPlot(rchisq(input$size_chi, input$df_chi), main = title, xlab = "Veriler",col = "#4D004B")
  })
  
  output$summary_chi <- renderPrint({
    summary(rchisq(input$size_chi, input$df_chi))
  })
  ## Log Normal Distribution
  output$hist_log <- renderPlot({
    title <- c(input$size_log, "Orneklem Genisligi" , "Rassal Log Normal Degerler (Histogram)")
    hist(rlnorm(input$size_log, input$mean_log, input$sd_log), main = title, xlab = "Veriler", col = "#8C96C6")
  })
  
  output$box_log <- renderPlot({
    title <- c(input$size_log, "Orneklem Genisligi" , "Rassal Log Normal Degerler (Box Plot)")
    boxplot(rlnorm(input$size_log, input$mean_log, input$sd_log), main = title, xlab = "Veriler", col= "#35978F")
  })
  output$violin_log <- renderPlot({
    title <- c(input$size_log, "Orneklem Genisligi" , "Rassal Log Normal Degerler (Violin Plot)")
    vioplot(rlnorm(input$size_log, input$mean_log, input$sd_log), main = title, xlab = "Veriler", col= "#FBB4AE")
  })
  
  
  output$dot_log <- renderPlot({ 
    title <- c(input$size_log, "Orneklem Genisligi" , "Rassal Log Degerler (Dot Plot)")
    dotPlot(rlnorm(input$size_log, input$mean_log, input$sd_log), main = title, xlab = "Veriler",col = "#4D004B")
  })
  
  output$summary_log <- renderPrint({
    summary(rlnorm(input$size_log, input$mean_log, input$sd_log))
  })
  
  ## Beta Distribution
  output$hist_beta <- renderPlot({
    title <- c(input$size_beta, "Orneklem Genisligi" , "Rassal Beta Degerler (Histogram)")
    hist(rbeta(input$size_beta, input$shape_beta_1, input$shape_beta_2), main = title, xlab = "Veriler", col = "#8C96C6")
  })
  
  output$box_beta <- renderPlot({
    title <- c(input$size_beta, "Orneklem Genisligi" , "Rassal Beta Degerler (Box Plot)")
    boxplot(rbeta(input$size_beta, input$shape_beta_1, input$shape_beta_2), main = title, xlab = "Veriler", col= "#35978F")
  })
  output$violin_beta <- renderPlot({
    title <- c(input$size_exp, "Orneklem Genisligi" , "Rassal Beta Degerler (Violin Plot)")
    vioplot(rbeta(input$size_beta, input$shape_beta_1, input$shape_beta_2), main = title, xlab = "Veriler", col= "#FBB4AE")
  })
  
  output$dot_beta <- renderPlot({ 
    title <- c(input$size_beta, "Orneklem Genisligi" , "Rassal Beta Degerler (Dot Plot)")
    dotPlot(rbeta(input$size_beta, input$shape_beta_1, input$shape_beta_2), main = title, xlab = "Veriler",col = "#4D004B")
  })
  
  output$summary_beta <- renderPrint({
    summary(rbeta(input$size_beta, input$shape_beta_1, input$shape_beta_2))
  })
  
  ## Cauchy Distribution
  output$hist_cauchy <- renderPlot({
    title <- c(input$size_cauchy, "Orneklem Genisligi" , "Rassal Cauchy Degerler (Histogram)")
    hist(rcauchy(input$size_cauchy, input$location_cauchy, input$scale_cauchy), main = title, xlab = "Veriler", col = "#8C96C6")
  })
  
  output$box_cauchy <- renderPlot({
    title <- c(input$size_cauchy, "Orneklem Genisligi" , "Rassal Cauchy Degerler (Box Plot)")
    boxplot(rcauchy(input$size_cauchy, input$location_cauchy, input$scale_cauchy), main = title, xlab = "Veriler", col= "#35978F")
  })
  
  output$violin_cauchy <- renderPlot({
    title <- c(input$size_cauchy, "Orneklem Genisligi" , "Rassal Cauchy Degerler (Violin Plot)")
    vioplot(rcauchy(input$size_cauchy, input$location_cauchy, input$scale_cauchy), main = title, xlab = "Veriler", col= "#FBB4AE")
  })
  
  output$dot_cauchy <- renderPlot({ 
    title <- c(input$size_cauchy, "Orneklem Genisligi" , "Rassal Cauchy Degerler (Dot Plot)")
    dotPlot(rcauchy(input$size_cauchy, input$location_cauchy, input$scale_cauchy), main = title, xlab = "Veriler",col = "#4D004B")
  })
  
  output$summary_cauchy <- renderPrint({
    summary(rcauchy(input$size_cauchy, input$location_cauchy, input$scale_cauchy))
  })
  
  ## Weibull Distribution
  output$hist_weibull <- renderPlot({
    title <- c(input$size_weibull, "Orneklem Genisligi" , "Rassal Weibull Degerler (Histogram)")
    hist(rweibull(input$size_weibull, input$shape_weibull, input$scale_weibull), main = title, xlab = "Veriler", col = "#8C96C6")
  })
  
  output$box_weibull <- renderPlot({
    title <- c(input$size_weibull, "Orneklem Genisligi" , "Rassal Weibull Degerler (Box Plot)")
    boxplot(rweibull(input$size_weibull, input$shape_weibull, input$scale_weibull), main = title, xlab = "Veriler", col= "#35978F")
  })
  
  output$violin_weibull <- renderPlot({
    title <- c(input$size_cauchy, "Orneklem Genisligi" , "Rassal Weibull Degerler (Violin Plot)")
    vioplot(rweibull(input$size_weibull, input$shape_weibull, input$scale_weibull), main = title, xlab = "Veriler", col= "#FBB4AE")
  })
  output$dot_weibull <- renderPlot({ 
    title <- c(input$size_cauchy, "Orneklem Genisligi" , "Rassal Cauchy Degerler (Dot Plot)")
    dotPlot(rweibull(input$size_weibull, input$shape_weibull, input$scale_weibull), main = title, xlab = "Veriler",col = "#4D004B")
  })
  
  output$summary_weibull <- renderPrint({
    summary(rweibull(input$size_weibull, input$shape_weibull, input$scale_weibull))
  })
  
  
  
  
}
