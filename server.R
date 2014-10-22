library(shiny)
library(dplyr)

library(survival)
library(lubridate)
regjering <- read.csv("regjering.csv", stringsAsFactors = FALSE) %>%
  tbl_df %>%
  mutate_each(funs(as.Date), ends_with("dato")) %>%
  mutate(Sluttdato = replace(Sluttdato, is.na(Sluttdato), today("Europe/Oslo")),
         Dager = as.numeric(Sluttdato - Startdato),
         År = decimal_date(Sluttdato) - decimal_date(Startdato))

shinyServer(function(input, output) {
  data1 <- reactive({
    i <- which(regjering$Regjering == input$valgtregjering)
    if (length(i) == 0) {
      data <- regjering
    } else {
      data <- regjering[i, ]
    }
    return(data)
  })
  output$tabell <- renderDataTable({
    data1()
  })
  output$plot <- renderPlot({
    data <- data1()
    makstid <- input$zoom
    formel  <- as.formula(paste0("Surv(",
                                 input$dager_år,
                                 ", Avskjed) ~ 1"))
    plot(survfit(formel, data = data),
         mark.time = input$merker,
         xlab = input$dager_år,
         main = input$valgtregjering,
         xmax = makstid)
  })
  data2 <- reactive({
    i <- which(regjering$Regjering %in% input$valgteregjeringer)
    data <- regjering[i, ]
    return(data)
  })
  output$plot2 <- renderPlot({
    n <- length(input$valgteregjeringer)
    formel  <- as.formula(paste0("Surv(",
                                 input$dager_år,
                                 ", Avskjed) ~ Regjering"))
    data <- data2()
    makstid <- input$zoom
    plot(survfit(formel, data = data),
         col = 1:n,
         conf.int = FALSE,
         mark.time = input$merker,
         xlab = input$dager_år,
         main = paste(sort(input$valgteregjeringer), collapse = ", "),
         xmax = makstid)
    legend("bottomleft", legend = sort(input$valgteregjeringer),
           col = 1:n, lty = 1, bty = "n")
  })
  output$zoom <- renderUI({
    if (input$sammenlign) {
      data <- data2()
    } else {
      data <- data1()
    }
    makstid  <- max(data[, input$dager_år])
    sliderInput(inputId = "zoom",
                label = "Zoom",
                min = 0,
                max = makstid,
                value = makstid,
                locale = "se")
  })
})
