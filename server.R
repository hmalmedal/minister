library(shiny)

library(survival)
library(lubridate)
regjering <- read.csv("regjering.csv",
                      colClasses = c("character",
                                     "Date",
                                     "Date",
                                     "integer",
                                     "factor"))
regjering[is.na(regjering)] <- today()
regjering$Dager <- as.numeric(regjering$Sluttdato - regjering$Startdato)
regjering$År <- decimal_date(regjering$Sluttdato) -
    decimal_date(regjering$Startdato)

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
             conf.int = F,
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
