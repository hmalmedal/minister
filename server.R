library(shiny)

library(survival)
library(lubridate)
regjering <- read.csv("regjering.csv",
                      colClasses=c("character",
                                   "Date",
                                   "Date",
                                   "integer",
                                   "factor"))
regjering[is.na(regjering)] <- today()
regjering$Dager <- as.numeric(regjering$Sluttdato - regjering$Startdato)
regjering$År <- decimal_date(regjering$Sluttdato) -
    decimal_date(regjering$Startdato)

shinyServer(function(input, output) {
    output$tabell <- renderDataTable({
        i <- which(regjering$Regjering == input$valgtregjering)
        if (length(i) == 0) {
            data <- regjering
        } else {
            data <- regjering[i, ]
        }
        return(data)
    })
    output$plot <- renderPlot({
        i <- which(regjering$Regjering == input$valgtregjering)
        if (length(i) == 0) {
            data <- regjering
        } else {
            data <- regjering[i, ]
        }
        formel  <- as.formula(paste("Surv(",
                                    input$dager_år,
                                    ", Avskjed) ~ 1",
                                    sep = ""))
        plot(survfit(formel, data = data),
             mark.time = input$merker,
             xlab = input$dager_år,
             main = input$valgtregjering)
    })
    output$plot2<- renderPlot({
        i <- which(regjering$Regjering %in% input$valgteregjeringer)
        n <- length(input$valgteregjeringer)
        formel  <- as.formula(paste("Surv(",
                                    input$dager_år,
                                    ", Avskjed) ~ Regjering",
                                    sep = ""))
        data <- regjering[i, ]
        plot(survfit(formel, data = data),
             col = 1:n,
             conf.int = F,
             mark.time = input$merker,
             xlab = input$dager_år,
             main = paste(sort(input$valgteregjeringer), collapse = ", "))
        legend("bottomleft", legend = sort(input$valgteregjeringer),
               col = 1:n, lty=1, bty = "n")
    })
})
