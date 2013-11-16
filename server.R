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
        if (input$dager_år == "Dager") {
            plot(survfit(Surv(Dager, Avskjed) ~ 1, data = data))
        } else {
            plot(survfit(Surv(År, Avskjed) ~ 1, data = data))
        }
    })
})
