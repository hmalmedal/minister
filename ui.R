library(shiny)

regjeringsliste <- read.csv("regjering.csv",
                            colClasses=c(rep("NULL", 4),
                                         "character"))
regjeringsliste <- unique(regjeringsliste)
regjeringsliste <- regjeringsliste[, 1]
regjeringsliste <- c("Alle regjeringer", regjeringsliste)

shinyUI(

    pageWithSidebar(

        headerPanel("Tid i regjering"),

        sidebarPanel(
            selectInput(inputId = "valgtregjering",
                        label = "Velg regjering",
                        choices = regjeringsliste,
                        selected = "Alle regjeringer"),
            radioButtons(inputId = "dager_år",
                         label = "",
                         choices = c("Dager", "År"),
                         selected = "Dager"),
            checkboxInput(inputId = "merker",
                          label = "Merker",
                          value = T)
        ),

        mainPanel(
            plotOutput("plot"),
            dataTableOutput("tabell")
        )
    )
)
