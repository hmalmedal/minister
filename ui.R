library(shiny)

regjeringsliste <- read.csv("regjering.csv",
                            colClasses = c(rep("NULL", 4),
                                           "character"))
regjeringsliste <- unique(regjeringsliste)
regjeringsliste <- regjeringsliste[, 1]
kunregjeringer <- regjeringsliste
regjeringsliste <- c("Alle regjeringer", regjeringsliste)

shinyUI(

    pageWithSidebar(

        headerPanel("Tid i regjering"),

        sidebarPanel(
            checkboxInput(inputId = "sammenlign",
                          label = "Sammenlign",
                          value = F),
            conditionalPanel(condition = "input.sammenlign == false",
                             selectInput(inputId = "valgtregjering",
                                         label = "Velg regjering",
                                         choices = regjeringsliste,
                                         selected = "Alle regjeringer")
            ),
            conditionalPanel(condition = "input.sammenlign == true",
                             selectInput(inputId = "valgteregjeringer",
                                         label = "Velg regjeringer",
                                         choices = kunregjeringer,
                                         selected = kunregjeringer[1],
                                         multiple = T)
            ),
            radioButtons(inputId = "dager_år",
                         label = "",
                         choices = c("Dager", "År"),
                         selected = "Dager"),
            checkboxInput(inputId = "merker",
                          label = "Merker",
                          value = T),
            uiOutput("zoom")
        ),

        mainPanel(
            conditionalPanel(condition = "input.sammenlign == false",
                             tabsetPanel(
                                 tabPanel("Plott", plotOutput("plot")),
                                 tabPanel("Tabell", dataTableOutput("tabell"))
                             )
            ),
            conditionalPanel(condition = "input.sammenlign == true",
                             plotOutput("plot2")
            )
        )
    )
)
