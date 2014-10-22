library(shiny)
library(dplyr)

regjeringsliste <- read.csv("regjering.csv", stringsAsFactors = FALSE) %>%
  select(Regjering) %>%
  distinct %>%
  getElement(1) %>%
  c("Alle regjeringer", .)
kunregjeringer <- regjeringsliste[-1]

shinyUI(

  pageWithSidebar(

    headerPanel("Tid i regjering"),

    sidebarPanel(
      checkboxInput(inputId = "sammenlign",
                    label = "Sammenlign",
                    value = FALSE),
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
                                   multiple = TRUE)
      ),
      radioButtons(inputId = "dager_år",
                   label = "",
                   choices = c("Dager", "År"),
                   selected = "Dager"),
      checkboxInput(inputId = "merker",
                    label = "Merker",
                    value = TRUE),
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
