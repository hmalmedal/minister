library(tidyverse)
library(lubridate)
library(survival)
library(ggsurvfit)
library(shiny)
library(bslib)

regjering <- read_csv("regjering.csv") |>
  replace_na(list(Sluttdato = today("Europe/Oslo"))) |>
  mutate(År = decimal_date(Sluttdato) - decimal_date(Startdato))

regjeringer <- unique(regjering$Regjering)

server <- function(input, output, session) {
  regjeringsdata <- reactive({
    i <- which(regjering$Regjering %in% input$valgteregjeringer)
    if (length(i) == 0) {
      regjering$Regjering <- "Alle regjeringer"
      regjering
    } else {
      regjering[i, ]
    }
  })

  regjering_survfit <- reactive({
    survfit2(
      Surv(År, Avskjed) ~ Regjering,
      data = regjeringsdata(),
      time0 = TRUE
    )
  })

  output$p <- renderPlot({
    regjering_survfit() |>
      ggsurvfit() +
      scale_ggsurvfit() +
      labs(
        x = "År som statsråd",
        y = "Estimert andel uten avskjed"
      )
  })
}

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Tid i regjering"),
  layout_sidebar(
    sidebar = sidebar(
      selectInput(
        inputId = "valgteregjeringer",
        label = "Velg regjeringer",
        choices = regjeringer,
        multiple = TRUE
      ),
      a(href = "https://github.com/hmalmedal/minister", "GitHub")
    ),
    plotOutput("p", height = 520)
  )
)

shinyApp(ui = ui, server = server)
