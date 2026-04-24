library(tidyverse)
library(lubridate)
library(survival)
library(broom)
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
    r <- regjeringsdata()$Regjering[1]

    s <- survfit(Surv(År, Avskjed) ~ Regjering, data = regjeringsdata()) |>
      tidy()

    if (!"strata" %in% names(s)) {
      s$strata <- paste0("Regjering=", r)
    }

    s |>
      group_by(strata) |>
      (\(x) bind_rows(
        x,
        summarise(
          x,
          time = 0,
          estimate = 1,
          conf.high = 1,
          conf.low = 1
        )
      ))() |>
      arrange(strata, time) |>
      separate(strata, c("key", "Regjering"), "=")
  })

  output$p <- renderPlot({
    d <- regjering_survfit()

    ggplot(d, aes(x = time, y = estimate, color = Regjering)) +
      geom_step() +
      scale_y_continuous(
        name = "Rate",
        limits = c(0, 1),
        labels = scales::label_percent()
      ) +
      labs(x = "År")
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
