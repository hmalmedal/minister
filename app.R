library(readr)
library(dplyr)
library(lubridate)

regjering <- read_csv("regjering.csv") %>%
  mutate(Sluttdato = replace(Sluttdato, is.na(Sluttdato), today("Europe/Oslo")),
         Dager = as.numeric(Sluttdato - Startdato),
         År = decimal_date(Sluttdato) - decimal_date(Startdato))

regjeringer <- unique(regjering$Regjering)

library(survival)
library(broom)
library(tidyr)
library(ggvis)

server <- function(input, output) {
  input_opacity <- reactive(input$opacity)

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

    s <- survfit(Surv(År, Avskjed) ~ Regjering, data = regjeringsdata()) %>%
      tidy()

    if (is.null(s$strata))
      s$strata <- paste0("Regjering=", r)

    s %>% group_by(strata) %>%
      bind_rows(summarise(., time = 0, estimate = 1,
                          conf.high = 1, conf.low = 1)) %>%
      arrange(strata, time) %>%
      separate(strata, c("key", "Regjering"), "=")
  })

  regjering_survfit %>%
    group_by(Regjering) %>%
    ggvis(x = ~time, interpolate := "step-after") %>%
    layer_ribbons(y = ~conf.high, y2 = ~conf.low, opacity := input_opacity,
                  fill = ~Regjering) %>%
    layer_lines(y = ~estimate, stroke = ~Regjering) %>%
    scale_numeric("y", domain = c(0, 1)) %>%
    bind_shiny("r")
}

ui <- fluidPage(
  titlePanel("Tid i regjering"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "valgteregjeringer",
                  label = "Velg regjeringer",
                  choices = regjeringer,
                  multiple = TRUE),
      sliderInput("opacity", "Opasitet:",
                  min = 0, max = 1, value = 0.2)
    ),
    mainPanel(
      ggvisOutput("r")
    )
  )
)

shinyApp(ui = ui, server = server)