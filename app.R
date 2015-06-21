library(readr)
library(dplyr)
library(lubridate)
regjering <- read_csv("regjering.csv") %>%
  mutate(Sluttdato = replace(Sluttdato, is.na(Sluttdato), today("Europe/Oslo")),
         Dager = as.numeric(Sluttdato - Startdato),
         År = decimal_date(Sluttdato) - decimal_date(Startdato))

library(survival)
library(broom)
library(ggvis)
server <- function(input, output) {
  input_opacity <- reactive(input$opacity)
  survfit(Surv(År, Avskjed) ~ 1, data = regjering) %>%
    tidy() %>%
    bind_rows(summarise(., time = 0, estimate = 1,
                        conf.high = 1, conf.low = 1)) %>%
    arrange(time) %>%
    ggvis(x = ~time, interpolate := "step-after") %>%
    layer_ribbons(y = ~conf.high, y2 = ~conf.low, opacity := input_opacity) %>%
    layer_lines(y = ~estimate) %>%
    scale_numeric("y", domain = c(0, 1)) %>%
    bind_shiny("r")
}

ui <- fluidPage(
  titlePanel("Tid i regjering"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("opacity", "Opasitet:",
                  min = 0, max = 1, value = 0.2)
    ),
    mainPanel(
      ggvisOutput("r")
    )
  )
)

shinyApp(ui = ui, server = server)
