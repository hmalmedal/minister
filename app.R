library(tidyverse)
library(lubridate)
library(survival)
library(broom)
library(plotly)
library(shiny)
library(bslib)

regjering <- read_csv("regjering.csv") %>%
  replace_na(list(Sluttdato = today("Europe/Oslo"))) %>%
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

    s <- survfit(Surv(År, Avskjed) ~ Regjering, data = regjeringsdata()) %>%
      tidy()

    if (!"strata" %in% names(s))
      s$strata <- paste0("Regjering=", r)

    s %>% group_by(strata) %>%
      bind_rows(summarise(., time = 0, estimate = 1,
                          conf.high = 1, conf.low = 1)) %>%
      arrange(strata, time) %>%
      separate(strata, c("key", "Regjering"), "=")
  })

  output$p <- renderPlotly({
    d <- regjering_survfit()
    plt <- plot_ly()
    for (g in unique(d$Regjering)) {
      dg <- d %>% filter(Regjering == g)
      plt <- plt %>%
        add_lines(
          data = dg,
          x = ~time, y = ~estimate,
          name = g, legendgroup = g,
          line = list(shape = "hv"),
          hovertemplate = paste0(
            "<b>", g, "</b><br>",
            "År: %{x:.2f}<br>",
            "Rate: %{y:.3f}<extra></extra>"
          )
        )
    }
    plt %>%
      layout(
        xaxis = list(title = "År", zeroline = FALSE),
        yaxis = list(title = "Rate", range = c(0, 1), tickformat = ".0%"),
        legend = list(orientation = "v"),
        margin = list(l = 60, r = 20, b = 50, t = 40)
      )
  })
}


ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Tid i regjering"),
  layout_sidebar(
    sidebar = sidebar(
      selectInput(inputId = "valgteregjeringer",
                  label = "Velg regjeringer",
                  choices = regjeringer,
                  multiple = TRUE),
      a(href = "https://github.com/hmalmedal/minister", "GitHub")
    ),
    plotlyOutput("p", height = 520)
  )
)

shinyApp(ui = ui, server = server)
