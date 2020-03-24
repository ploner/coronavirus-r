library(shiny)
library(plotly)

shinyUI(fluidPage(
  tags$head(includeHTML("google-analytics.html")),
  tags$style(
    type='text/css', 
    ".selectize-input { font-family: Courier New, monospace; } .selectize-dropdown { font-family: Courier New, monospace; }"
  ),
  tags$style(HTML(
    "body { font-family: Courier New, monospace; line-height: 1.1; }"
  )),
  
  titlePanel("Case History of the Coronavirus (COVID-19)"),
  fluidRow(
    column(
      4, 
      selectizeInput("country", label=h5("Country"), choices=NULL, width="100%")
    ),
    column(
      4, 
      selectizeInput("state", label=h5("State / Province"), choices=NULL, width="100%")
    ),
    column(
      4, 
      checkboxGroupInput(
        "metrics", label=h5("Selected Metrics"), 
        choices=c("Confirmed", "Deaths", "Recovered"), 
        selected=c("Confirmed", "Deaths", "Recovered"), width="100%")
    )
  ),
  fluidRow(
    plotlyOutput("dailyMetrics")
  ),
  fluidRow(
    plotlyOutput("cumulatedMetrics")
  )
))

