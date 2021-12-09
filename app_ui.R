## library
library(shiny)
library(plotly)

## Chart
#pie
chart_sidebar_content <- sidebarPanel(
  checkboxInput("data", label = strong("show data"), value = TRUE)
)

chart_main_content <- mainPanel(
  plotOutput("plot")
)

chart_panel <- tabPanel(
  "Pie chart",
  titlePanel("Pie chart"), 
  sidebarLayout(
    chart_sidebar_content,
    chart_main_content
  )
)

#bar chart

#line chart

# ui
ui <- navbarPage(
  "TV show",
  chart_panel,
)
