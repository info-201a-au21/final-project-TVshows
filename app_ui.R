## library
library(shiny)
library(plotly)

# Linear Plot
linear_chart_sidebar <- sidebarPanel(
  sliderInput(
    inputId = "linear",
    label = "Year Range",
    min = 1900,
    max = 2025,
    value = c(1925,2000)
  )
)

linear_chart_main <- mainPanel(
  plotlyOutput(outputId = "linear")
)

linear_chart_tab <- tabPanel(
  "Linear Chart Visulization",
  titlePanel("Linear chart"),
  sidebarLayout(
    linear_chart_sidebar,
    linear_chart_main,
  ),
  fluidPage(
    p("some words")
  )
)

# Bar Chart
bar_chart_sidebar <- sidebarPanel(
  selectInput(
    inputId = "bar",
    label = "IMDb Rating Range",
    choices = list(
      "IMDb > 9.0" = "xx",
      "7.0 < IMDb <= 9.0" = "xx",
      "IMDb < 7.0" = "xx"
    ),
    selected = "xx"
  )
)

bar_chart_main <- mainPanel(
  plotlyOutput(outputId = "bar")
)

bar_chart_tab <- tabPanel(
  "Bar Chart Visulization",
  titlePanel("Bar chart"),
  sidebarLayout(
    bar_chart_sidebar,
    bar_chart_main,
  ),
  fluidPage(
    p("some words")
  )
)

# Pie Chart
pie_sidebar_content <- sidebarPanel(
  checkboxInput(inputId = "data", 
                label = strong("show data"), 
                value = TRUE)
)

pie_main_content <- mainPanel(
  plotlyOutput(outputId = "pie")
)

pie_chart_tab <- tabPanel(
  "Pie Chart Visualization",
  titlePanel("Pie chart"), 
  sidebarLayout(
    pie_sidebar_content,
    pie_main_content
  )
)


# Introductory
intro_tab <- tabPanel(
  "Introduction",
  fluidPage(
    p(paste("blah blah blah"))
  )
)

# Conclusion
conclude_tab <- tabPanel(
  "Conclusion",
  fluidPage(
    h1("This is a title"),
    p(paste("blah blah blah"))
  )
)

# ui
ui <- navbarPage(
  "TV show",
  intro_tab,
  linear_chart_tab,
  bar_chart_tab,
  pie_chart_tab,
  conclude_tab
)
