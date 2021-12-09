## library
library(shiny)
library(plotly)

## Chart
#pie
pie_sidebar_content <- sidebarPanel(
  checkboxInput("data", label = strong("show data"), value = TRUE)
)

pie_main_content <- mainPanel(
  plotOutput("pie")
)

pie_panel <- tabPanel(
  "Pie chart",
  titlePanel("Pie chart"), 
  sidebarLayout(
    pie_sidebar_content,
    pie_main_content
  )
)

#bar chart
bar_sidebar_content <- sidebarPanel(
  checkboxInput("pie", label = strong("Turn into pie chart"), value = F)
)

bar_main_content <- mainPanel(
  plotOutput("bar")
)

bar_panel <- tabPanel(
  "Bar chart",
  titlePanel("Bar chart"), 
  sidebarLayout(
    bar_sidebar_content,
    bar_main_content
  )
)

#line chart
year_range <- as.numeric(range(year_aggregate$Year)) 
line_sidebar_content <- sidebarPanel(
  sliderInput(
    "year",
    label = "Year", 
    min = year_range[1],
    max = year_range[2], 
    value = c(2000,2020)
  )
)

line_main_content <- mainPanel(
  plotOutput("line")
)

line_panel <- tabPanel(
  "Line chart",
  titlePanel("Line chart"), 
  sidebarLayout(
    line_sidebar_content,
    line_main_content
  )
)

# Introductory
intro_tab <- tabPanel(
  "Introduction",
  titlePanel("Introduction"), 
  fluidPage(
    img("", src = "fp1.jpg"),
    h3("Team Member: Helen Lei, Kelsey Li, Scarlet Zheng"),
    br(),
    h4("Background"),
    p("We are interested in the quality and popularity of TV shows in the 
     mainstreaming platforms, including Netflix, Prime Video, Hulu, and Disney+. 
     During the pandemic period, people stay at home and do limited activity. A 
     tremendous amount of people tend to watch TV shows during quarantine which 
     lead streaming platforms to increas subscribers."),
    br(),
    h4("Database"),
    p("First of all, we use the data set collected by Ruchi Bhatia, and the 
     methodology that she uses is IMDb (data was scraped using Beautiful Soup). 
     This intergraded data set contains the data from several platforms, which 
     helps us a lot since she puts information in one CSV file. Neelima Jauhari 
     collects the other one, which is the Amazon Prime website. This one focus 
     on one platform, and we explore the diversity of languages in our 
     analysis.")
  )
  
)

# Conclusion
conclude_tab <- tabPanel(
  "Conclusion",
  titlePanel("Conclusion"), 
  fluidPage(
    h4("1. Blooming of Streaming Services"),
    p("Since the year of 2000, streaming services have started blooming. 
      As two market leaders, Netflix and Amazon Prime Video, brought accessibility 
      to new TV shows to their peak within the past 20 years, TV shows production 
      was stimulated in return. However, the pandemic in 2020 hindered the productions,
      and a massive reduction in numbers was presented."),
    br(),
    h4("2. Trend of TVshows"),
    p("In this big data era, internet databases collect reviews and ratings of movies and tv 
       series is a phenomenon and provide a reference for users to make tv show selections. 
       Within the 10 point rating scale, subscribers are able to pick high-quality shows easily. 
       From the dataset, the most recommended platforms for including a larger number of highly-rated 
       tv-shows are Netflix and Hulu."),
    br(),
    h4("3. The Diversity of Language"),
    p("Many TV shows have different languages applied on the Amazon Prime 
      platform. The diversity of languages reflects the diversity in the US. 
      In one perspective, as a platform that has the most subscription from US 
      citizens, Amazon Prime explore the market of other languages such as 
      Hindi, Japanese, etc., and it attracts customers who can speak other 
      languages to subscribe. For example, Japanese-Americans would like to 
      subscribe because of Japanese TV shows. In otherwise, languages are one 
      of the features of humanity. The difference between languages shows the 
      various ways that people communicate with each other. People can 
      communicate by using their languages, so the diversity of languages 
      in a platform builds a bridge for globalization.")
  )
)

# ui
ui <- navbarPage(
  "TV show",
  intro_tab,
  line_panel,
  bar_panel,
  pie_panel,
  conclude_tab
)
