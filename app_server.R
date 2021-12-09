## library
library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(RColorBrewer)

## read data
Prime_data <- read.csv("Prime TV Shows Data set.csv", stringsAsFactors = F)
prime_lang <- Prime_data %>%
  group_by(Language) %>%
  tally() %>%
  filter(Language != "") %>% #to discard the empty raws appeared in original dataset
  mutate(
    lang_prop = n / sum(n)
  ) %>%
  filter(lang_prop > 0.005) # discard proportion that is less then 0.5
prime_lang$lang_prop <- percent(prime_lang$lang_prop, accuracy = 0.1)

## sever
server <- function(input, output) { 
  output$plot <- renderPlot({
    lang_pie <- ggplot(prime_lang, aes(x = "", y = lang_prop, fill = Language)) +
      geom_bar(stat = "identity") +
      labs(title = "Language Proportions", y = "", x = "") +
      coord_polar("y", start = 0)
    
    if (input$data) {
      lang_pie <- lang_pie + geom_text(aes(label = paste0(lang_prop)), position = position_stack(vjust=0.5)) +
        labs(title = "Language Proportions", y = "", x = "") +
        scale_fill_brewer(palette="Paired")
    }
    
    lang_pie 
  })
}
