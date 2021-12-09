## library
library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(base)
library(ggrepel)


# Load datasets
TV_show_4 <- read.csv("tv_shows.csv")


#1. Which platform includes the largest amount of TV shows? 
#Netflix
(amount_Netflix <- TV_show_4 %>%
    filter(Netflix == 1) %>%
    summarise(sum_Netflix = sum(Netflix)))

#Hulu
(amount_Hulu <- TV_show_4 %>%
    filter(Hulu == 1) %>%
    summarise(sum_Hulu = sum(Hulu)))

#Prime.Video
(amount_Prime.Video <- TV_show_4 %>%
    filter(Prime.Video == 1) %>%
    summarise(sum_Prime.Video = sum(Prime.Video)))

#Disney.
(amount_Disney. <- TV_show_4 %>%
    filter(Disney. == 1) %>%
    summarise(sum_Disney. = sum(Disney.)))

amount_each_platform <- cbind(amount_Disney., amount_Hulu, amount_Netflix, amount_Prime.Video)
amount_each_platform$Max <- max(amount_each_platform[1:4])

# 2. Which platform is the most recommended for a family, which have children to subscribe? 
## bar plot (blank in Age didn't remove yet)
(netflix_underage <- TV_show_4 %>%
    filter(Age != "18+", na.rm = TRUE) %>%
    filter(Age != "all", na.rm = TRUE) %>%
    filter(Netflix == 1, na.rm = TRUE) %>%
    nrow()
)
(hulu_underage <- TV_show_4 %>%
    filter(Age != "18+", na.rm = TRUE) %>%
    filter(Age != "all", na.rm = TRUE) %>%
    filter(Hulu == 1, na.rm = TRUE) %>%
    nrow()
)
(prime_underage <- TV_show_4 %>%
    filter(Age != "18+", na.rm = TRUE) %>%
    filter(Age != "all", na.rm = TRUE) %>%
    filter(Prime.Video == 1, na.rm = TRUE) %>%
    nrow())

(disney_underage <- TV_show_4 %>%
    filter(Age != "18+", na.rm = TRUE) %>%
    filter(Age != "all", na.rm = TRUE) %>%
    filter(Disney. == 1, na.rm = TRUE) %>%
    nrow())

all_underage <- rbind(netflix_underage, hulu_underage, prime_underage, disney_underage)
all_underage_name <- c("Netflix", "Hulu", "Prime Video", "Disney")
colnames(all_underage) <- "value"

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
