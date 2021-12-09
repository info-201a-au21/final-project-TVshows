## library
library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(base)
library(ggrepel)
library(stats)
library(graphics)
library(RColorBrewer)
library(kableExtra)
library(scales)
library(lintr)
library(knitr)

## read data
# pie chart
Prime_data <- read.csv("https://raw.githubusercontent.com/info-201a-au21/final-project-TVshows/main/file/Prime%20TV%20Shows%20Data%20set.csv?token=AV53JLGVYE76TWRAVWDTTIDBXPDCC", stringsAsFactors = F)
prime_lang <- Prime_data %>%
  group_by(Language) %>%
  tally() %>%
  filter(Language != "") %>% #to discard the empty raws appeared in original dataset
  mutate(
    lang_prop = n / sum(n)
  ) %>%
  filter(lang_prop > 0.005) # discard proportion that is less then 0.5
prime_lang$lang_prop <- percent(prime_lang$lang_prop, accuracy = 0.1)

# bar chart
TV_show_4 <- read.csv("https://raw.githubusercontent.com/info-201a-au21/final-project-TVshows/main/file/tv_shows.csv?token=AV53JLE35SVZ2QWVG32QX6DBXPC6M", stringsAsFactors = F)
TV_show_4$IMDb <- sub("/10", "", TV_show_4$IMDb)
IMDb_Netflix_9.0 <- TV_show_4 %>%
  filter(IMDb > 9.0, na.rm = TRUE,
         Netflix == 1) %>%
  summarise(IMDb_Netflix = sum(Netflix, na.rm = TRUE))

IMDb_Hulu_9.0 <- TV_show_4 %>%
  filter(IMDb > 9.0, na.rm = TRUE,
         Hulu == 1) %>%
  summarise(IMDb_Hulu = sum(Hulu, na.rm = TRUE))

IMDb_Prime_9.0 <- TV_show_4 %>%
  filter(IMDb > 9.0, na.rm = TRUE,
         Prime.Video == 1) %>%
  summarise(IMDb_Prime = sum(Prime.Video, na.rm = TRUE))

IMDb_Disney_9.0 <- TV_show_4 %>%
  filter(IMDb > 9.0, na.rm = TRUE,
         Disney. == 1) %>%
  summarise(IMDb_Disney = sum(Disney., na.rm = TRUE))

overview_IMDb <- cbind(IMDb_Netflix_9.0, IMDb_Hulu_9.0, IMDb_Disney_9.0, IMDb_Prime_9.0)
new_IMDb <- t(overview_IMDb)
new_IMDb <- data.frame(names = row.names(new_IMDb), new_IMDb)



# line chart
(year_Neflix <- TV_show_4 %>%
    group_by(Year) %>%
    filter(Netflix == 1) %>%
    summarise(netflix_per_year = sum(Netflix))
)

(year_Hulu <- TV_show_4 %>%
    group_by(Year) %>%
    filter(Hulu == 1) %>%
    summarise(hulu_per_year = sum(Hulu)))

year_Prime <- TV_show_4 %>%
  group_by(Year) %>%
  filter(Prime.Video == 1) %>%
  summarise(prime_per_year = sum(Prime.Video))

year_Disney <- TV_show_4 %>%
  group_by(Year) %>%
  filter(Disney. == 1) %>%
  summarise(disney_per_year = sum(Disney.))

year_aggregate <- year_Neflix %>%
  full_join(year_Hulu, by = "Year") %>%
  full_join(year_Prime, by = "Year") %>%
  full_join(year_Disney, by = "Year") %>%
  filter(str_length(Year) == 4)

year_aggregate[is.na.data.frame(year_aggregate)] <- 0

color <- c("netflix" = "red", "hulu" = "green", "prime" = "blue", "disney" = "purple")

## sever
server <- function(input, output) { 
  output$pie <- renderPlot({
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
  
  # bar chart
  output$bar <- renderPlot({
    if (input$pie) {
      bar_IMDb <- ggplot(new_IMDb, aes(x = "", y = new_IMDb, fill = names)) +
        geom_bar(stat = "identity") +
        labs(title = "IMDb Higher Than 9.0", y = "", x = "") +
        coord_polar("y", start = 0) +
        geom_text(aes(label = paste0(new_IMDb)), position = position_stack(vjust=0.5)) +
        labs(title = "IMDb Higher Than 9.0", y = "", x = "") +
        scale_fill_brewer(palette="Paired")
    } else {
      bar_IMDb <- ggplot(data = new_IMDb, mapping = aes(x = names,
                                                        y = new_IMDb,
                                                        fill = names)) +
        geom_bar(stat = "identity") +
        labs(x = "Platforms", y = "Amount of TV shows with 9.0 +", title = "IMDb Higher Than 9.0")
    }
    bar_IMDb
  })
  
  output$line <- renderPlot({
    p <- ggplot(data = year_aggregate, 
                mapping = aes(x = Year, y = netflix_per_year, color = "netflix")) +
      geom_point()
    labs(x = "Year", y = "Number of TV Shows", title = "The Amount of TV Shows on 4 Platforms Each Year") +
      scale_color_manual(values = color)
    if (input$smooth) {
      p <- p + geom_smooth(se = F)
    }
    p
  })
}