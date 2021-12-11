## library
library(shiny)
library(plotly)
library(ggplot2)
library(tidyverse)
library(base)
library(ggrepel)
library(stats)
library(graphics)
library(RColorBrewer)
library(dplyr)
library(scales)
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
# barchart
# 9.0+
TV_show_4 <- read.csv("https://raw.githubusercontent.com/info-201a-au21/final-project-TVshows/main/file/tv_shows.csv?token=AV53JLE35SVZ2QWVG32QX6DBXPC6M", stringsAsFactors = F)
TV_show_4$IMDb <- sub("/10", "", TV_show_4$IMDb)
IMDb_Netflix_9.0 <- TV_show_4 %>%
  filter(IMDb > 9.0, na.rm = TRUE,
         Netflix == 1) %>%
  summarise(IMDb_Netflix_9.0 = sum(Netflix, na.rm = TRUE))

IMDb_Hulu_9.0 <- TV_show_4 %>%
  filter(IMDb > 9.0, na.rm = TRUE,
         Hulu == 1) %>%
  summarise(IMDb_Hulu_9.0 = sum(Hulu, na.rm = TRUE))

IMDb_Prime_9.0 <- TV_show_4 %>%
  filter(IMDb > 9.0, na.rm = TRUE,
         Prime.Video == 1) %>%
  summarise(IMDb_Prime_9.0 = sum(Prime.Video, na.rm = TRUE))

IMDb_Disney_9.0 <- TV_show_4 %>%
  filter(IMDb > 9.0, na.rm = TRUE,
         Disney. == 1) %>%
  summarise(IMDb_Disney_9.0 = sum(Disney., na.rm = TRUE))

# 7.0-9.0
IMDb_Netflix_8.0 <- TV_show_4 %>%
  filter(IMDb > 7.0, IMDb <= 9.0, na.rm = TRUE,
         Netflix == 1) %>%
  summarise(IMDb_Netflix_8.0 = sum(Netflix, na.rm = TRUE))

IMDb_Hulu_8.0 <- TV_show_4 %>%
  filter(IMDb > 7.0, IMDb <= 9.0, na.rm = TRUE,
         Hulu == 1) %>%
  summarise(IMDb_Hulu_8.0 = sum(Hulu, na.rm = TRUE))

IMDb_Prime_8.0 <- TV_show_4 %>%
  filter(IMDb > 7.0, IMDb <= 9.0, na.rm = TRUE,
         Prime.Video == 1) %>%
  summarise(IMDb_Prime_8.0 = sum(Prime.Video, na.rm = TRUE))

IMDb_Disney_8.0 <- TV_show_4 %>%
  filter(IMDb > 7.0, IMDb <= 9.0, na.rm = TRUE,
         Disney. == 1) %>%
  summarise(IMDb_Disney_8.0 = sum(Disney., na.rm = TRUE))

# 7.0 - 
IMDb_Netflix_7.0 <- TV_show_4 %>%
  filter(IMDb < 7.0, na.rm = TRUE,
         Netflix == 1) %>%
  summarise(IMDb_Netflix_7.0 = sum(Netflix, na.rm = TRUE))

IMDb_Hulu_7.0 <- TV_show_4 %>%
  filter(IMDb < 7.0, na.rm = TRUE,
         Hulu == 1) %>%
  summarise(IMDb_Hulu_7.0 = sum(Hulu, na.rm = TRUE))

IMDb_Prime_7.0 <- TV_show_4 %>%
  filter(IMDb < 7.0, na.rm = TRUE,
         Prime.Video == 1) %>%
  summarise(IMDb_Prime_7.0 = sum(Prime.Video, na.rm = TRUE))

IMDb_Disney_7.0 <- TV_show_4 %>%
  filter(IMDb < 7.0, na.rm = TRUE,
         Disney. == 1) %>%
  summarise(IMDb_Disney_7.0 = sum(Disney., na.rm = TRUE))


overview_IMDb_9.0 <- cbind(IMDb_Netflix_9.0, IMDb_Hulu_9.0, IMDb_Disney_9.0, IMDb_Prime_9.0)
overview_IMDb_8.0 <- cbind(IMDb_Netflix_8.0, IMDb_Hulu_8.0, IMDb_Disney_8.0, IMDb_Prime_8.0)
overview_IMDb_7.0 <- cbind(IMDb_Netflix_7.0, IMDb_Hulu_7.0, IMDb_Disney_7.0, IMDb_Prime_7.0)
new_IMDb_9.0 <- as.data.frame(t(overview_IMDb_9.0))
new_IMDb_8.0 <- as.data.frame(t(overview_IMDb_8.0))
new_IMDb_7.0 <- as.data.frame(t(overview_IMDb_7.0))
new_IMDb_all <- cbind(new_IMDb_7.0, new_IMDb_8.0, new_IMDb_9.0)
colnames(new_IMDb_all) <- c("< 0.7", "0.7 - 0.9", "0.9+")
rownames(new_IMDb_all) <- c("Netflix", "Hulu", "Disney+", "Prime")
Platforms <- c("Netflix", "Hulu", "Disney+", "Prime")


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
  
  output$bar <- renderPlot({
    bar_IMDb <- ggplot(data = new_IMDb_all) +
      geom_bar(aes(x = x_axis,
                   y = !!as.name(input$range),
                   fill = Platforms),
               stat = "identity") +
      labs(x = "Platforms", 
           y = "Amount of TV shows with 9.0 +", 
           title = "IMDb Distribution")
    return(bar_IMDb)
  })
  
  output$line <- renderPlot({
    plot_data <- year_aggregate %>%
      filter(Year > input$year[1], Year < input$year[2])
    
    linear <- ggplot(data = plot_data, aes(x = Year)) +
      geom_line(aes(y = netflix_per_year, color = "netflix")) +
      geom_line(aes(y = hulu_per_year, color = "hulu")) +
      geom_line(aes(y = prime_per_year, color = "prime")) +
      geom_line(aes(y = disney_per_year, color = "disney")) +
      labs(x = "Years", y = "Number of TV Shows",
           title = "The Amount of TV Shows on 4 Platforms Each Year")
    # scale_color_manual(values = color)
    return(linear)
  })
}