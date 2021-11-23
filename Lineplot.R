library(dplyr)
library(ggplot2)
library(plotly)

# Load datasets

TV_show_4 <- read.csv("tv_shows.csv")
View(TV_show_4)

# Calculated the amount of TV shows that are released in each year

year_Neflix <- TV_show_4 %>%
  group_by(Year) %>%
  filter(Netflix == 1) %>%
  summarise(netflix_per_year = sum(Netflix))

year_Hulu <- TV_show_4 %>%
  group_by(Year) %>%
  filter(Hulu == 1) %>%
  summarise(hulu_per_year = sum(Hulu))

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
  full_join(year_Disney, by = "Year")

year_aggregate[is.na.data.frame(year_aggregate)] <- 0

# Creating the line chart

color <- c("netflix" = "red", "hulu" = "green", "prime" = "blue", "disney" = "purple")
year_line_plot <- ggplot(data = year_aggregate, aes(x = Year)) +
  geom_line(aes(y = netflix_per_year, color = "netflix")) +
  geom_line(aes(y = hulu_per_year, color = "hulu")) + 
  geom_line(aes(y = prime_per_year, color = "prime")) +
  geom_line(aes(y = disney_per_year, color = "disney")) +
  labs(x = "Year", y = "Number of TV Shows", title = "The Amount of TV Shows on 4 Platforms Each Year") +
  scale_color_manual(values = color)

ggplotly(year_line_plot)
