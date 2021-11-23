library(dplyr)
library(tidyverse)
library(lintr)
library(base)
library("ggplot2")
library(ggrepel)


# Load datasets
TV_show_4 <- read.csv("tv_shows.csv")

# Which platform includes the largest amount of TV shows? 
## Netflix(1971) has the largest amount of TV shows
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

# Which platform is the most recommended for a family, which have children to subscribe? 
## bar plot (blank in Age didn't remove yet)
netflix_underage <- TV_show_4 %>%
  filter(Age != "18+", na.rm = TRUE) %>%
  filter(Age != "all", na.rm = TRUE) %>%
  filter(Netflix == 1, na.rm = TRUE) %>%
  nrow()

hulu_underage <- TV_show_4 %>%
  filter(Age != "18+", na.rm = TRUE) %>%
  filter(Age != "all", na.rm = TRUE) %>%
  filter(Hulu == 1, na.rm = TRUE) %>%
  nrow()

prime_underage <- TV_show_4 %>%
  filter(Age != "18+", na.rm = TRUE) %>%
  filter(Age != "all", na.rm = TRUE) %>%
  filter(Prime.Video == 1, na.rm = TRUE) %>%
  nrow()

disney_underage <- TV_show_4 %>%
  filter(Age != "18+", na.rm = TRUE) %>%
  filter(Age != "all", na.rm = TRUE) %>%
  filter(Disney. == 1, na.rm = TRUE) %>%
  nrow()
  
all_underage <- rbind(netflix_underage, hulu_underage, prime_underage, disney_underage)
all_underage_name <- c("Netflix", "Hulu", "Prime Video", "Disney")
colnames(all_underage) <- "value"

ggplot(data = all_underage, aes(x = all_underage_name, y = all_underage$value)) +
  labs(title= "The most recommended platform for a family", 
       y="amount of underage TV shows", x = "Platforms") +
  ylim(0, 1500) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette="Paired") +
  geom_text(aes(label = mean), position = dodgewidth, vjust = 1.6, 
            color = "white", size = 2.5)
# Which platform has the most enormous amount of TV shows (in a year)?


# Which platform includes largest amount of TV shows with IMDb >  9.0?

# How does the average IMDb rate changed over N year? line/ scatterplot
(average_IMDb_2013 <- TV_show_4 %>%
    group_by(Year) %>%
    filter(Year == 2013) %>%
    summarise(IMDb = mean(IMDb, na.rm = T)))


# Which platform has most TV show original?
# Top used language? 
# Pie chart
