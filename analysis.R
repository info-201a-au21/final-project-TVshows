library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(lintr)
library(base)
library("ggplot2")
library(ggrepel)


# Load datasets
TV_show_4 <- read.csv("tv_shows.csv")
View(TV_show_4)

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

amount_each_platform <- cbind(amount_Disney., amount_Hulu, amount_Netflix, amount_Prime.Video)
amount_each_platform$Max <- max(amount_each_platform[1:4])

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

ggplot(all_underage, aes(x = all_underage_name, y = all_underage$value)) +
  labs(title= "The most recommended platform for a family", 
       y="amount of underage TV shows", x = "Platforms") +
  ylim(0, 1500) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette="Paired") +
  geom_text(aes(label = mean), position = dodgewidth, vjust = 1.6, 
            color = "white", size = 2.5)

# Which platform has the most enormous amount of TV shows (in a year)?
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

color <- c("netflix" = "red", "hulu" = "green", "prime" = "blue", "disney" = "purple")
year_line_plot <- ggplot(data = year_aggregate, aes(x = Year)) +
  geom_line(aes(y = netflix_per_year, color = "netflix")) +
  geom_line(aes(y = hulu_per_year, color = "hulu")) + 
  geom_line(aes(y = prime_per_year, color = "prime")) +
  geom_line(aes(y = disney_per_year, color = "disney")) +
  labs(x = "Year", y = "Number of TV Shows", title = "The Amount of TV Shows on 4 Platforms Each Year") +
  scale_color_manual(values = color)

ggplotly(year_line_plot)


# Which platform includes largest amount of TV shows with IMDb >  9.0?


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

# creating the bar plot
bar_IMDb <- ggplot(data = new_IMDb) +
  geom_bar(aes(x = names,
               y = new_IMDb,
               fill = names),
           stat = "identity") +
  labs(x = "Platforms", y = "Amount of TV shows with 9.0 +", title = "IMDb Higher Than 9.0")
  
ggplotly(bar_IMDb)

# How does the average IMDb rate changed over N year? line/ scatterplot
(average_IMDb_2013 <- TV_show_4 %>%
    group_by(Year) %>%
    filter(Year == 2013) %>%
    summarise(IMDb = mean(IMDb, na.rm = T)))


# Which platform has most TV show original?

# Top used language? 
library(scales)
library(RColorBrewer)
Prime_data <- read.csv("Prime TV Shows Data set.csv")
prime_lang <- Prime_data %>%
  
  group_by(Language) %>%
  tally() %>%
  filter(Language != "") %>% #to discard the empty raws appeared in original dataset
  mutate(
    lang_prop = n / sum(n)
  ) %>%
  filter(lang_prop > 0.005) # discard proportion that is less then 0.5%
  
prime_lang$lang_prop <- percent(prime_lang$lang_prop, accuracy = 0.1)

# Pie plot to see the language proportions

lang_pie <- ggplot(prime_lang, aes(x = "", y = lang_prop, fill = Language)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(lang_prop)), position = position_stack(vjust=0.5)) +
  labs(title = "Language Proportions", y = "", x = "") +
  scale_fill_brewer(palette="Paired")
lang_pie
  

# Pie chart


# what platform has the highest average IMDb rating.

highest_ave <- TV_show_4 %>%
  group_by()

