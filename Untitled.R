


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

# Which platform is the most recommended for a family, which have children to subscribe? 
## bar plot


# Which platform has the most enormous amount of TV shows (in a year)?

# Which platform includes largest amount of TV shows with IMDb >  9.0?

# How does the average IMDb rate changed over N year? line/ scatterplot

# Which platform has most TV show original?
# Top used language? 
