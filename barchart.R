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
