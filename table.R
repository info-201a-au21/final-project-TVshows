library(knitr)

TV_show_4 %>% select(Title, Year, Age, IMDb, Netflix, Hulu, Prime.Video, Disney.) %>%
  kable(caption = 'Table: TV Shows (Netflix, Hulu, Prime, Disney)')
