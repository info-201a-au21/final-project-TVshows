library(knitr)

TV_show_4 %>% select(Title, Year, Age, IMDb, Netflix, Hulu, Prime.Video, Disney.) %>%
<<<<<<< HEAD
  group_by(Year) %>%
  kable(caption = 'Table: TV Shows (Netflix, Hulu, Prime, Disney)') %>%
  kable_styling(bootstrap_options = c('striped', 'hover', 'responsive', 'condensed'))
=======
  kable(caption = 'Table: TV Shows (Netflix, Hulu, Prime, Disney)')
>>>>>>> c2d64d4f0e5d40198ec4312caebda93cc0d68435
