library(knitr)
library(kableExtra)

TV_show_4 %>% select(Title, Year, Age, IMDb, Netflix, Hulu, Prime.Video, Disney.) %>%
  group_by(Year) %>%
  kable(caption = 'Table: TV Shows (Netflix, Hulu, Prime, Disney)') %>%
  kable_styling(bootstrap_options = c('striped', 'hover', 'responsive', 'condensed'))
