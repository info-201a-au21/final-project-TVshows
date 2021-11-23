library(dplyr)
library(ggplot2)
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
