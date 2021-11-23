ggplot(data = all_underage, aes(x = all_underage_name, y = all_underage$value)) +
  labs(title= "The most recommended platform for a family", 
       y="amount of underage TV shows", x = "Platforms") +
  ylim(0, 1500) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette="Paired") +
  geom_text(aes(label = mean), position = dodgewidth, vjust = 1.6, 
            color = "white", size = 2.5)