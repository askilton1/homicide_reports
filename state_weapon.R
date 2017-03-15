library(tidyverse);source("clean.R")
homicides <- clean()

homicides %>% 
  filter(State %in% c("California", "Texas", "New York", "Florida", "Michigan"),
         Weapon %in% c("Handgun", "Knife", "Blunt Object", "Firearm")) %>% 
  count(Year, State, Weapon, sort = T) %>% 
  ggplot(aes(x = Year, y = n, color = Weapon)) +
  geom_line() +
  facet_wrap(~Weapon + State, scales = "free") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "Homicides 1980-2014",
       y = "Number of Homicides")

ggsave("state_weapon.png")

