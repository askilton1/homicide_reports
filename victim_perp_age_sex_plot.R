library(tidyverse);source("clean.R")
homicides <- clean()

toplot <- homicides %>%
  select(VictimSex, VictimAge, PerpetratorSex, PerpetratorAge) %>%
  filter(VictimSex %in% c("Female", "Male"),
         PerpetratorSex %in% c("Female", "Male")) %>%
  mutate(PerpetratorSex = paste0("Perpetrator: ", PerpetratorSex),
         VictimSex = paste0("Victim: ", VictimSex))

ggplot(toplot, aes(x = VictimAge, y = PerpetratorAge)) +
  geom_point(alpha = 0.01) +
  coord_equal() +
  theme_minimal() +
  facet_wrap(PerpetratorSex ~ VictimSex) +
  labs(title = "Homicides 1980-2014")

ggsave("homicides_age_and_gender.png")

homicides %>% 
  filter(PerpetratorSex == "Male", VictimSex == "Male",
         State %in% c("California", "Texas", "New York", "Florida", "Michigan"),
         Weapon %in% c("Handgun", "Knife", "Blunt Object", "Firearm")) %>% 
  # sample_n(10000) %>% 
  ggplot(aes(x = VictimAge, y = PerpetratorAge)) +
  geom_point(alpha = 0.1) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Homicides Between Males 1980-2014", 
       y = "Perpetrator Age",
       x = "Victim Age") +
  facet_grid(Weapon~State) +
  xlim(NA, 75) + ylim(10, 75)

ggsave("homicides_btwn_males_states_weapons.png")


count(homicides, State, sort = T)

count(homicides, Weapon, sort = T)

count(homicides, VictimAge)

homicides %>% 
  filter(VictimAge < 6) %>% 
  count(Weapon, sort = T)
