library(tidyverse);source("clean.R")
homicides <- clean()

homicides %>% 
  filter(VictimSex != "Unknown", PerpetratorSex != "Unknown") %>% 
  mutate(age_gap = abs(PerpetratorAge - VictimAge)) %>% 
  mutate(PerpetratorSex = paste0("Perpetrator: ", PerpetratorSex),
         VictimSex = paste0("Victim: ", VictimSex)) %>% 
  ggplot(aes(x = age_gap)) +
  geom_histogram(bins = 60) +
  facet_wrap(VictimSex~PerpetratorSex, scales = "free") +
  labs(x = "Age of Perpetror - Age of Victim")


homicides %>% 
  filter(VictimSex != "Unknown", PerpetratorSex != "Unknown") %>% 
  mutate(age_gap = PerpetratorAge - VictimAge,
         PerpetratorAge = factor(PerpetratorAge, levels = 0:150, ordered = T)) %>% 
  ggplot(aes(x = (PerpetratorAge), y = age_gap)) +
  geom_boxplot()

homicides %>% 
  filter(PerpetratorSex != "Unknown") %>% 
  group_by(PerpetratorAge, PerpetratorSex) %>% 
  summarise(median_VictimAge = median(VictimAge, na.rm = T), n()) %>% 
  ggplot(aes(x = PerpetratorAge, y = median_VictimAge, color = PerpetratorSex)) +
  geom_line()

toplot <- homicides %>%
  select(VictimSex, VictimAge, PerpetratorSex, PerpetratorAge) %>%
  filter(VictimSex %in% c("Female", "Male"),
         PerpetratorSex %in% c("Female", "Male")) %>%
  mutate(PerpetratorSex = paste0("Perpetrator: ", PerpetratorSex),
         VictimSex = paste0("Victim: ", VictimSex))
toplot %>% 
  group_by(PerpetratorAge, PerpetratorSex, VictimSex) %>% 
  summarise(median_VictimAge = median(VictimAge, na.rm = T), n = n()) %>% 
  unite(Sex, PerpetratorSex, VictimSex, sep = ", ") %>% 
  # ggplot(aes(x = n)) +geom_histogram()
  filter(n > 50) %>% na.omit %>% 
  #ungroup %>% mutate(PerpetratorAge = factor(PerpetratorAge, levels = 0:150, ordered = T)) %>% 
  ggplot(aes(x = PerpetratorAge, y = median_VictimAge, color = Sex)) +
  geom_line() +
  # geom_point(aes(size = n)) +
  theme_minimal() +
  labs(title = "Homicides 1980-2014", 
       y = "Median Age of Victim")

toplot %>% 
  unite(Sex, PerpetratorSex, VictimSex, sep = ", ") %>% 
  #ungroup %>% mutate(PerpetratorAge = factor(PerpetratorAge, levels = 0:150, ordered = T)) %>% 
  ggplot(aes(x = PerpetratorAge, y = VictimAge, color = Sex)) +
  geom_smooth() +
  # geom_point(aes(size = n)) +
  theme_minimal() +
  labs(title = "Homicides 1980-2014")

