homicides %>% glimpse

homicides %>% 
  count(Relationship, sort = T)

homicides %>% 
  count(Weapon, sort = T)

homicides %>%
  inner_join(homicides %>% 
               count(Relationship, sort = T) %>% 
               slice(1:4) %>% 
               select(-n)) %>%
  inner_join(homicides %>% 
               count(Weapon, sort = T) %>% 
               slice(1:4)) %>% 
  ggplot(aes(x = VictimAge, y = PerpetratorAge)) +
  geom_point(alpha = 0.01) +
  facet_grid(Weapon~Relationship)

