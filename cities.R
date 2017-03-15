library(tidyverse);source("clean.R")

homicides <- clean()
homicides %>% 
  inner_join(count(., City, sort = T) %>% slice(1:10) %>% select(-n)) %>% 
  count(Year, City, sort = T) %>% 
  ggplot(aes(x = Year, y = n)) +
    geom_line() + facet_wrap(~City, scales = "free")
