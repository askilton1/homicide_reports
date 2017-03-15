homicides %>% 
  summarise_if(is.character, function(col) length(unique(col))) %>% 
  gather(var, n_unique)

homicides %>% count(City, sort = T)

homicides %>% 
  summarise_all(function(col) sum(is.na(col))) %>% 
  gather(var, n_missing) %>% 
  arrange(desc(n_missing))
library(randomForest)
set.seed(1)

homicides_for_rf <- homicides %>% 
  select(-RecordID, -AgencyCode, -AgencyName, -City) %>% 
  select(-contains("Age")) %>% # too many missing variables
  sample_n(1000) %>% na.omit %>% 
  mutate_if(is.character, as.factor)

rfs <- homicides %>% 
  select(-RecordID, -AgencyCode, -AgencyName, -City) %>% 
  filter(VictimSex != "Unknown") %>% 
  group_by(VictimSex) %>% 
  na.omit %>% 
  sample_n(5000) %>% 
  mutate_if(is.character, as.factor) %>% #count(VictimSex, CrimeType)
  nest(-VictimSex) %>% 
  mutate(rf_model = map(data, ~ randomForest(VictimAge ~ Weapon + Relationship + Year + Month + State + PerpetratorRace + VictimRace, data = ., importance = T)))


rfs$rf_model[[1]] %>% importance

importance(rf)
