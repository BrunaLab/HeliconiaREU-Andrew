# mean survival in full dataset
data %>%
  group_by(habitat) %>% 
  summarize(mean(surv))
  
# mean survival in subset of plants with location data
full %>% 
  summarize(mean(surv))

# Mean survival in subset of plants that existed in 2009
data_2009 <- 
  data %>% 
  filter(year == 2009) 

plants_2009 <- unique(data_2009$ha_id_number) #pull unique ID numbers in 2009

data_sub <- data %>% 
  #only those plants that existed in 2009, but all years of data for them
  filter(ha_id_number %in% plants_2009) 

data_sub$year %>% range() #still has full range of years

data_sub %>% 
  group_by(habitat) %>% 
  summarize(mean(surv))
