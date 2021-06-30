# Calculating distance to edge at broader scale. 
rm(list=ls())
library(here)
library(tidyverse)

rough_data <-read_rds(here("data","one_ha_coords_updated.rds"))

rough_data %>%
  filter(!is.na(row) & !is.na(column)) # df same length, no missing values for row or column 

refined_data <-rough_data %>%
  filter(habitat == "1-ha") %>%
  filter(!is.na(plot_x) | !is.na(plot_y))

refined_data <-refined_data %>%
  mutate(rough_x = plot_x+5,
         rough_y = plot_y+5)
# use code from Calc_Dist_to_Edge_20210607 to calculate distance to nearest edge for 3 plots
# we have distance data for 

EST_1ha <- refined_data %>%
  filter(ranch == "Esteio-Colosso" & habitat == "1-ha")

EST_simple <- EST_1ha %>% 
  group_by(ha_id_number,plot,row,column) %>%
  summarize(x= unique(rough_x), y= unique(rough_y))
length(unique(EST_1ha$ha_id_number))

distance_to_N_edge <- NULL # set up variables to store distances to all edges
distance_to_S_edge <- NULL
distance_to_E_edge <- NULL
distance_to_W_edge <- NULL
# ERS: you might consider shorter variable names.  Maybe `dist_to_N` or even just `dist_N`?

#doesn't need to be a for-loop.  Addition is vectorized.
distance_to_N_edge <- EST_simple$y + 20
distance_to_E_edge <- EST_simple$x 
distance_to_W_edge <- 100 - EST_simple$x
distance_to_S_edge <- EST_simple$y + 50

ha_id_number <- EST_simple$ha_id_number # need to make this a variable so I can add it  
# into a new dataframe to allow for a join later. 

distances <- data.frame(ha_id_number,distance_to_N_edge,distance_to_E_edge,
                        distance_to_W_edge,distance_to_S_edge)

Colosso_1ha <- left_join(EST_simple,distances) # join dataframes

#distances$dist_to_nearest_edge <- apply(distances,1,FUN=min) this is how to get first edge

# How to get second closest edge?

distance_to_nearest_edge <- NULL # set up variables to store the two lowest values from the 4 distances
distance_to_next_nearest_edge <- NULL #ERS or maybe just `dist_nearest` and `dist_next`?

# This is awesome!  I could not think of how to get next-nearest distance.  Sorting and using the nth() function is a perfect solution!
for(i in 1:length(EST_simple$x)){
  distances$distance_to_nearest_edge[i] <-apply(distances[i,2:5],1,FUN=min) # apply min() to 2nd-5th column of ith row
  distance.temp <- as.matrix(distances[i,2:5]) # convert to matrix so nth() can work
  distances$distance_to_next_nearest_edge[i] <- Rfast::nth(distance.temp,2,descending=F) # run nth() to select 2nd value 
  # in a vector of increasing distances 
}

Colosso_1ha <- left_join(EST_simple,distances) # join the previous Colosso data with newly aquired distances df

# Porto Alegre 1-ha fragment ---------------------------------------------------
Porto_Alegre_simple <- refined_data %>%
  filter(habitat == "1-ha" & ranch == "PortoAlegre") %>%
  group_by(ha_id_number,plot,row,column) %>%
  summarise(x=unique(rough_x),y=unique(rough_y))

distance_to_N_edge <- NULL # setup distance variables
distance_to_E_edge <- NULL
distance_to_W_edge <- NULL
distance_to_S_edge <- NULL

distance_to_N_edge <- 50-Porto_Alegre_simple$y
distance_to_E_edge <- Porto_Alegre_simple$x
distance_to_W_edge <- 100- Porto_Alegre_simple$x
distance_to_S_edge <- Porto_Alegre_simple$y + 50

ha_id_number <- Porto_Alegre_simple$ha_id_number
distances_Alegre <- data.frame(ha_id_number,distance_to_N_edge,distance_to_E_edge, # form df of distances w/ ha_id_number
                               distance_to_W_edge,distance_to_S_edge)

## Tidyverse version (I think):
# distances_Alegre <-
#   Porto_Alegre_simple %>% 
#   mutate(distance_to_N_edge = 50 - y,
#          distance_to_E_edge = x,
#          distance_to_W_edge = 100 - x,
#          distance_to_S_edge = y + 50)

distance_to_nearest_edge <- NULL
distance_to_next_nearest_edge <- NULL

for(i in 1:210){
  distances_Alegre$distance_to_nearest_edge[i] <- apply(distances_Alegre[i,2:5],1,FUN=min)
  distance.temp <- as.matrix(distances_Alegre[i,2:5])
  distances_Alegre$distance_to_next_nearest_edge[i] <- Rfast::nth(distance.temp,2,descending=F)
}

## Tidyverse version (I think):
# distances_Alegre %>% 
#   rowwise() %>% 
#   mutate(nearest_dist = min(c_across(ends_with("_edge"))),
#          next_nearest_dist = nth(sort(c_across(ends_with("_edge"))), 2))
# The tidyverse version is maybe a bit less readable for this one

Porto_Alegre_1ha_join <- left_join(Porto_Alegre_simple,distances_Alegre)



# Dimona 1-ha fragment 2107 ------------------------------------------
Dimona_2107_1ha_simple <- refined_data %>%
  filter(bdffp_reserve_no == "2107" & habitat == "1-ha") %>%
  group_by(ha_id_number,plot,row,column)%>%
  summarise(x=unique(rough_x),y=unique(rough_y))

distance_to_N_edge <- NULL # same as above
distance_to_E_edge <- NULL
distance_to_W_edge <- NULL
distance_to_S_edge <- NULL

distance_to_nearest_edge <- NULL # same as above
distance_to_next_nearest_edge <- NULL

distance_to_N_edge <- Dimona_2107_1ha_simple$y + 50
distance_to_E_edge <- Dimona_2107_1ha_simple$x
distance_to_W_edge <- 100-Dimona_2107_1ha_simple$x
distance_to_S_edge <- 50-Dimona_2107_1ha_simple$y


ha_id_number <- Dimona_2107_1ha_simple$ha_id_number
distances_Dimona_2107 <- data.frame(ha_id_number,distance_to_N_edge,distance_to_E_edge, # form df containing distances and ha_id_number
                                    distance_to_W_edge,distance_to_S_edge)

for (i in 1:length(Dimona_2107_1ha_simple$x)) {
  distances_Dimona_2107$distance_to_nearest_edge[i] <- apply(distances_Dimona_2107
                                                             [i,2:5],1,FUN=min)
  distance.temp <- as.matrix(distances_Dimona_2107[i,2:5])
  distances_Dimona_2107$distance_to_next_nearest_edge[i] <-Rfast::nth(distance.temp,2,descending=F)
}

Dimona_2107_1ha_join <- left_join(Dimona_2107_1ha_simple,distances_Dimona_2107) # join dfs 

# plot data to check for weirdness
ggplot(Dimona_2107_1ha_join, aes(x = x, y = y, color = distance_to_N_edge, size = distance_to_E_edge)) + geom_point(alpha = 0.7)+
  coord_fixed()
# Check that y=0 is on the north edge and x=0 is the east edge

# re-combine plots --------------------------------------------------------
Colosso_1ha
Dimona_2107_1ha_join
Porto_Alegre_1ha_join


Dimona_2107_1ha_join$column <- as.double(Dimona_2107_1ha_join$column) # type error, wouldn't let me bind rows due to conflict
Porto_Alegre_1ha_join$column <- as.double(Porto_Alegre_1ha_join$column)
Colosso_1ha$column <- as.double(Colosso_1ha$column)


xy_dist <-
  bind_rows(
    Dimona_2107_1ha_join,
    Porto_Alegre_1ha_join,
    Colosso_1ha 
  )

full_test <- right_join(rough_data %>% select(-x, -y), #get rid of old, incorrect coords which don't join up.
                   xy_dist)
write_rds(full_test,here("data","10m_resolution_1ha_dists.rds"))
  
