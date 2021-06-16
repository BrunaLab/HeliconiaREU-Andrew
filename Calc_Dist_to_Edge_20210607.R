
# Overview ------------------------------------------------------------------

# Andrew Mercadante
# REU Project Summer 2021
# 
# This code measures the linear distance of a point in a 1-ha fragment
# (e.g., the xy coordinates of a plant) to the nearest edge of that fragment.



# loading packages --------------------------------------------------------

install.packages("Rfast") # Need this package for the nth() function
library(Rfast)
library(tidyverse)
library(here)


# loading datasets --------------------------------------------------------

data <- read_rds(here("data","one_ha_coords_updated.rds")) # reading in .rds file


# data organization -------------------------------------------------------

# create a dataframe for each fragment 

# COLOSSO 1-HA
EST_1_ha <- data %>%
  filter(habitat == "1-ha" & ranch == "Esteio-Colosso") %>% #filtering data
  filter(!is.na(x_final) & !is.na(y_final))

EST_simple <- EST_1_ha %>% 
  group_by(ha_id_number,row,column) %>%
  summarize(x=unique(x_final), y= unique(y_final))
  

str(EST_simple)
EST_simple$column <- as.character(EST_simple$column)
str(EST_simple)
length(EST_simple$x) # 192 length, index for for loop

# DIMONA 1-HA
Dimona_2107_1ha <- data %>%
  filter(bdffp_reserve_no == "2107" & habitat == "1-ha") %>%
  filter(!is.na(x_final) & !is.na(y_final))

Dimona_2107_1ha_simple <- Dimona_2107_1ha %>%
  group_by(ha_id_number,row,column) %>%
  summarize(x=unique(x_final), y=unique(y_final))

# PORTO ALEGRE 1-HA
Porto_Alegre_1ha <- data %>%
  filter(habitat == "1-ha" & ranch == "PortoAlegre") %>%
  filter(!is.na(x_final) & !is.na(y_final))

Porto_Alegre_simple <- Porto_Alegre_1ha %>%
  group_by(ha_id_number,row,column) %>%
  summarize(x= unique(x_final),y=unique(y_final)) 

# A generic way to check if they have the same number of plants is 
# to ask TRUE/FALSE if the number of unique id's in each df are equal
# that way you don't have to know the exact number (in this case 210).
# be sure to use the == (double equals).

length(unique(Porto_Alegre_1ha$ha_id_number)) == 
  length(unique(Porto_Alegre_simple$ha_id_number))


length(unique(Dimona_2107_1ha$ha_id_number)) == 
  length(unique(Dimona_2107_1ha_simple$ha_id_number))

length(unique(EST_1_ha$ha_id_number)) == 
  length(unique(EST_simple$ha_id_number))


# ----------------------------- New Method, exhaustive --------------

EST_simple
distance_to_N_edge <- NULL # set up variables to store distances to all edges
distance_to_S_edge <- NULL
distance_to_E_edge <- NULL
distance_to_W_edge <- NULL

for (i in 1:length(EST_simple$x)) { # for loop that calculates linear distance to each edge
  distance_to_N_edge[i] <- EST_simple$y[i] + 20 
  distance_to_E_edge[i] <- EST_simple$x[i] 
  distance_to_W_edge[i] <-100-EST_simple$x[i]
  distance_to_S_edge[i] <- EST_simple$y[i] + 50
}


ha_id_number <- EST_simple$ha_id_number # need to make this a variable so I can add it  
                                        # into a new dataframe to allow for a join later. 

distances <- data.frame(ha_id_number,distance_to_N_edge,distance_to_E_edge,
                        distance_to_W_edge,distance_to_S_edge)

Colosso_1ha <- left_join(EST_simple,distances) # join dataframes

#distances$dist_to_nearest_edge <- apply(distances,1,FUN=min) this is how to get first edge

# How to get second closest edge?
distance_to_nearest_edge <- NULL # set up variables to store the two lowest values from the 4 distances
distance_to_next_nearest_edge <- NULL

for(i in 1:length(EST_simple$x)){
distances$distance_to_nearest_edge[i] <-apply(distances[i,2:5],1,FUN=min) # apply min() to 2nd-5th column of ith row
distance.temp <- as.matrix(distances[i,2:5]) # convert to matrix so nth() can work
distances$distance_to_next_nearest_edge[i] <- Rfast::nth(distance.temp,2,descending=F) # run nth() to select 2nd value 
                                                                                       # in a vector of increasing distances 
}

Colosso_1ha <- left_join(EST_simple,distances) # join the previous Colosso data with newly aquired distances df

# Porto Alegre 1-ha fragment -----------------------------------------------


distance_to_N_edge <- NULL # setup distance variables
distance_to_E_edge <- NULL
distance_to_W_edge <- NULL
distance_to_S_edge <- NULL

for ( i in 1:length(Porto_Alegre_simple$x)) { # for loop for calculating distance to edge
  distance_to_N_edge[i] <- 50-Porto_Alegre_simple$y[i]
  distance_to_E_edge[i] <- Porto_Alegre_simple$x[i]
  distance_to_W_edge[i] <- 100- Porto_Alegre_simple$x[i]
  distance_to_S_edge[i] <- Porto_Alegre_simple$y[i] + 50
}
ha_id_number <- Porto_Alegre_simple$ha_id_number
distances_Alegre <- data.frame(ha_id_number,distance_to_N_edge,distance_to_E_edge, # form df of distances w/ ha_id_number
                                          distance_to_W_edge,distance_to_S_edge)

distance_to_nearest_edge <- NULL # initialize variables to store two lowest distance values
distance_to_next_nearest_edge <- NULL

for(i in 1:length(Porto_Alegre_simple$x)){
  distances_Alegre$distance_to_nearest_edge[i] <-apply(distances_Alegre[i,2:5],1,FUN=min) # apply min() to 2nd-5th columns of ith row
  distance.temp <- as.matrix(distances_Alegre[i,2:5]) # store as matrix to allow nth() to work
  distances_Alegre$distance_to_next_nearest_edge[i] <- Rfast::nth(distance.temp,2,descending=F)
}

Porto_Alegre_1ha_join <- left_join(Porto_Alegre_simple,distances_Alegre) # join two dfs


# Dimona 1-ha fragment 2107 ------------------------------------------

distance_to_N_edge <- NULL # same as above
distance_to_E_edge <- NULL
distance_to_W_edge <- NULL
distance_to_S_edge <- NULL

distance_to_nearest_edge <- NULL # same as above
distance_to_next_nearest_edge <- NULL

for (i in 1:length(Dimona_2107_1ha_simple$x)) { # for loop for calculating linear distance to edge
  distance_to_N_edge[i] <- Dimona_2107_1ha_simple$y[i] + 50
  distance_to_E_edge[i] <- Dimona_2107_1ha_simple$x[i]
  distance_to_W_edge[i] <- 100-Dimona_2107_1ha_simple$x[i]
  distance_to_S_edge[i] <- 50-Dimona_2107_1ha_simple$y[i] 
}

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
