
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
  summarize(x= unique(x_final),y=unique(y_final)) # 210


length(unique(Porto_Alegre_1ha$ha_id_number)) # 210 checks out, no missing plants


# calculations of distance to edge  ---------------------------------------


str(EST_simple)
EST_simple$column <- as.character(EST_simple$column)
str(EST_simple)

length(EST_simple$x) # 192 length, index for for loop

# list of equidistant points for Colosso, can define each edge orientation by what 
# is below the curve, to the left, to the right, and above and below the curve.

 test_var <-seq(20,80,1)
y <- seq(0,30,1)
y2 <- sort(y,decreasing=TRUE)
y2 <- y2[-1]
y_final <- c(y,y2)

y_final

plot(test_var,y_final,lty=1,type="b")

# Can describe the function, then use it to test against points
#to then determine what edge they are closest to and what method to use to find linear distance

# its an absolute value function of the form: y = -|x-50|+30

abs_value_Colosso <- function(x_value) {
  y = -abs(x_value-50)+30
  return(y)
}

abs_value_Colosso(20)


# test a conditional with this function
for (i in 1:192) {
if(abs_value_Colosso(EST_simple$x[i]) > EST_simple$y[i]) {
  print("Yes")

}
  else {
    print("no")
  }
}  
warnings() 
EST
EST_x <- EST_simple$x
EST_y <- EST_simple$y
Edge1_Orientation <-NULL
Edge2_Orientation <- NULL
distance_to_nearest_edge <- NULL
distance_to_next_nearest_edge <- NULL
ha_id_number <- EST_x_over50$ha_id_number

# split Colosso data into three cases based on domain values of x 
EST_x_under50 <- EST_simple[EST_simple$x < 50,]
EST_x_over50 <- EST_simple[EST_simple$x >50,]
EST_x_at50 <- EST_simple[EST_simple$x == 50,] # 0 observations

for(i in 1:72) {
  if (EST_x_over50$y[i] > abs_value_Colosso(EST_x_over50$x[i])){
    distance_to_nearest_edge[i] <- 101-EST_x_over50$x[i]
    Edge1_Orientation[i] <- "West"
  }
  else if (EST_x_over50$y[i] < abs_value_Colosso(EST_x_over50$x[i])){
    distance_to_nearest_edge[i] <- EST_x_over50$y[i] + 20
    Edge1_Orientation[i] <- "North"
    
  }
  else if (EST_x_over50$y[i] == abs_value_Colosso(EST_x_over50$x[i])){
    distance_to_nearest_edge[i] <- EST_x_over50$y[i] + 20
    Edge1_Orientation[i] <- "Equidistant; NW"
  }
}


# ----------------------------- New Method, exhaustive --------------

EST_simple
distance_to_N_edge <- NULL
distance_to_S_edge <- NULL
distance_to_E_edge <- NULL
distance_to_W_edge <- NULL

for (i in 1:192) {
  distance_to_N_edge[i] <- EST_simple$y[i] + 20
  distance_to_E_edge[i] <- EST_simple$x[i] 
  distance_to_W_edge[i] <-100-EST_simple$x[i]
  distance_to_S_edge[i] <- EST_simple$y[i] + 50
}


ha_id_number <- EST_simple$ha_id_number
distances <- data.frame(ha_id_number,distance_to_N_edge,distance_to_E_edge,
                        distance_to_W_edge,distance_to_S_edge)
Colosso_1ha <- left_join(EST_simple,distances) # join dataframes

#distances$dist_to_nearest_edge <- apply(distances,1,FUN=min) this is how to get first edge

# How to get second closest edge?
distance_to_nearest_edge <- NULL
distance_to_next_nearest_edge <- NULL

for(i in 1:192){
distances$distance_to_nearest_edge[i] <-apply(distances[i,2:5],1,FUN=min)
distance.temp <- as.matrix(distances[i,2:5]) # convert to matrix so nth() can work
distances$distance_to_next_nearest_edge[i] <- nth(distance.temp,2,descending=F)
}
warnings()
Colosso_1ha <- left_join(EST_simple,distances)

# Porto Alegre 1-ha fragment -----------------------------------------------


distance_to_N_edge <- NULL
distance_to_E_edge <- NULL
distance_to_W_edge <- NULL
distance_to_S_edge <- NULL

for ( i in 1:210) {
  distance_to_N_edge[i] <- 50-Porto_Alegre_simple$y[i]
  distance_to_E_edge[i] <- Porto_Alegre_simple$x[i]
  distance_to_W_edge[i] <- 100- Porto_Alegre_simple$x[i]
  distance_to_S_edge[i] <- Porto_Alegre_simple$y[i] + 50
}
ha_id_number <- Porto_Alegre_simple$ha_id_number
distances_Alegre <- data.frame(ha_id_number,distance_to_N_edge,distance_to_E_edge,
                                          distance_to_W_edge,distance_to_S_edge)

distance_to_nearest_edge <- NULL
distance_to_next_nearest_edge <- NULL

for(i in 1:210){
  distances_Alegre$distance_to_nearest_edge[i] <-apply(distances_Alegre[i,2:5],1,FUN=min)
  distance.temp <- as.matrix(distances_Alegre[i,2:5])
  distances_Alegre$distance_to_next_nearest_edge[i] <- nth(distance.temp,2,descending=F)
}

Porto_Alegre_1ha_join <- left_join(Porto_Alegre_simple,distances_Alegre)


# Dimona 1-ha fragment 2107 ------------------------------------------

distance_to_N_edge <- NULL
distance_to_E_edge <- NULL
distance_to_W_edge <- NULL
distance_to_S_edge <- NULL

distance_to_nearest_edge <- NULL
distance_to_next_nearest_edge <- NULL

for (i in 1:177) {
  distance_to_N_edge[i] <- Dimona_2107_1ha_simple$y[i] + 50
  distance_to_E_edge[i] <- Dimona_2107_1ha_simple$x[i]
  distance_to_W_edge[i] <- 100-Dimona_2107_1ha_simple$x[i]
  distance_to_S_edge[i] <- 50-Dimona_2107_1ha_simple$y[i] 
}

ha_id_number <- Dimona_2107_1ha_simple$ha_id_number
distances_Dimona_2107 <- data.frame(ha_id_number,distance_to_N_edge,distance_to_E_edge,
                                    distance_to_W_edge,distance_to_S_edge)

for (i in 1:177) {
  distances_Dimona_2107$distance_to_nearest_edge[i] <- apply(distances_Dimona_2107
                                                             [i,2:5],1,FUN=min)
  distance.temp <- as.matrix(distances_Dimona_2107[i,2:5])
  distances_Dimona_2107$distance_to_next_nearest_edge[i] <-nth(distance.temp,2,descending=F)
}

Dimona_2107_1ha_join <- left_join(Dimona_2107_1ha_simple,distances_Dimona_2107)
