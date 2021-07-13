# Calculating distance to edge at broader scale. 
#rm(list=ls())
library(here)
library(tidyverse)
alt_data <- read_csv(here("data", "cleaned_ha_data_2021-07-01.csv"),
                     col_types = cols(plot = col_character(),
                                      bdffp_reserve_no = col_character()))

# Porto-Alegre 1-ha fragment -------------------------------------------------
Porto_Alegre_1ha <- 
  alt_data %>%
  filter(ranch == "PortoAlegre" & habitat == "1-ha") %>%
  filter(!is.na(row) & !is.na(column))

PortoAlegre_1ha_layout <-  # assuming bottom-left is origin, and E1 is at (0,0)
  expand_grid(
    tibble(row = LETTERS[5:1],
           plot_y = seq(0,40,10)),  
    tibble(column = 1:10,
           plot_x = seq(0,90,10))
  ) %>%
  add_column(plot = "5753")

PortoAlegre_1ha_join <- left_join(Porto_Alegre_1ha,PortoAlegre_1ha_layout)
str(PortoAlegre_1ha_layout)

Porto_Alegre_simple <-
  PortoAlegre_1ha_join %>%
  group_by(ha_id_number,plot,row,column) %>%
  mutate(rough_x = plot_x + 5, #to place the coord at the midpoint?
         rough_y = plot_y + 5) %>%
  summarise(x=unique(rough_x),y=unique(rough_y))

# setup distance variables
dist_N <- 50-Porto_Alegre_simple$y
dist_E <- Porto_Alegre_simple$x
dist_W <- 100- Porto_Alegre_simple$x
dist_S <- Porto_Alegre_simple$y + 50

ha_id_number <- Porto_Alegre_simple$ha_id_number
distances_Alegre <-
  # form df of distances w/ ha_id_number
  data.frame(ha_id_number, dist_N,  dist_E, dist_W, dist_S)


dist_near<- NULL
dist_next <- NULL

for(i in 1:210){
  distances_Alegre$dist_near[i] <- apply(distances_Alegre[i,2:5],1,FUN=min)
  distance.temp <- as.matrix(distances_Alegre[i,2:5])
  distances_Alegre$dist_next[i] <- Rfast::nth(distance.temp,2,descending=F)
}

Porto_Alegre_1ha_final <- left_join(Porto_Alegre_simple,distances_Alegre)


# Colosso -----------------------------------------------------------------

EST_1ha <-
  alt_data %>%
  filter(ranch == "Esteio-Colosso" & habitat == "1-ha") %>%
  filter(!is.na(row) & !is.na(column))
  
EST_COL_1ha_layout <-
  expand_grid(
    tibble(row = LETTERS[1:5], 
           plot_y =seq(0,40,10)), 
    tibble(column = 10:1,
           plot_x = seq(0,90,10))
  ) %>%
  add_column(plot="5751")

EST_1ha_join1 <- left_join(EST_1ha,EST_COL_1ha_layout)

EST_simple <- EST_1ha_join1 %>% 
  group_by(ha_id_number,plot,row,column) %>%
  mutate(rough_x = plot_x + 5,
         rough_y = plot_y + 5) %>%
  summarize(x= unique(rough_x), y= unique(rough_y))

dist_N <- EST_simple$y + 20
dist_E <- EST_simple$x 
dist_W <- 100 - EST_simple$x
dist_S <- EST_simple$y + 50

ha_id_number <- EST_simple$ha_id_number # need to make this a variable so I can add it  
# into a new dataframe to allow for a join later. 

distances_EST <- data.frame(ha_id_number, dist_N, dist_E, dist_W, dist_S)
dist_near <- NULL # set up variables to store the two lowest values from the 4 distances
dist_next <- NULL 

#Sorting and using the nth() function to find next-nearest dist
for(i in 1:length(EST_simple$x)){
  distances_EST$dist_near[i] <-apply(distances_EST[i,2:5],1,FUN=min) # apply min() to 2nd-5th column of ith row
  distance.temp <- as.matrix(distances_EST[i,2:5]) # convert to matrix so nth() can work
  distances_EST$dist_next[i] <- Rfast::nth(distance.temp,2,descending=F) # run nth() to select 2nd value 
  # in a vector of increasing distances 
}

Colosso_1ha <- left_join(EST_simple, distances_EST) # join the previous Colosso data with newly aquired distances df


# Dimona 1-ha fragment 2107 ------------------------------------------
Dimona_2107_1ha <-
  alt_data %>%
  filter(bdffp_reserve_no == "2107" & habitat == "1-ha") %>%
  filter(!is.na(row) & !is.na(column))

dimona_1_layout <- # Assumes bottom-left origin and E1 is at (0,0)
  expand_grid(
    tibble(row = LETTERS[5:1], 
           plot_y = seq(0,40,10)), 
    tibble(column = 1:10,
           plot_x = seq(0,90,10))
  ) %>%
  add_column(plot = "2107")

Dimona_2107_join <- left_join(Dimona_2107_1ha,dimona_1_layout)

Dimona_2107_1ha_simple <- Dimona_2107_join %>%
  group_by(ha_id_number,plot,row,column)%>%
  mutate(rough_x = plot_x+5,
         rough_y = plot_y+5) %>%
  summarise(x=unique(rough_x),y=unique(rough_y))


dist_near <- NULL # same as above
dist_next <- NULL

dist_N <- Dimona_2107_1ha_simple$y + 50
dist_E<- Dimona_2107_1ha_simple$x
dist_W<- 100-Dimona_2107_1ha_simple$x
dist_S <- 50-Dimona_2107_1ha_simple$y


ha_id_number <- Dimona_2107_1ha_simple$ha_id_number
distances_Dimona_2107 <- data.frame(ha_id_number,dist_N,dist_E, # form df containing distances and ha_id_number
                                    dist_W,dist_S)

for (i in 1:length(Dimona_2107_1ha_simple$x)) {
  distances_Dimona_2107$dist_near[i] <- apply(distances_Dimona_2107
                                                             [i,2:5],1,FUN=min)
  distance.temp <- as.matrix(distances_Dimona_2107[i,2:5])
  distances_Dimona_2107$dist_next[i] <-Rfast::nth(distance.temp,2,descending=F)
}

Dimona_2107_1ha <- left_join(Dimona_2107_1ha_simple,distances_Dimona_2107) # join dfs 

# plot data to check for weirdness
ggplot(Dimona_2107_1ha, aes(x = x, y = y, color = dist_N, size = dist_E)) + geom_point(alpha = 0.7)+
  coord_fixed()
# Check that y=0 is on the north edge and x=0 is the east edge

# re-combine plots --------------------------------------------------------
# Colosso_1ha
# Dimona_2107_1ha
# Porto_Alegre_1ha_final


Dimona_2107_1ha$column <- as.double(Dimona_2107_1ha$column) # type error, wouldn't let me bind rows due to conflict
Porto_Alegre_1ha_final$column <- as.double(Porto_Alegre_1ha_final$column)
Colosso_1ha$column <- as.double(Colosso_1ha$column)


xy_dist <-
  bind_rows(
    Dimona_2107_1ha,
    Porto_Alegre_1ha_final,
    Colosso_1ha 
  )

full_test <- right_join(alt_data, xy_dist) 
                   
write_rds(full_test,here("data", "10m_resolution_1ha_dists.rds"))
  
