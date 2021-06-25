# Overview ------------------------------------------------------------------
rm(list=ls())
# Andrew Mercadante
# REU Project Summer 2021
# 
# This code measures the linear distance of a point in a 1-ha fragment
# (e.g., the xy coordinates of a plant) to the nearest edge of that fragment.

# loading packages --------------------------------------------------------

if(!"Rfast" %in% installed.packages()) {
  install.packages("Rfast")
} # Need this package for the nth() function
library(Rfast)
library(tidyverse)
library(here)

if(!"car" %in% installed.packages()) {
  install.packages("car")
}
library(car)

if(!"broom" %in% installed.packages()) {
  install.packages("broom")
}
library(broom)
# loading datasets --------------------------------------------------------

data <- read_rds(here("data","one_ha_coords_updated.rds")) # reading in .rds file


# data organization -------------------------------------------------------

# create a dataframe for each fragment 

# COLOSSO 1-HA
EST_1_ha <- data %>%
  filter(habitat == "1-ha" & ranch == "Esteio-Colosso") %>% #filtering data
  filter(!is.na(x_final) & !is.na(y_final))


# Simplify to one location per plant, since plants don't move

EST_simple <- EST_1_ha %>% 
  group_by(plot, ha_id_number,row,column) %>%
  summarize(x = unique(x_final), y = unique(y_final))
  

str(EST_simple)
EST_simple$column <- as.character(EST_simple$column)
str(EST_simple)
length(EST_simple$x) # 192 length, index for for loop

# DIMONA 1-HA
Dimona_2107_1ha <- data %>%
  filter(bdffp_reserve_no == "2107" & habitat == "1-ha") %>%
  filter(!is.na(x_final) & !is.na(y_final))

Dimona_2107_1ha_simple <- Dimona_2107_1ha %>%
  group_by(plot, ha_id_number,row,column) %>%
  summarize(plot = unique(plot), x=unique(x_final), y=unique(y_final))

# PORTO ALEGRE 1-HA
Porto_Alegre_1ha <- data %>%
  filter(habitat == "1-ha" & ranch == "PortoAlegre") %>%
  filter(!is.na(x_final) & !is.na(y_final))

Porto_Alegre_simple <- Porto_Alegre_1ha %>%
  group_by(plot, ha_id_number,row,column) %>%
  summarize(plot = unique(plot), x= unique(x_final),y=unique(y_final)) 

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

# Porto Alegre 1-ha fragment -----------------------------------------------


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
ggplot(Dimona_2107_1ha_join, aes(x = x, y = y, color = distance_to_N_edge, size = distance_to_E_edge)) + geom_point(alpha = 0.7)
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

full <- right_join(data %>% select(-x, -y), #get rid of old, incorrect coords which don't join up.
                   xy_dist)
range(full$distance_to_nearest_edge)
nrow(full)

# data validation ---------------------------------------------------------
# A place for tests and plots of the data to double check that you are getting sensible data.
# plot data:

p <- ggplot(full, aes(x = x, y = y)) + facet_wrap(~plot)

p +  geom_point(aes(color = distance_to_nearest_edge)) + scale_color_viridis_c()
p +  geom_point(aes(color = distance_to_next_nearest_edge)) + scale_color_viridis_c()

# do we have growth data for all ?
ggplot(full, aes(x = year, y = ht, group = ha_id_number)) + geom_line() + facet_wrap(~plot)

# simple logistic regression --------------------------------------------------------

survival.fit <- glm(surv ~ distance_to_nearest_edge, data=full, family=binomial)
summary(survival.fit) #not the p-values you're looking for!
# back transform log-odds to a probability
log_odds <-survival.fit$coef[2]
probability <- plogis(log_odds)
?plogis()
library(car)
car::Anova(survival.fit) #these are the p-values you want.

confint(survival.fit)
# Not significant results given this data. SE is fairly large, 95% confidence interval overlaps zero

#plot fitted line from model with confidence bands:
library(broom)

plotdf <-
  broom::augment(survival.fit,
                 se_fit = TRUE, #yes, calculate confidence bands
                 type.predict = "response") #back-transform calculations to probability
?augment()
ggplot(plotdf, aes(x = distance_to_nearest_edge)) +
  geom_line(aes(y = .fitted)) +
  # confidence bands:
  geom_ribbon(aes(ymin = .fitted - .se.fit, ymax = .fitted + .se.fit), alpha =0.4) +
  # shows x-values of points along bottom:
  geom_rug()

#survival is *higher* near the edge. Interesting! 
# Logistic regression for flowering probability --------------------------------
flower.fit <- glm(flwr~distance_to_nearest_edge,data=full,family=binomial)
summary(flower.fit)
log_odds_flwr <- flower.fit$coef[2]
flwr_probability <- plogis(log_odds_flwr)

car::Anova(flower.fit)
confint(flower.fit)

flowerdf <- broom::augment(flower.fit,
                           se_fit = TRUE,
                           type.predict = "response")

ggplot(flowerdf, aes(x = distance_to_nearest_edge)) +
  geom_line(aes(y = .fitted)) +
  geom_ribbon(aes(ymin = .fitted - .se.fit, ymax = .fitted + .se.fit), alpha = 0.4) # alpha modulates transparency

# Some exploratory data visualization, practicing with ggplot ------------------

ggplot(data=full, aes(x=year , y= ht, color=shts))+
  geom_point()+
  geom_smooth()+
facet_wrap(~plot)


# look at how height distributions changes over time ---------------------------
#full$year <- as.factor(full$year) # had to do this to plot year as linetype or color..
ggplot(data=full, aes(x=ht, color= year))+
 geom_density()+
  facet_wrap(~plot)
# easier to see how year-to-year plant height changed within plots
# seems like height distributions are right-skewed, not normal. 

ggplot(data=full, aes(x=ht, color=plot))+
  geom_density()+
  facet_wrap(~year)
# Helps me see how plant height distributions vary among plots within a given year. 

