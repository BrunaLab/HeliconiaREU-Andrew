# Calculating linear distance to nearest edge for each 1-ha plot ------------
rm(list=ls())
library(tidyverse)
library(here)

data <- read_rds(here("data","one_ha_coords_updated.rds"))

EST_1_ha <- data %>%
  filter(habitat == "1-ha" & ranch == "Esteio-Colosso") %>%
  filter(!is.na(x_final) & !is.na(y_final))

?is.na

EST_simple <- EST_1_ha %>%
  group_by(ha_id_number,row,column) %>%
  summarize(x=unique(x_final), y= unique(y_final))
  

str(EST_simple)
EST_simple$column <- as.character(EST_simple$column)
str(EST_simple)

length(EST_simple$x) # 192 length, index for for loop

# list of equidistant points for Colosso, can define each edge orientation by what 
# is below the curve, to the left, to the right, and above and below the curve.

 test_var <-seq(19,81,1)
y <- seq(0,31,1)
y2 <- sort(y,decreasing=TRUE)
y2 <- y2[-1]
y_final <- c(y,y2)

y_final

plot(test_var,y_final,type="b")

# Can describe the function, then use it to test against points
#to then determine what edge they are closest to and what method to use to find linear distance

# its an absolute value function of the form: y = -|x-50|+31

abs_value_Colosso <- function(x_value) {
  y = -abs(x_value-50)+31
  return(y)
}

abs_value_Colosso(19)


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
Edge_Orientation <-NULL
distance_to_edge <- NULL
ha_id_number <- EST_x_over50$ha_id_number

# split Colosso data into three cases based on domain values of x 
EST_x_under50 <- EST_simple[EST_simple$x < 50,]
EST_x_over50 <- EST_simple[EST_simple$x >50,]
EST_x_at50 <- EST_simple[EST_simple$x == 50,] # 0 observations

for(i in 1:72) {
  if (EST_x_over50$y[i] > abs_value_Colosso(EST_x_over50$x[i])){
    distance_to_edge[i] <- 101-EST_x_over50$x[i]
    Edge_Orientation[i] <- "West"
  }
  else if (EST_x_over50$y[i] < abs_value_Colosso(EST_x_over50$x[i])){
    distance_to_edge[i] <- EST_x_over50$y[i] + 20
    Edge_Orientation[i] <- "North"
    
  }
  else if (EST_x_over50$y[i] == abs_value_Colosso(EST_x_over50$x[i])){
    distance_to_edge[i] <- EST_x_over50$y[i] + 20
    Edge_Orientation[i] <- "Equidistant; NW"
  }
}

df_over50 <- data.frame(ha_id_number,distance_to_edge,Edge_Orientation)

for(i in 1:120) {
  if(EST_x_under50$y[i] > abs_value_Colosso(EST_x_under50$x[i])) {
    distance_to_edge[i] <- EST_x_under50$x[i]+1
    Edge_Orientation[i] <- "East"
  }
  else if (EST_x_under50$y[i] < abs_value_Colosso(EST_x_under50$x[i])) {
          distance_to_edge[i] <- EST_x_under50$y[i]
          Edge_Orientation[i] <- "North"
  }
  else if (EST_x_under50$y[i] == abs_value_Colosso(EST_x_under50$x[i])) {
          distance_to_edge[i] <- EST_x_under50$x + 1
          Edge_Orientation[i] <- "Equidistant; NE"
  }
}
ha_id_number <- EST_x_under50$ha_id_number
df_under50 <- data.frame(ha_id_number,distance_to_edge,Edge_Orientation)

Colosso_1ha.df <- rbind(df_under50,df_over50)

