library(tidyverse)
library(here)

coords <- read_csv(here("data","09_coords.csv"), col_types = cols(plot=col_character(),
                                                    bdffp_reserve_no=col_character()
))


coords

grid_coords <-read_csv(here("data","grid_coords.csv"))

ha <- 
  read_csv(here("data", "cleaned_ha_data.csv"), col_types = cols(
    #make sure these columns don't get mistaken for numeric
    plot = col_character(), 
    bdffp_reserve_no = col_character()
  ))

# Join grid coord data ----------------------------------------------------
grid_coords_simple <- 
  grid_coords %>% 
  group_by(ha_id_number) %>% #only need a single row for each plant
  summarize(row = unique(row),
            column = unique(column))
ha2 <- left_join(ha, grid_coords_simple, by = "ha_id_number")


# sort to find 1-ha habitats
ha_1frag <-ha2 %>% 
  filter(habitat == "1-ha")

unique(ha_1frag$ranch)  # "Dimona" "Esteio-Colosso" and "PortoAlegre"

PortoAlegre_1ha <-ha_1frag %>%
  filter(ranch == "PortoAlegre") %>%
  select(ranch,plot,habitat,ha_id_number,year,ht,row,column)

PortoAlegre_1ha_layout <-  # assuming bottom-left is origin, and E1 is at (0,0)
  expand_grid(
    tibble(row = LETTERS[5:1],
           plot_y = seq(0,40,10)),  
    tibble(column = 1:10,
           plot_x = seq(0,90,10))
  ) %>%
  add_column(plot = "5753")

PortoAlegre_1ha_join <-left_join(PortoAlegre_1ha,PortoAlegre_1ha_layout)


# Dimona layout from data-wrangling example 

dimona_1ha_1 <- 
  ha2 %>%
  filter(plot == "2107") %>% 
  select(ranch, plot, habitat, ha_id_number, year, ht, row, column)

dimona_1_layout <- # Assumes bottom-left origin and E1 is at (0,0)
  expand_grid(
    tibble(row = LETTERS[5:1], 
           plot_y = seq(0,40,10)), 
    tibble(column = 1:10,
           plot_x = seq(0,90,10))
  ) %>%
  add_column(plot = "2107")

dimona_1_layout
dimona_1ha_1_join <- left_join(dimona_1ha_1,dimona_1_layout)

dimona_1ha_2 <- #dimona 2nd 1-ha plot 
  ha2 %>%
  filter(plot == "2108") %>%
  select(ranch, plot, habitat, ha_id_number, year, ht, row, column)

dimona_2_layout <- # Assumes bottom-left origin with A10 at (0,0)
  expand_grid(
    tibble(row = LETTERS[1:5], 
           plot_y = seq(0,40,10)), 
    tibble(column = 10:1,
           plot_x = seq(0,90,10))
  ) %>%
  add_column(plot = "2108")
dimona_2_layout

dimona_1ha_2_join <- left_join(dimona_1ha_2,dimona_2_layout)


# Writing function to repeat filtering task ----------------------------------

ha_data_filter <- function(data,ranch_name,habitat_type) {
  data %>%
    filter(ranch == ranch_name & habitat == habitat_type) %>%
    select(ranch, plot, habitat, ha_id_number, year, ht, row, column)
}

# test function -----------------------------------------------------------

ha_data_filter(ha2,"PortoAlegre","1-ha")
ha_data_filter(ha2,"Esteio-Colosso","CF")

# 1-ha Esteio-Colosso Fragment ------------------------------------------

Esteio_Colosso_1ha <- ha_data_filter(ha2,"Esteio-Colosso","1-ha")
unique(Esteio_Colosso_1ha$plot)

# assume bottom-right point of origin and A10 at (0,0)
EST_COL_1ha_layout <-
  expand_grid(
    tibble(row = LETTERS[1:5], 
           plot_y =seq(0,40,10)), 
    tibble(column = 10:1,
           plot_x = seq(0,90,10))
  ) %>%
    add_column(plot="5751")
EST_COL_1ha_join<-left_join(Esteio_Colosso_1ha,EST_COL_1ha_layout)



# test to figure out if id in excel sheet is the same as ha_id_number --

ha2 %>%
  filter(plot=="2107") %>%
  filter(ha_id_number=="100") %>%
  select(ha_id_number,row,column)

ha2 %>%
  filter(plot=="2107") %>%
  filter(ha_id_number == "262") %>%
  select(ha_id_number,row,column)

# seems like the plantID doesn't correspond with the ha_id_number in dataframe ha2


# test rbind() and gather all joins from 1-ha fragments into a single df 
Joint_data <- rbind(dimona_1ha_1_join,dimona_1ha_2_join)
Joint_data2 <- rbind(EST_COL_1ha_join,PortoAlegre_1ha_join)
Joint_data_final <- rbind(Joint_data,Joint_data2) 

ha3 <- left_join(ha2,Joint_data_final)
?join

# group coordinates in sub-plot by ha_id_number and summarize x y columns
simple_coords <-coords %>%
  group_by(ha_id_number) %>%
  summarize(x,y) %>%
  view()
# join dataframe with coordinates based on row column combinations w/ subplot coords
ha3 <-left_join(ha2,simple_coords)  

# Join this dataframe with the row-column coords for all the 1-ha fragments. 
ha4 <- left_join(ha3,Joint_data_final)

one_ha_coord_final<-
mutate(ha4,x_final = plot_x + x) %>%
mutate(ha4,y_final = plot_y + y) %>%
  view()


