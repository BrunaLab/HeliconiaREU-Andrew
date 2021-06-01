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
  select(ranch,plot,habitat,ha_id_number,year,row,column)

PortoAlegre_1ha_layout <-  # assuming bottom-left is origin
  expand_grid(
    tibble(row = LETTERS[1:5],
           plot_x = 0:4), # swapped x and y here since row and column variables are swapped. 
    tibble(column = 1:10,
           plot_y = 0:9)
  ) %>%
  add_column(plot = "5753")

left_join(PortoAlegre_1ha,PortoAlegre_1ha_layout)


# Dimona layout from data-wrangling example 

dimona_1ha_1 <- 
  ha2 %>%
  filter(plot == first(plot)) %>% 
  select(ranch, plot, habitat, ha_id_number, year, ht, row, column)

dimona_1_layout <- # Assumes bottom-left origin
  expand_grid(
    tibble(row = LETTERS[5:1], 
           plot_x = 4:0), # swapped position of plot_x and plot_y to reflect data and diagrams
    tibble(column = 1:10,
           plot_y = 0:9)
  ) %>%
  add_column(plot = "2107")
dimona_1_layout
left_join(dimona_1ha_1,dimona_1_layout)

dimona_1ha_2 <- #dimona 2nd 1-ha plot 
  ha2 %>%
  filter(plot == "2108") %>%
  select(ranch, plot, habitat, ha_id_number, year, ht, row, column)

dimona_2_layout <- # Assumes top-right origin
  expand_grid(
    tibble(row = LETTERS[1:5], 
           plot_x = 0:4), # swapped position of plot_x and plot_y to reflect data and diagrams
    tibble(column = 1:10,
           plot_y = 0:9)
  ) %>%
  add_column(plot = "2108")
dimona_2_layout

left_join(dimona_2_layout,dimona)

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

# assume bottom-right point of origin
EST_COL_1ha_layout <-
  expand_grid(
    tibble(row = LETTERS[1:5], 
           plot_x = 0:4), # swapped position of plot_x and plot_y to reflect data and diagrams
    tibble(column = 1:10,
           plot_y = 0:9)
  ) %>%
    add_column(plot="5751")
left_join(Esteio_Colosso_1ha,EST_COL_1ha_layout)
