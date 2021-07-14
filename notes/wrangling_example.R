
# Load packages -----------------------------------------------------------

library(tidyverse)
library(here) #for reproducible file paths.  Will discuss next week in lab meeting

# read in data ------------------------------------------------------------

coords_09 <- 
  read_csv(here("data", "09_coords.csv"), col_types = cols(
    #make sure these columns don't get mistaken for numeric
    plot = col_character(), 
    bdffp_reserve_no = col_character()
  ))

coords_09
# These contain position within each 10m x 10m grid cell.  All you need to join
# it to other data sets is the `ha_id_number`, which is a unique ID for each
# plant.

grid_coords <- read_csv(here("data", "grid_coords.csv"))
grid_coords
# contains row and column each plant is in within plots.


ha <- 
  read_csv(here("data", "cleaned_ha_data.csv"), col_types = cols(
  #make sure these columns don't get mistaken for numeric
  plot = col_character(), 
  bdffp_reserve_no = col_character()
))

ha

# `ha`: Demograhpic data for all plants
# 
# - `ranch` (character): ranch name
# - `bdffp_reserve_no` (character): official reserve number
# - `plot` (character): plot ID
# - `habitat` (character): CF = continuous forest, 1-ha = fragment
# - `ha_id_number` (numeric): unique plant ID number
# - `year` (numeric): year of survey (surveys conducted in Feb)
# - `ht` (numeric): height in cm
# - `ht` (numeric): height in cm
# - `ht_prev` (numeric): height in cm in previous year
# - `shts` (integer): number of shoots
# - `shts_prev` (integer): number of shoots in prev year
# - `size` (numeric): shts * ht
# - `size_prev` (numeric): shts * ht in previous year
# - `log_size` (numeric): natural log of size
# - `log_size_prev` (numeric): natural log of size in prev year
# - `infl_num` (numeric): number of inflorescences
# - `flwr` (numeric): is the plant flowering? 1 = yes, 0 = no
# - `flwr_prev` (numeric): did it flower in previous year?
# - `surv` (numeric): 1 = alive, 0 = dead
# - `code_notes` (character): notes
# - `code2` (character): notes


# Join grid coord data ----------------------------------------------------
grid_coords_simple <- 
  grid_coords %>% 
  group_by(ha_id_number) %>% #only need a single row for each plant
  summarize(row = unique(row),
            column = unique(column))
ha2 <- left_join(ha, grid_coords_simple, by = "ha_id_number")

# Adjust coordinates. ------------------------------------

# example data
dimona_1ha <- 
  ha2 %>%
  filter(plot == first(plot)) %>% 
  select(ranch, plot, habitat, ha_id_number, year, ht, row, column) #just grabbing a few columns as an example

# The approach I used was to create a dataframe relating row and column numbers
# in each plot to x and y coords. Then I added those numbers to the x and y in
# coords_09 to get actual position in the plot. I *might* be making some
# incorrect assumptions about what the numbers in coords_09 mean though.

# made this from looking at the slides and decidng which corner is 0,0
dimona_layout <- 
  expand_grid(
  tibble(row = LETTERS[1:5],
         plot_y = 5:1),
  tibble(column = 1:10,
         plot_x = 0:9)
) %>%
  add_column(plot = "2107")

dimona_layout

left_join(dimona_1ha, dimona_layout)
# Then you'd want to join this to the coord_09 data and add plot_y and plot_x to
# those x and y coords

  
