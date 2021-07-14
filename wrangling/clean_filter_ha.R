
# Clean full dataset ------------------------------------------------------

library(tidyverse)
library(here)

# Read in raw data --------------------------------------------------------

ha_raw <- read_csv(here("data", "ha_data_2021-07-01.csv"),
               col_types = cols(
                 plot = col_character(),
                 bdffp_reserve_no = col_character()
               ))


# fix bdffp reserve number ------------------------------------------------

#let's change "none" to NA so it gets read in as a number correctly in the future
ha_raw$bdffp_reserve_no <- 
  replace(ha_raw$bdffp_reserve_no, ha_raw$bdffp_reserve_no == "none", NA)

# Remove size 0 -----------------------------------------------------------

# Size = 0 means there were no aboveground parts. These are mostly plants that
# completely died back or were crushed by treefalls. Emilio and I decided early
# on that this was categorically different than just being small, and we decided
# it's probably best to just exclude those plants for now.  I think we should do
# the same (at least for now) for your analyses.

# size0 <- ha_raw %>%
#   filter(size == 0)
# unique(size0$code_notes)
## seedlings, "dried", under branchfall, dead above ground (but comes back?)

ha <- 
  ha_raw %>%
  filter(size_prev > 0 | is.na(size_prev),
         size > 0 | is.na(size))
# NAs are filtered out by size > 0 because NA > 0 is NA, not TRUE.  We want to
# keep NAs because dead plants have size NA.

# Now we shouldn't have any infinite values for log_size
# ha %>% filter(is.infinite(log_size))
# ha %>% filter(is.infinite(log_size_prev))


# Write file --------------------------------------------------------------

write_csv(ha, here("data", "ha_clean.csv"))
