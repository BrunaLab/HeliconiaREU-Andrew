## Exploring options for distributions to use with growth GLMs

# packages ----------------------------------------------------------------

library(tidyverse)
library(performance)
library(lme4)
library(car)
library(here)

# load and clean up data ---------------------------------------------------------------
dist_data <- read_rds(here("data","10m_resolution_1ha_dists.rds"))

# size --------------------------------------------------------------------

#one option is to just model size instead of growth

m_size <- lmer(log_size ~ log_size_prev + dist_near*dist_next + (1|year),
                data = dist_data)

check_model(m_size)
#not terrible, but residuals are a little leptokurtic


# growth ------------------------------------------------------------------

# calculate some different measures of growth
df <-
  dist_data %>% 
  mutate(fold_change = size / size_prev,
         log2_fc = log(fold_change, base = 2), #Interpret as number of doublings.  log2 fold change of 3 = fold change of 8 or 2*2*2 times bigger than in previous year (3 doublings)
         growth = size - size_prev,
         percent_growth = growth/size_prev)

# look at distribution of response variables
qplot(df$log_size)
qplot(df$fold_change)
qplot(df$growth)
qplot(df$percent_growth)
qplot(df$log2_fc) #this is the most promising looking distribution.  
qplot(log(df$percent_growth + 1)) #exactly the same distribution because mathematically related.
m_l2fc <- lmer(log2_fc ~ dist_near*dist_next + (1|year),
                data = df)

check_model(m_l2fc)
#not perfect, but not terrible.  Stil leptokurtic

# with "scat" distribution ------------------------------------------------

# I think the most correct distribution for these leptokurtic residuals is the scaled T distribution, but it's not available for glm() or glmer().  It is available for gam(), and might be worth it?
library(mgcv)
library(gratia) #for visualizaing gams

m_gam <- 
  gam(
    log2_fc ~ dist_near*dist_next + s(year, bs = "re"), #the GAM way of doing random effects
    data = df,
    family = scat #scaled T for leptokurtic data
  )

gratia::appraise(m_gam) #check model performance
#qq plot looks better

AIC(m_gam, m_l2fc)
#AIC for the gam with the scaled t distribution is lower even though df is a lot higher (because of how gam() fits random effects).  So maybe worth doing?
