library(tidyverse)
library(here)
library(car)
library(broom)
library(lme4)
ha_data <-read_rds(here("data","full_1ha_location_data.rds"))
data <- read_rds(here("data","one_ha_coords_updated.rds"))
# simple logistic regression --------------------------------------------------------

survival.fit <- glm(surv ~ distance_to_nearest_edge, data=ha_data, family=binomial)
summary(survival.fit) #not the p-values you're looking for!
# back transform log-odds to a probability
log_odds <-survival.fit$coef[2]
probability <- plogis(log_odds)

car::Anova(survival.fit) #these are the p-values you want.

confint(survival.fit)
# Not significant results given this data. SE is fairly large, 95% confidence interval overlaps zero

#plot fitted line from model with confidence bands:

plotdf <-
  broom::augment(survival.fit,
                 se_fit = TRUE, #yes, calculate confidence bands
                 type.predict = "response") #back-transform calculations to probability

ggplot(plotdf, aes(x = distance_to_nearest_edge)) +
  geom_line(aes(y = .fitted)) +
  # confidence bands:
  geom_ribbon(aes(ymin = .fitted - .se.fit, ymax = .fitted + .se.fit), alpha =0.4) +
  # shows x-values of points along bottom:
  geom_rug()

#survival is *higher* near the edge. Interesting! 
# Logistic regression for flowering probability --------------------------------
flower.fit <- glm(flwr~distance_to_nearest_edge,data=ha_data,family=binomial)
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

ggplot(data=ha_data, aes(x=year , y= ht, color=shts))+
  geom_point()+
  facet_wrap(~plot)
ha_data$year <- as.factor(ha_data$year)# had to do this to plot year as linetype or color..
ggplot(data=ha_data)+
  geom_boxplot(aes(x=year,y=ht))+
  facet_wrap(~plot)

# look at how height distributions changes over time ----6-25-2021--------------

ggplot(data=ha_data, aes(x=ht, color= year))+
  geom_density()+
  facet_wrap(~plot)
# easier to see how year-to-year plant height changed within plots
# seems like height distributions are right-skewed, not normal. 

ggplot(data=ha_data, aes(x=ht, color=plot))+
  geom_density()+
  facet_wrap(~year)
# Helps me see how plant height distributions vary among plots within a given year. 

#Histogram of plants at varying distances from edge. 







# Look at survival using lme4 package and glmer function -----------------------
survival2.fit <- glmer(surv~size_prev+distance_to_nearest_edge+(1|year),data=ha_data,
                       family=binomial,nAGQ=25) #uses GHQ method, 25 iterations
summary(survival2.fit)



