# Analysis and Data Visualization of rough dist dataset 

library(tidyverse)
library(here)
library(broom)
library(lme4)
library("numDeriv")
library(RCurl)
dist_data <-read_rds(here("data","rough_dist_data_10m.rds"))

# Survival model, intercept only --------------------------------------------

surv.null.fit <- glm(surv~1,data=dist_data,family=binomial)
summary(surv.null.fit) # AIC 3436
log_odds <- surv.null.fit$coef[1]
Prob.surv <-plogis(log_odds)


# Survival model, distance to nearest edge as predictor variable ------------

surv_dist_near.fit <- glm(surv~dist_near,data=dist_data, family=binomial)
summary(surv_dist_near.fit) #AIC 3430
plogis(surv_dist_near.fit$coef[2]) #.502 # think this means with every unit increase (5m) in dist_near
# prob of survival increases by .002, 

car::Anova(surv_dist_near.fit) # p-value = 0.006

surv.df <- broom::augment(surv_dist_near.fit,
                          se_fit =TRUE,
                          type.predict = "response")
ggplot(data=surv.df,aes(x=dist_near))+
  geom_line(aes(y=.fitted))+
  geom_ribbon(aes(ymin = .fitted-.se.fit, ymax=.fitted+.se.fit),alpha=0.4)

# Survival model, using size_prev and dist_near as predictors

surv_size_dist.fit <- glm(surv~size_prev+dist_near,data=dist_data,family=binomial) 
summary(surv_size_dist.fit) # AIC 2840

car::Anova(surv_size_dist.fit) # within this model, dist_near is not significant predictor

# Mixed-effects model with year as random effect.
dist_data$year <- as.factor(dist_data$year)
surv.mixeff.fit <- glmer(surv~size_prev+(1|year),data=dist_data,
                         family=binomial,nAGQ=25)


