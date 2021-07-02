# Analysis and Data Visualization of rough dist dataset 

library(tidyverse)
library(here)
library(broom)
library(lme4)

dist_data <-read_rds(here("data","rough_dist_data_10m.rds"))
dataCF <- read_csv(here("data","cleaned_ha_data_2021-07-01.csv"))
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



# estimate of survival for CF -------------------------------------------------

CFdata_rev <- dataCF %>%
  filter(habitat== "CF")

surv.CF <- glm(surv~1, data=CFdata_rev, family=binomial)
plogis(surv.CF$coef[1])
summary(surv.CF)

surv.CF.df <- broom::augment(surv.CF,
                             se_fit =TRUE,
                             type.predict = "response")
se <- surv.CF.df$.se.fit[1]
surv.df <- broom::augment(surv_dist_near.fit,
                          se_fit =TRUE,
                          type.predict = "response")

ggplot(data=surv.df,aes(x=dist_near))+
  geom_line(aes(y=.fitted))+
  geom_ribbon(aes(ymin = .fitted-.se.fit, ymax=.fitted+.se.fit),alpha=0.4)+
  geom_hline(aes(yintercept=0.969),color="red")+
  geom_ribbon(aes(ymin=0.969-se, ymax=0.969+se),fill="red", alpha=0.2)


# Survival model, using size_prev and dist_near as predictors

surv_size_dist.fit <- glm(surv~size_prev+dist_near,data=dist_data,family=binomial) 
summary(surv_size_dist.fit) # AIC 2840

car::Anova(surv_size_dist.fit) # within this model, dist_near is not significant predictor

# Mixed-effects model with year as random effect.
dist_data$year <- as.factor(dist_data$year)
dist_data_no_NA <- dist_data %>%
  filter(!is.na(log_size_prev & !is.na(surv)))

surv.mixeff.fit <- glmer(surv~log_size_prev+(1|year),data=dist_data_no_NA,
                         family=binomial)


# Flowering Models --------------------------------------------------------
dist_data_flwr <- dist_data %>%
  filter(!is.na(flwr))

# Null model --------------------------------------------------------------
flower.null <- glm(flwr~1, data=dist_data_flwr, family=binomial)
summary(flower.null) # AIC 2062.4

# flowering probability with dist_near as predictor -----------------------
flower.dist.fit <- glm(flwr~dist_near,data=dist_data_flwr,family=binomial)
summary(flower.dist.fit) #2062.6

flower.df <- broom::augment(flower.dist.fit,
                            se_fit=TRUE,
                            type.predict="response")


car::Anova(flower.dist.fit)

# CF flowering probability ------------------------------------------------
CF.flwr <- dataCF %>% 
  filter(habitat == "CF") %>%
  filter (!is.na(flwr))

CF.flwr.mod <- glm(flwr~1,data=CF.flwr,family=binomial)

CF.flwr.df <- broom::augment(CF.flwr.mod,
                             se_fit=TRUE,
                             type.predict="response")

se.flwr <- CF.flwr.df$.se.fit[1]
base.flwr <- CF.flwr.df$.fitted[1]

ggplot(data=flower.df,aes(x=dist_near))+
  geom_line(aes(y=.fitted))+
  geom_ribbon(aes(ymin=.fitted-.se.fit,ymax=.fitted+.se.fit),alpha=0.4)+
  geom_hline(aes(yintercept=base.flwr),color="red")+
  geom_ribbon(aes(ymin=base.flwr-se.flwr,ymax=base.flwr+se.flwr),fill="red",alpha=0.2)

#flower prob with size_prev and flwr_prev as predictors

flower.past.fit <- glm(flwr~size+flwr_prev,data=dist_data_flwr,
                       family=binomial)

summary(flower.past.fit) # AIC 1458

car::Anova(flower.past.fit)

# Tackling growth and how to model it.
