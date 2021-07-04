# Analysis and Data Visualization of rough dist dataset 

library(tidyverse)
library(here)
library(broom)
library(lme4)
library(MASS)
library(car)
library(sn)
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
plogis(surv_dist_near.fit$coef[2]) #.502 # think this means with every unit increase (10m) in dist_near
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
  filter(!is.na(log_size_prev & !is.na(surv)) & !is.)

surv.mixeff.fit <- glmer(surv~size_prev+(1|year),data=dist_data_no_NA,
                         family=binomial)
summary(surv.mixeff.fit)

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

#flower prob with ha_id_number and year as random effects 
flower.ind <- glmer(flwr~size+flwr_prev+dist_near+(1|year)+(1|ha_id_number),data=dist_data_flwr,family=binomial)
summary(flower.ind) # 1353

# Tackling growth and how to model it.
dist_data_noNA <- dist_data %>%
  filter(!is.na(dist_data$size_prev))
length(unique(dist_data_noNA$ha_id_number))
count <-0
num<- NULL

dist_data_growth<-dist_data_noNA %>% 
  mutate(dist_data_noNA,growth = (size-size_prev)/size_prev) %>%
  view()

data_finite <- dist_data_growth %>%
  filter(is.finite(growth))

# some data visualization -----------------------------------------------------
ggplot(data=data_finite)+ 
  geom_jitter(aes(x=dist_near,y=growth,color=dist_next))+
  facet_wrap(~year)

ggplot(data=data_finite)+
  geom_jitter(aes(x=dist_near,y=dist_next,size=growth,color=growth<0))

ggplot(data=data_finite)+
  geom_jitter(aes(x=dist_near,y=dist_next,size=growth,color=growth>2))


ggplot(data=data_finite)+
  geom_density(aes(x=growth)) #clearly non-normal, has negative values. 


skew_model <- selm(data_finite$growth~1)
summary(skew_model)

#fit density function of skew-normal using estimates from model against actual binned data

hist(data_finite$growth,prob=TRUE,nclass="scott") 
plot(function(x)dsn(x,dp=skew_model@param$dp),from = -1.0, to = 64, col="red",add=TRUE) #

# much more mass at peak than predicted by skewed normal distribution. 

skew_t <- selm(data_finite$growth~1,family="ST")
hist(data_finite$growth,prob=TRUE,nclass="scott") 
plot(function(x)dst(x,dp=skew_t@param$dp),from = -1.0, to = 64, col="red",add=TRUE)
# skewed t-dist seems to be almost a perfect fit. 

  # Not sure glm() is appropriate for analysis here? Would I need a custom
# distribution matching this one to be used when performing MLE? 

# test for normally distributed if log-transformed

log_growth<-log(data_finite$growth) # log transforming it produces lots of NaNs

# Ultimately what family-link combination will allow me to create linear predictor
# terms that don't result in non-normally distributed residuals, if family=gaussian?
# Is a normal distribution used in MLE methods appropriate?


# My understanding has changed slightly, GLM family should be relevant to the 
# CONDITIONAL distribution of the response variable (Y|Xi). 

# Need to understand how to plot these better, especially with multiple predictors??


ggplot(data=data_finite,aes(x= jitter(dist_near),y=growth))+
  geom_point()+
  stat_smooth()
?geom_smooth

# Analysis of growth 

growth.null <- glm(growth~1,data=data_finite,family=gaussian)
summary(growth.null)

growth.dist.fit <- glm(growth~dist_near,data=data_finite,family=gaussian)
summary(growth.dist.fit)

plot(growth.dist.fit$fitted.values,growth.dist.fit$residuals)

plot(growth.dist.fit) # provides series of graph, most seem to indicate this 
# isn't a great-fitting model. 


# Look at alternative ways to define growth. 













