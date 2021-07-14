# General Questions about modeling strategies/ best practices.

library(here)
library(tidyverse)
library(lme4)
library(MASS)
# library(performance)
#library(see)
#library(gridExtra)
#library(qqplotr)

dist_data <- 
  read_rds(here("data","10m_resolution_1ha_dists.rds"))


surv.null <- glm(surv~1,data=dist_data, family=binomial)
summary(surv.null) # AIC 3436

surv.m1 <- glm(surv~dist_near,data=dist_data,family=binomial)
summary(surv.m1) # AIC 3430.5
car::Anova(surv.m1)

surv.m2 <- glm(surv~dist_near+dist_next, data=dist_data,family=binomial)
summary(surv.m2)
car::Anova(surv.m2) # AIC 3432.5

surv.m3 <- glm(surv~dist_near+size_prev,data=dist_data,family=binomial)
summary(surv.m3)
car::Anova(surv.m3) # in this model, dist_near no longer significant predictor


dist_data$year <- as.factor(dist_data$year)
surv.m4 <- glmer(surv~size_prev+bdffp_reserve_no+(1|year),data=noNA,family=binomial)
summary(surv.m4) # AIC 2830.9
car::Anova(surv.m4)


# Size_prev is a powerful predictor, but size itself is derived from equation: ht*shts 
# Maybe dist predictors are significant on  these terms? 

ggplot(data=dist_data,aes(x=ht))+
  geom_histogram(fill="blue",color="white")
?geom_histogram

car::qqp(dist_data$ht,"norm")

car::qqp(dist_data$ht,"lnorm")

car::qqp(dist_data$ht,"t",df=8407)

ht <- dist_data %>%
  filter(!is.na(ht))%>%
  mutate(ht_rev = ht+0.01)%>%
  mutate(log_ht = log(ht_rev))

car::qqp(ht$log_ht,"norm") # seems like the best fit, though still some issues

ht.null <- glm(log_ht~1,data=ht,family=gaussian)
summary(ht.null) # AIC 20,130

ht.m1 <- glm(log_ht~dist_near,data=ht,family=gaussian)
summary(ht.m1)
car::Anova(ht.m1)

ht.m2 <-glm(log_ht~dist_near+dist_next+bdffp_reserve_no,data=ht,family=gaussian)
summary(ht.m2) #20,110
car::Anova(ht.m2)

# seems like dist is a significant predictor for ht, although the distribution fit 
# to the data leaves much to be desired.

sht.df <- dist_data %>%
  filter(!is.na(shts))

shts.null <- glm(shts~1,data=sht.df,family=poisson)
summary(shts.null)

shts.m1 <- glm(shts~dist_near,data=sht.df,family=poisson)
summary(shts.m1)
car::Anova(shts.m1)


#check_model(shts.m1)

# Residual deviance - difference in G^2 between a saturated model and built model
# Null deviance - difference in G^2 between a saturated model and intercept-only model.


# Homer-Lemeshow statistic as an alternative for measuring goodness-of-fit.

# constructing interaction plots, maybe consider treating dist_near as a categorical
#variable for this particular data set. 

# when model is a good fit, log-likelihood is close to 0, when it is poorly fitted, should
#be a large negative number

# McFaddens Psuedo-R2 for logistic regression

# R^2 = (LL(overall probability)-LL(fit))/LL(overall probability)
# 2(LL(fit)-LL(overall probability)) = Chi-square value w/ associated p-value

# AIC is just residual deviance adjusted for number of parameters in the model

# ll.null <- model$null.deviance/-2
#ll.proposed <- model$deviance/-2

# R_sq <- (ll.null-ll.proposed)/ll.null 
# 1-pchisq(2*(ll.proposed-ll.null),df=(length(model$coefficients)-1))

