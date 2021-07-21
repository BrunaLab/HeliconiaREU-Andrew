# Detecting edge-dependent patterns in survival probability, flowering 
# probability, and size using linear distance to nearest edge as a predictor
# for edge effects. 

library(here)
library(tidyverse)
library(lme4)
library(MASS)
library(car)
library(performance)
library(piecewiseSEM)
library(broom)
# Survival Probability using 10m distances ---------------------------

dist_10m <- read_rds(here("data","10m_resolution_1ha_dists.rds"))

dist_10m_surv <- dist_10m %>%
  filter(!is.na(size_prev) & !is.na(surv)) 

str(dist_10m)
dist_10m$year <- as.factor(dist_10m$year)
dist_10m$bdffp_reserve_no <- as.factor(dist_10m$bdffp_reserve_no)
dist_10m$surv <- as.factor(dist_10m$surv)
dist_10m$flwr <- as.factor(dist_10m$flwr)
dist_10m$flwr_prev <- as.factor(dist_10m$flwr_prev)
# perform logistic regression on survival

surv.mod <- glmer(surv~log_size_prev+dist_near+bdffp_reserve_no+(1|year),
                  data=dist_10m,family=binomial)

surv.mod2 <- glm(surv~log_size_prev+dist_near+bdffp_reserve_no,data=dist_10m,family=binomial)

z <- resid(surv.mod2,type="working")+surv.mod2$linear.predictors

plot(z~surv.mod2$linear.predictors)
abline(0,1)
# This is a form of model diagnostics to be sure the link function used is appropriate.



summary(surv.mod)
car::Anova(surv.mod) # dist_near not a significant predictor
check_model(surv.mod)
piecewiseSEM::rsquared(surv.mod)

# Flowering probability using 10m distances --------------------------

flwr.mod <- glmer(flwr~log_size_prev+flwr_prev+dist_near+bdffp_reserve_no+(1|year),
                  data=dist_10m,family=binomial)
summary(flwr.mod)
car::Anova(flwr.mod) # dist_near is not a significant predictor
check_model(flwr.mod)
piecewiseSEM::rsquared(flwr.mod)

# Log_Size as a function of distance using linear regression --------------

size.mod <- lmer(log_size~log_size_prev+dist_near+bdffp_reserve_no+(1|year),
                 data=dist_10m)
summary(size.mod)
car::Anova(size.mod)
piecewiseSEM::rsquared(size.mod)
check_model(size.mod) # log-transform fixed heteroscedasity, still some questions 
# on normality of residuals.

car::Anova(size.mod) # dist_near significant predictor of log_transformed size. 

# Now preform the same set of analysis using finer scale distances -----------

dist_fine <- read_rds(here("data","full_1ha_location_data.rds"))

dist_fine$year <- as.factor(dist_fine$year)
dist_fine$bdffp_reserve_no <- as.factor(dist_fine$bdffp_reserve_no)
dist_fine$flwr <- as.factor(dist_fine$flwr)
dist_fine$flwr_prev <- as.factor(dist_fine$flwr_prev)
dist_fine$surv <- as.factor(dist_fine$surv)


# Logistic regression of survival using fine distance predictor --------------

surv.fine <- glmer(surv~log_size_prev+distance_to_nearest_edge+bdffp_reserve_no+(1|year),
                   data=dist_fine,family=binomial)
summary(surv.fine)
car::Anova(surv.fine) # distance_to_nearest_edge is a significant predictor
check_model(surv.fine)
piecewiseSEM::rsquared(surv.fine)

surv.fine2 <- glm(surv~log_size_prev+distance_to_nearest_edge+bdffp_reserve_no,
                  data=dist_fine,family=binomial)

summary(surv.fine2)
car::Anova(surv.fine2) # dist_near still significant 

# Logistic regression of flowering using fine distance predictor---------------

flwr.fine <- glmer(flwr~log_size_prev+flwr_prev+distance_to_nearest_edge+bdffp_reserve_no+
                     (1|year),data=dist_fine,family=binomial)
summary(flwr.fine)
car::Anova(flwr.fine) # distance_to_nearest_edge signficant predictor when log_size_prev used,
                      # but not signficant when size_prev used. Got convergence warnings when size_prev
                      # was used, maybe scaled variable through log-transform makes REML easier.
check_model(flwr.fine)
piecewiseSEM::rsquared(flwr.fine)
# Linear relationship between log_size and distance using fine distance predictor ---

size.fine <- lm(log_size~log_size_prev+distance_to_nearest_edge+bdffp_reserve_no+year,
                data=dist_fine)
summary(size.fine) 

size.fine2 <- lmer(log_size~log_size_prev+distance_to_nearest_edge+bdffp_reserve_no+(1|year),
                   data=dist_fine)
piecewiseSEM::rsquared(size.fine2) 
car::Anova(size.fine2)
check_model(size.fine2)

# simple regression - size and dist

size.simple <- lm(log_size~distance_to_nearest_edge,data=dist_fine)
summary(size.simple)
car::Anova(size.simple)

model.diag.size <- broom::augment(size.fine)

ggplot(data=model.diag.size,aes(x=distance_to_nearest_edge,y=log_size))+
  geom_point(aes(color=bdffp_reserve_no))+
  stat_smooth(method=lm,se=FALSE)


# Determining DOI of edge effect on survival using CF -------------------------

CF_data <- read_csv(here("data","ha_clean.csv"))

CF_data <- CF_data %>% 
  filter(habitat == "CF")

# find CF 

CF.surv <- glm(surv~1,data=CF_data,family=binomial)
CF.CI <-confint(CF.surv, level=0.84)
CF.CI.bt <- plogis(CF.CI)

CF.surv.df <- broom::augment(CF.surv,
                               se_fit=TRUE,
                               type.predict="response")

base.surv <- CF.surv.df$.fitted[1]


# For Fine scale distances --------------------------------------------
surv.simple <- glm(surv~distance_to_nearest_edge,data=dist_fine,family=binomial)
car::Anova(surv.simple)

surv.fine2.df <- broom::augment(surv.fine2,
                                se_fit=TRUE,
                                type.predict="response")

## Eric's notes ##
newdata = data.frame(log_size_prev = mean(dist_fine$log_size_prev, na.rm = TRUE),
                     distance_to_nearest_edge = seq(0, 50, by = 0.1),
                     bdffp_reserve_no = unique(dist_fine$bdffp_reserve_no))

broom::augment(surv.fine2, type.predict = "response", newdata = newdata) %>%
  ggplot(aes(x = distance_to_nearest_edge, color = bdffp_reserve_no, y = .fitted)) + 
  geom_line()
##########

 
ggplot(data=surv.fine2.df,aes(x=distance_to_nearest_edge))+
  geom_line(aes(y=.fitted))+
  geom_hline(aes(yintercept=base.surv),color="red")+
  geom_ribbon(aes(ymin=base.surv-1.405*CF.surv.df$.se.fit[1],ymax=base.surv+1.405*CF.surv.df$.se.fit[1]),fill="red",
              alpha=0.5)
  

# Results ---------------------------------------------------------------------

# Distance to nearest edge was a significant predictor of survival and flowering using finer distances

# DOI hard to interpret, have overlap almost instantly between base survival from CF and
# values for FF. Looks like there is more variance in vital rate of survival for plants
#closer to an edge than farther away. 

# Cannot interpret DOI for edge effect on any parameter, lots of variance


