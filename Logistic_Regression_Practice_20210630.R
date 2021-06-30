# Logistic Regression Practice

install.packages("ISLR")
library(ISLR)
Smarket
Smarket.df <- Smarket
plot(Smarket.df$Year,Smarket.df$Volume)

# fit logistic model to predict direction using variables lag1-5 and volume

stock.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                 data=Smarket.df,family=binomial)
summary(stock.fit)

stock_predict <- broom::augment(stock.fit,
                                se.fit = TRUE,
                                type.predict = "response")


glm.pred <- rep("Down",1250)
glm.pred[stock_predict$.fitted >.5] = "UP"

#Confusion matrix for prediction of stock market direction.
table(glm.pred,stock_predict$Direction)

(507+145)/1250 # correctly predicted 52.2%
# represents training error rate of 47.8%

train <- (Smarket.df$Year < 2005)
Smarket.2005 <- Smarket.df[!train,]
Direction.2005 <- Smarket.df$Direction[!train]

glm.stockfit2 <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5
                     +Volume,data=Smarket.df,family=binomial,subset=train)

glm.prob <- predict(glm.stockfit2,Smarket.2005,type="response")

glm.pred2 <- rep("Down",252)
glm.pred2[glm.prob > 0.5] = "Up"

table(glm.pred2,Direction.2005)
(77+44)/252 # test set error rate = 1-0.48 = 0.52
