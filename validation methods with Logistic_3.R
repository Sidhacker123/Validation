###
library(ISLR2)
names(Default)
dim(Default)
summary(Default)

###
attach(Default)

# Logistic Regression

###
glm.fits <- glm(
  default ~ income + balance,
  data = Default, family = binomial
)
summary(glm.fits)
###
coef(glm.fits)
###
glm.probs <- predict(glm.fits, type = "response")
contrasts(default)
###
glm.pred <- rep("No", 10000)
glm.pred[glm.probs > .5] = "Yes"
###
table(glm.pred, default)
(9629+108) / 10000
mean(glm.pred == default)

## Logistic Regression with Cross Validation

### 50-50 Split
train <- sample(10000, 5000)
test <- setdiff(1:10000, train)

set.seed(2)
glm.fits <- glm( default ~ income + balance,data = Default, subset = train, family = binomial)
summary(glm.fits)
###
coef(glm.fits)
summary(glm.fits)$coef

###
glm.probs <- predict(glm.fits, newdata= Default[train, ], type = "response")
contrasts(default)
###
glm.pred <- rep("No", 5000)
glm.pred[glm.probs > .5] = "Yes"
###
table(glm.pred, Default$default[train])
(4835+47) / 5000
mean(glm.pred == Default$default[train])

###
glm.probs <- predict(glm.fits, newdata= Default[test, ], type = "response")
contrasts(default)
###
glm.pred <- rep("No", 5000)
glm.pred[glm.probs > .5] = "Yes"
###
table(glm.pred, Default$default[test])
(4819+62) / 5000
mean(glm.pred == Default$default[test])
###

### 70-30 Split

train <- sample(10000, 7000)
test <- setdiff(1:10000, train)

set.seed(2)
glm.fits <- glm( default ~ income + balance,data = Default, subset = train, family = binomial)
summary(glm.fits)
###
coef(glm.fits)
summary(glm.fits)$coef

###
glm.probs <- predict(glm.fits, newdata= Default[train, ], type = "response")
contrasts(default)
###
glm.pred <- rep("No", 7000)
glm.pred[glm.probs > .5] = "Yes"
###
table(glm.pred, Default$default[train])
(6736+65) / 7000
mean(glm.pred == Default$default[train])

###
glm.probs <- predict(glm.fits, newdata= Default[test, ], type = "response")
contrasts(default)
###
glm.pred <- rep("No", 3000)
glm.pred[glm.probs > .5] = "Yes"
###
table(glm.pred, Default$default[test])
(2894+42) / 3000
mean(glm.pred == Default$default[test])
###

### 80-20 Split

train <- sample(10000, 8000)
test <- setdiff(1:10000, train)

set.seed(2)
glm.fits <- glm( default ~ income + balance,data = Default, subset = train, family = binomial)
summary(glm.fits)
###
coef(glm.fits)
summary(glm.fits)$coef

###
glm.probs <- predict(glm.fits, newdata= Default[train, ], type = "response")
contrasts(default)
###
glm.pred <- rep("No", 8000)
glm.pred[glm.probs > .5] = "Yes"
###
table(glm.pred, Default$default[train])
(7698+79) / 8000
mean(glm.pred == Default$default[train])

###
glm.probs <- predict(glm.fits, newdata= Default[test, ], type = "response")
contrasts(default)
###
glm.pred <- rep("No", 2000)
glm.pred[glm.probs > .5] = "Yes"
###
table(glm.pred, Default$default[test])
(1929+29) / 2000
mean(glm.pred == Default$default[test])
###

### 60-40 Split

train <- sample(10000, 6000)
test <- setdiff(1:10000, train)

set.seed(2)
glm.fits <- glm( default ~ income + balance,data = Default, subset = train, family = binomial)
summary(glm.fits)
###
coef(glm.fits)
summary(glm.fits)$coef

###
glm.probs <- predict(glm.fits, newdata= Default[train, ], type = "response")
contrasts(default)
###
glm.pred <- rep("No", 6000)
glm.pred[glm.probs > .5] = "Yes"
###
table(glm.pred, Default$default[train])
(7698+79) / 6000
mean(glm.pred == Default$default[train])

###
glm.probs <- predict(glm.fits, newdata= Default[test, ], type = "response")
contrasts(default)
###
glm.pred <- rep("No", 4000)
glm.pred[glm.probs > .5] = "Yes"
###
table(glm.pred, Default$default[test])
(1929+29) / 4000
mean(glm.pred == Default$default[test])
###