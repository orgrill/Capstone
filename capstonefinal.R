###16 variables, bp and chol code###
heart <-read.csv(file="~/Homework/capstone/heart.csv")
df <- data.frame(x = c(heart))

#Transforming categorical data from df to factors
names <- c(2,3,5,7:9,11,13:16)
df[,names] <- lapply(df[,names], factor)

########## UNIVARIATE EXPLORATORY ANALYSIS #################
library(tidyverse)
library(ggplot2)

#6 number summary of variables#
summary(df)

### Categorical Variables ###
ggplot( data = df) +
  geom_bar(mapping = aes((x = x.sex)))
ggplot( data = df) +
  geom_bar(mapping = aes((x = x.cp)))
ggplot( data = df) +
  geom_bar(mapping = aes((x = x.rbpcode)))
ggplot( data = df) +
  geom_bar(mapping = aes((x = x.normchol)))
ggplot( data = df) +
  geom_bar(mapping = aes((x = x.fbs)))
ggplot( data = df) +
  geom_bar(mapping = aes((x = x.restecg)))
ggplot( data = df) +
  geom_bar(mapping = aes((x = x.exang)))
ggplot( data = df) +
  geom_bar(mapping = aes((x = x.slope)))
ggplot( data = df) +
  geom_bar(mapping = aes((x = x.mv)))
ggplot( data = df) +
  geom_bar(mapping = aes((x = x.thal)))
ggplot( data = df) +
  geom_bar(mapping = aes((x = x.cad)))

### Continuous Variables ###
ggplot(data = df) + 
  geom_histogram(mapping = aes(x = x.age), stat = "bin", 
                 binwidth = 1)
ggplot(data = df) + 
  geom_histogram(mapping = aes(x = x.rbp),stat = "bin", 
                 binwidth = 4)
ggplot(data = df) + 
  geom_histogram(mapping = aes(x = x.chol),stat = "bin", 
                 binwidth = 5)
ggplot(data = df) + 
  geom_histogram(mapping = aes(x = x.mhr),stat = "bin", 
                 binwidth = 3)
ggplot(data = df) + 
  geom_histogram(mapping = aes(x = x.oldpeak),stat = "bin", 
                 binwidth = 0.1)


### Aggregate Exploration ###
rbpsex <- t(aggregate(df$x.rbp~df$x.sex, FUN = summary))
rbpsex
cholsex <- t(aggregate(df$x.chol~df$x.sex, FUN = summary))
cholsex
mhrsex <- t(aggregate(df$x.mhr~df$x.sex, FUN = summary))
mhrsex
oldpeaksex <- t(aggregate(df$x.oldpeak~df$x.sex, FUN = summary))
oldpeaksex
rbpcodesex <- t(aggregate(df$x.rbpcode~df$x.sex, FUN = summary))
rbpcodesex
cadsex <- t(aggregate(df$x.cad~df$x.sex, FUN = summary))
cadsex
normcholsex <- t(aggregate(df$x.normchol~df$x.sex, FUN = summary))
normcholsex
agesex <- t(aggregate(df$x.age~df$x.sex, FUN = summary))
agesex
rbpcad <- t(aggregate(df$x.rbp~df$x.cad, FUN = summary))
rbpcad
cholcad <- t(aggregate(df$x.chol~df$x.cad, FUN = summary))
cholcad
agecad <- t(aggregate(df$x.age~df$x.cad, FUN = summary))
agecad
cpcad <- t(aggregate(df$x.cp~df$x.cad, FUN = summary))
cpcad

### T Tests -> t.test(continuous~categorical) ###
t.test(df$x.rbp~df$x.sex)
t.test(df$x.chol~df$x.sex)
t.test(df$x.mhr~df$x.sex)
t.test(df$x.age~df$x.cad)
t.test(df$x.rbp~df$x.cad)
t.test(df$x.chol~df$x.cad)
t.test(df$x.mhr~df$x.cad)
t.test(df$x.oldpeak~df$x.cad)

###Correlation###
cormat <- round(x = cor(df4), digits = 2) method = c("kendall")
cormat
corrplot::corrplot((cormat), method = "number", type = "upper")

#Boxplots of independent variable on X and continuous Iv's on Y
library(ggplot2)
ggplot(data = df, aes(x = "", y = x.age)) + 
  geom_boxplot()
ggplot(df, aes(x = "", y = x.rbp)) + 
  geom_boxplot()
ggplot(df, aes(x = "", y = x.chol)) + 
  geom_boxplot()
ggplot(df, aes(x = "", y = x.mhr)) + 
  geom_boxplot()
ggplot(df, aes(x = "", y = x.oldpeak)) + 
  geom_boxplot()

#Obtaining the index values of outliers for each IV
out <- boxplot.stats(df$x.age)$out
out_ind <- which(df$x.age %in% c(out))
out_ind
out <- boxplot.stats(df$x.rbp)$out
out_ind <- which(df$x.rbp %in% c(out))
out_ind
out <- boxplot.stats(df$x.chol)$out
out_ind <- which(df$x.chol %in% c(out))
out_ind
out <- boxplot.stats(df$x.mhr)$out
out_ind <- which(df$x.mhr %in% c(out))
out_ind
out <- boxplot.stats(df$x.oldpeak)$out
out_ind <- which(df$x.oldpeak %in% c(out))
out_ind

### Creating dataset with outlier rows removed ###
outliersdf <- df[-c(10,46,77,118,168,179,210,235,243,
                    80,84,181,189,239,
                    165,
                    187,214,240,261),]

#Checking Linearity 
model <- glm(x.cad~., data = df4, family=binomial)
probabilities <- predict(model, type="response")

mydata <- df4 %>%
  dplyr::select_if(is.numeric())
predictors <- colnames(df4)

mydata <- df4 %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value)) +
  geom_point(size = 0.5, alpha = 0.5)+
  geom_smooth(formula = y~x, method = "loess")+
  theme_bw()+
  facet_wrap(~predictors, scales = "free_y")

### Multicollinearity among continuous variables ###
car::vif(model)

###################### APPROACH 1 #################################
### Only numeric variables ###
fullmodel <- glm(x.cad~x.age+x.rbp+x.chol+x.mhr+x.oldpeak, df, family = binomial )
summary(fullmodel)
#STEP 1
glm.age <- glm(x.cad~x.age, df, family = binomial)
summary(glm.age)
glm.rbp <- glm(x.cad~x.rbp, df, family = binomial)
summary(glm.rbp)
glm.chol <- glm(x.cad~x.chol, df, family = binomial)
summary(glm.chol)
glm.mhr <- glm(x.cad~x.mhr, df, family = binomial)
summary(glm.mhr)
glm.oldpeak <- glm(x.cad~x.oldpeak, df, family = binomial)
summary(glm.oldpeak)

###Omit Chol, mhr is best variable for Step 1###
#STEP 2
model1 <- glm(x.cad~x.mhr + x.age, df, family = binomial)
summary(model1)
model2 <- glm(x.cad~x.mhr + x.rbp, df, family = binomial)
summary(model2)
model3 <- glm(x.cad~x.mhr + x.oldpeak, df, family = binomial)
summary(model3)

##Omit age, oldpeak is best variable from Step 2##
#STEP 3
finalmodel <- glm(x.cad~x.mhr+x.oldpeak+x.rbp, df, family = binomial)
summary(finalmodel)

############### APPROACH 1 OUTLIERS REMOVED ###################
fullmodel2 <- glm(x.cad~x.age+x.rbp+x.chol+x.mhr+x.oldpeak,
                  outliersdf, family = binomial )
summary(fullmodel2)

glm.age2 <- glm(x.cad~x.age, outliersdf, family = binomial)
summary(glm.age2)
glm.rbp2 <- glm(x.cad~x.rbp, outliersdf, family = binomial)
summary(glm.rbp2)
glm.chol2 <- glm(x.cad~x.chol, outliersdf, family = binomial)
summary(glm.chol2)
glm.mhr2 <- glm(x.cad~x.mhr, outliersdf, family = binomial)
summary(glm.mhr2)
glm.oldpeak2 <- glm(x.cad~x.oldpeak, outliersdf, family = binomial)
summary(glm.oldpeak2)

model6 <- glm(x.cad~x.mhr + x.age, df, family = binomial)
summary(model6)
model7 <- glm(x.cad~x.mhr + x.rbp, df, family = binomial)
summary(model7)
model8 <- glm(x.cad~x.mhr + x.oldpeak, df, family = binomial)
summary(model8)

#STEP 3#
step3model <- glm(x.cad~x.mhr+x.oldpeak+x.rbp, outliersdf, family = binomial)
summary(step3model)
# rbp p-value=0.243, not below 0.15, omit from model
finalmodel2 <- glm(x.cad~x.mhr+x.oldpeak, outliersdf, family = binomial)
summary(finalmodel2)

### Interactions ### 
interaction1 <- glm(x.cad~x.mhr+x.oldpeak+x.rbp*x.age, outliersdf, family = binomial)
summary(interaction1)
interaction2 <- glm(x.cad~x.mhr+x.oldpeak+x.mhr*x.age, outliersdf, family = binomial)
summary(interaction2)
interaction3 <- glm(x.cad~x.mhr+x.oldpeak+x.rbp*x.mhr, outliersdf, family = binomial)
summary(interaction3)
interaction4 <- glm(x.cad~x.mhr+x.oldpeak+x.rbp*x.oldpeak, outliersdf, family = binomial)
summary(interaction4)
interaction5 <- glm(x.cad~x.mhr+x.oldpeak+x.chol*x.mhr, outliersdf, family = binomial)
summary(interaction5)
interaction6 <- glm(x.cad~x.mhr+x.oldpeak+x.chol*x.oldpeak, outliersdf, family = binomial)
summary(interaction6)
interaction7 <- glm(x.cad~x.mhr+x.oldpeak+x.chol*x.age, outliersdf, family = binomial)
summary(interaction7)
interaction8 <- glm(x.cad~x.mhr+x.oldpeak+x.chol*x.rbp, outliersdf, family = binomial)
summary(interaction8)
interaction9 <- glm(x.cad~x.mhr+x.oldpeak+x.oldpeak*x.mhr, outliersdf, family = binomial)
summary(interaction9)

#################################################################################
######################### APPROACH 2 ################################
#### WITH OUTLIERS ####
model4 <- glm(I(x.cad == 1)~x.age+x.sex+x.cp+x.rbp+x.rbpcode+x.chol+x.normchol+
                x.fbs+x.restecg+x.mhr+x.exang+x.oldpeak+x.slope+x.mv+x.thal,
              family = binomial,
              data = df)
summary(model4)
stepmodel<- step(model4, test="LRT")
stepmodel

#### OUTLIERS REMOVED ###
model5 <- glm(I(x.cad == 1)~x.age+x.sex+x.cp+x.rbp+x.rbpcode+x.chol+x.normchol+
                x.fbs+x.restecg+x.mhr+x.exang+x.oldpeak+x.slope+x.mv+x.thal,
              family = binomial,
              data = outliersdf)
summary(model5)
stepmodel2<- step(model5, test="LRT")
stepmodel2


############## Model Checking ###########
### ANOVA ###
#Approach 1#
#   WITH OUTLIERS   #
anova(finalmodel, test = "Chisq")
#   WITHOUT OUTLIERS  #
anova(finalmodel2, test = "Chisq")

#Approach 2#
#    WITH OUTLIERS   #
anova(stepmodel, test = "Chisq")
#   WITHOUT OUTLIERS   #
anova(stepmodel2, test = "Chisq")

### Wald Test ###
#Approach 1#
library(lmtest)
waldtest(model3, finalmodel)
waldtest(model3, glm.mhr)
waldtest(finalmodel,fullmodel)
waldtest(model3,fullmodel)
waldtest(interaction2, finalmodel2)

### Wald Test ### 
#Approach 2#
library(survey)
regTermTest(finalmodel, "x.mhr")
regTermTest(finalmodel, "x.oldpeak")
regTermTest(finalmodel, "x.rbp")

regTermTest(stepmodel2, "x.sex")
regTermTest(stepmodel2, "x.cp")
regTermTest(stepmodel2, "x.rbp")
regTermTest(stepmodel2, "x.chol")
regTermTest(stepmodel2, "x.normchol")    # not significant
regTermTest(stepmodel2, "x.mv")
regTermTest(stepmodel2, "x.slope")       # not significant 
regTermTest(stepmodel2, "x.oldpeak")
regTermTest(stepmodel2, "x.mhr")         # not significant
regTermTest(stepmodel2, "x.thal")

###################################################################
#################### FINAL MODELS FOR APP. 1&2 ########################
#1#
interaction2 <- glm(x.cad~x.mhr+x.oldpeak+x.mhr*x.age, outliersdf, family = binomial)
summary(interaction2)

#2#
A2finalmodel<- glm(x.cad~x.sex+x.cp+x.rbp+x.chol+x.oldpeak+x.mv+x.thal, 
                   data=outliersdf, family=binomial)
summary(A2finalmodel)

################ PLOTS and CONFUSION MATRICES-FINAL MODELS ################
##########################################################################
################## WITH OUTLIERS ##########################
library(caret)
Train <- createDataPartition(df$x.cad, p=0.6, list = FALSE)
training <- df[Train, ]
testing <- df[-Train, ]

###Approach 1###
mod_fit1<- train(x.cad~x.mhr+x.oldpeak+x.mhr*x.age, 
                 data = training, method="glm", family="binomial")
exp(coef(mod_fit1$finalModel))

pred = predict(mod_fit1, newdata = testing)
accuracy <- table(pred, testing[,"x.cad"])
sum(diag(accuracy))/sum(accuracy)
confusionMatrix(data = pred, testing$x.cad)

###Approach 2###
mod_fit3<- train(x.cad~x.sex+x.cp+x.rbp+x.chol+x.oldpeak+x.mv+x.thal, 
                 data = training, method="glm", family="binomial", 
                 na.action = na.exclude)
exp(coef(mod_fit3$finalModel))

pred = predict(mod_fit3, newdata = testing)
accuracy <- table(pred, testing[,"x.cad"])
sum(diag(accuracy))/sum(accuracy)
confusionMatrix(data = pred, testing$x.cad)

##################### WITHOUT OUTLIERS ######################
library(caret)
Train1 <- createDataPartition(outliersdf$x.cad, p=0.8, list = FALSE)
training1 <- df[Train1, ]
testing1 <- df[-Train1, ]

#Approach 1#
mod_fit3<- train(x.cad~x.mhr+x.oldpeak+x.mhr*x.age, 
                 data = training1, 
                 method="glm", family="binomial")
exp(coef(mod_fit3$finalModel))

pred = predict(mod_fit3, newdata = testing1)
accuracy <- table(pred, testing1[,"x.cad"])
sum(diag(accuracy))/sum(accuracy)
confusionMatrix(data = pred, testing1$x.cad)

#Approach 2#
mod_fit4<- train(x.cad~x.sex+x.cp+x.rbp+x.chol+x.oldpeak+x.mv+x.thal, 
                 data = training1,
                 method="glm", family="binomial")
exp(coef(mod_fit4$finalModel))

pred = predict(mod_fit4, newdata = testing1)
accuracy <- table(pred, testing1[,"x.cad"])
sum(diag(accuracy))/sum(accuracy)
confusionMatrix(data = pred, testing1$x.cad)

### Approach 1 without outliers ###
predicted.data <- data.frame(
  probability.of.cad = interaction2$fitted.values, 
  cad=outliersdf$x.cad)
predicted.data <- predicted.data[
  order(predicted.data$probability.of.cad, decreasing = FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)
library(ggplot2)
ggplot(data=predicted.data, aes(x=rank, y=probability.of.cad)) +
  geom_point(aes(color=cad), alpha=1, shape=4, stroke=2)+
  xlab("Index")+
  ylab("Predicted Probability of Coronary Artery Disease")

### Plot for Approach 2 without outliers### 
predicted.data <- data.frame(
  probability.of.cad = A2finalmodel$fitted.values, 
  cad=outliersdf$x.cad)
predicted.data <- predicted.data[
  order(predicted.data$probability.of.cad, decreasing = FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)
library(ggplot2)
ggplot(data=predicted.data, aes(x=rank, y=probability.of.cad)) +
  geom_point(aes(color=cad), alpha=1, shape=4, stroke=2)+
  xlab("Index")+
  ylab("Predicted Probability of CAD")
