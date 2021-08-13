---
title: "Assignment 5"
author: "Josh Rogers"
date: "7/16/2021"
output: html_document
---
  
  ```{r}
#What age range correlates with the most heart failure and can we predict this 80% of the time for those younger? If not, what other variables might be important to look at / should be considered?

library(dplyr)
library(ggplot2)
library(lattice)
library(caret)
library(ROCR)
library(corrplot)

library(PerformanceAnalytics)
library(ggthemes)
library(car)
library(psych)
library(caretEnsemble)
library(doParallel)

data <- read.csv("C:/Users/joshr/Desktop/Decision Making/Heart_Failure.csv")
head(data)
```

#Survival Analysis???
```{r}
sapply(data, function(x) sum(is.na(x)))

dt = sort(sample(nrow(data), nrow(data)*.7))
train <- data[dt,]
test <- data[-dt,]
```

```{r}
train_model <- glm(DEATH_EVENT ~ ., data = train, family = "binomial")
summary(train_model)

results <- train %>% 
  mutate(pred_prob_model = predict(train_model, newdata = train, type = "response")) %>% 
  mutate(pred_outcome_model = ifelse(pred_prob_model >= 0.5, 1,0))

results$DEATH_EVENT <- as.factor(results$DEATH_EVENT)
results$pred_outcome_model <- as.factor(results$pred_outcome_model)

confusionMatrix(results$pred_outcome_model, results$DEATH_EVENT)
```

```{r}
final_results <- test %>% 
  mutate(pred_prob_model = predict(train_model, newdata = test, type = "response")) %>% 
  mutate(pred_outcome_model = ifelse(pred_prob_model >= 0.5, 1,0)) 

#Let's turn the "DEATH_EVENT" and the "pred_outcome_model" to factors 
final_results$DEATH_EVENT <- as.factor(final_results$DEATH_EVENT)
final_results$pred_outcome_model <- as.factor(final_results$pred_outcome_model)

#Let's see how our model did with the test data set
confusionMatrix(final_results$pred_outcome_model, final_results$DEATH_EVENT)
```
```{r}
pred <- prediction(final_results$pred_prob_model, final_results$DEATH_EVENT) 
class(pred)

perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=T)

auc.perf <-  performance(pred, measure = "auc")
auc.perf@y.values
```
```{r}
library(DataExplorer)
library(tidyverse)
library(gridExtra)
library(corrplot)
#install.packages("ResourceSelection")
library(ResourceSelection)
library(pscl)
library(caret)
library(pROC)

heart <- read.csv("C:/Users/joshr/Desktop/Decision Making/Heart_Failure.csv")
head(heart)

```
```{r}
heart$anaemia=factor(heart$anaemia)
heart$diabetes=factor(heart$diabetes)
heart$high_blood_pressure=factor(heart$high_blood_pressure)
heart$sex=factor(heart$sex)
heart$smoking=factor(heart$smoking)
heart$DEATH_EVENT=factor(heart$DEATH_EVENT)

#CHANGE BASED ON LAST PRESENTATION
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(df)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(cor(df), method = "color",col=col(200), type = "upper", order = "hclust",addCoef.col = "black",tl.col="black", tl.srt=45,p.mat = p.mat, sig.level = 0.05,insig = "blank",diag=FALSE,)
```


```{r}
#Need to change a to data we want to use
set.seed(1234)
a=sample(1:299,239)
train=heart[a,]
test=heart[-a,]

#MLR
lr.model=glm(DEATH_EVENT~.,family=binomial,data=train)
lr.model0=glm(DEATH_EVENT~1,family=binomial,data=train)
anova(lr.model,lr.model0, test='Chisq')
summary(lr.model)

#Stepwise model

library(randomForest)

lr.model.st=step(lr.model)
summary(lr.model.st)

feat_imp_df <- importance(fit) %>% 
  data.frame() %>% 
  mutate(feature = row.names(.)) 

# plot dataframe
ggplot(feat_imp_df, aes(x = reorder(feature, MeanDecreaseGini), 
                        y = MeanDecreaseGini)) +
  geom_bar(stat='identity') +
  coord_flip() +
  theme_classic() +
  labs(
    x     = "Feature",
    y     = "Importance",
    title = "Feature Importance: <Model>"
  )
#CAN USE TO ONLY SE COEFF: summary(mod_multiple)$coefficients
```
```{r}
#NEED TO LOOK AT SERUM, PALATELS,CREATININE, AGE, EJECTION, SODIUM? FOR OUTLIERS
plot_histogram(heart, ncol = .5L, ggtheme = theme_classic())
plot_boxplot(heart, by = "DEATH_EVENT", ncol = .5L)

hist(heart$serum_creatinine)

#NEED TRENDLINE
ggplot(df, aes(x=age, y=DEATH_EVENT)) + 
  geom_bar(stat = "identity", width=0.5) #+ stat_smooth(method=loess)

#Easy way to split data
#set.seed(3456)
#index <- createDataPartition(df$DEATH_EVENT, p = .7, list = FALSE, times = 1)
#train <- df[ index,]
#test  <- df[-index,]
```

```{r}
plot_bar(heart)
```

```{r}
#36 outliers - 28
boxplot(heart[-9], col = "orange", main = "Features Boxplot")
boxplot(heart$creatinine_phosphokinase, col = "red")
age_outliers <- which(heart$creatinine_phosphokinase > 1000)
heart[age_outliers, "creatinine_phosphokinase"]
boxplot.stats(heart$creatinine_phosphokinase)$out
#21 outliers - 3
boxplot(heart$platelets, col = "red")
boxplot.stats(heart$platelets)$out

#29 outliers - 20
boxplot(heart$serum_creatinine, col = "red")
serum_outliers <- which(heart$serum_creatinine > 2.1)
heart[serum_outliers, "serum_creatinine"]
boxplot.stats(heart$serum_creatinine)$out
#2 outliers - NON PER TEST
boxplot(heart$ejection_fraction, col = "red")
serum2_outliers <- which(heart$ejection_fraction > 65)
heart[serum2_outliers, "ejection_fraction"]
boxplot.stats(heart$ejection_fraction)$out
#4 outliers - 3
boxplot(heart$serum_sodium, col = "red")
serum3_outliers <- which(heart$serum_sodium < 125)
heart[serum2_outliers, "serum_sodium"]

out<-boxplot.stats(heart$serum_sodium)$out
boxplot(heart$serum_sodium,
        ylab = "serum_sodium",
        main = "")
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

#no multicollinearity
simple_lm <- lm(DEATH_EVENT ~ ., data = df)
vif(simple_lm)

#Rosner’s test
#it is used to detect several outliers at once (unlike Grubbs and Dixon test which must be performed iteratively to screen for multiple outliers), and
#it is designed to avoid the problem of masking, where an outlier that is close in value to another outlier can go undetected.
library(EnvStats)
#28 instead of 36
##2,61,73,104,135,172,53,228,281,39,251,165,298,164,209,150,297,247,188,108 + more
test <- rosnerTest(heart$creatinine_phosphokinase, k = 36)
test
test$all.stats
#3 out of 21
##110,297,106
test <- rosnerTest(heart$platelets, k = 21)
test
test$all.stats
#20 out of 29
##10,218,53,132,29,229,49,11,283,125,36,204,130,118,32,40,66,5,57,282
test <- rosnerTest(heart$serum_creatinine, k = 29)
test
test$all.stats
#false
test <- rosnerTest(heart$ejection_fraction, k = 2)
test
test$all.stats
#3 out of 4
##20,5,200
test <- rosnerTest(heart$serum_sodium, k = 4)
test
test$all.stats


install.packages('nortest')
library(nortest)

ad.test(heart$age)
ad.test(heart$time)
ad.test(heart$ejection_fraction)
ad.test(heart$serum_creatinine)
