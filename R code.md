# Predicting Heart Failure 

### Author: Josh Rogers 
### Date: 7/16/2021

### Question: Can we predict heart failure 80% of the time and what variables are most important?
#### Here we will be looking at a multitude of things that helped perform a better analysis on this data

- Correlations
- Multicollinearity
- Outliers
- Rosner Test
- Anderson-Darling Test
- Logistic Regression
- Stepwise Regression

```r
library(dplyr)
library(ggplot2)
library(lattice)
library(caret)
library(ROCR)
library(corrplot)
library(DataExplorer)
library(tidyverse)
library(gridExtra)
library(corrplot)
library(ResourceSelection)
library(pscl)
library(caret)
library(pROC)
library(PerformanceAnalytics)
library(ggthemes)
library(car)
library(psych)
library(caretEnsemble)
library(doParallel)
library(randomForest)

data <- read.csv("C:/Users/joshr/Desktop/Decision Making/Heart_Failure.csv")
head(data)

heart <- read.csv("C:/Users/joshr/Desktop/Decision Making/Heart_Failure.csv")
head(heart)

heart$anaemia=factor(heart$anaemia)
heart$diabetes=factor(heart$diabetes)
heart$high_blood_pressure=factor(heart$high_blood_pressure)
heart$sex=factor(heart$sex)
heart$smoking=factor(heart$smoking)
heart$DEATH_EVENT=factor(heart$DEATH_EVENT)
```

## matrix of the p-value of the correlation
```r
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value}}
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat}

p.mat <- cor.mtest(data)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(cor(data), method = "color",col=col(200), type = "upper", order = "hclust",addCoef.col = "black",tl.col="black", tl.srt=45,p.mat = p.mat, sig.level = 0.05,insig = "blank",diag=FALSE,)
```

## Helps us identify what variables have a disperse distribution and possible outliers
```r
plot_histogram(heart, ncol = .5L, ggtheme = theme_classic())
plot_bar(heart)
```
## Allows us to look at where most deaths fall based on age
```r
ggplot(df, aes(x=age, y=DEATH_EVENT)) +  geom_bar(stat = "identity", width=0.5) #+ stat_smooth(method=loess)
```
# FIX BELOW
## Using what we found, we need to look at Creatinine Phosphokinase, Platelets, Serum Creatinine, Age, Ejection Fraction, and Serum Sodium
```r
boxplot(heart[-9], col = "orange", main = "Features Boxplot")
boxplot(heart$creatinine_phosphokinase, col = "red")
age_outliers <- which(heart$creatinine_phosphokinase > 1000)
heart[age_outliers, "creatinine_phosphokinase"]
boxplot.stats(heart$creatinine_phosphokinase)$out

boxplot(heart$platelets, col = "red")
boxplot.stats(heart$platelets)$out

boxplot(heart$serum_creatinine, col = "red")
serum_outliers <- which(heart$serum_creatinine > 2.1)
heart[serum_outliers, "serum_creatinine"]
boxplot.stats(heart$serum_creatinine)$out

boxplot(heart$ejection_fraction, col = "red")
serum2_outliers <- which(heart$ejection_fraction > 65)
heart[serum2_outliers, "ejection_fraction"]
boxplot.stats(heart$ejection_fraction)$out

boxplot(heart$serum_sodium, col = "red")
serum3_outliers <- which(heart$serum_sodium < 125)
heart[serum2_outliers, "serum_sodium"]

out<-boxplot.stats(heart$serum_sodium)$out
boxplot(heart$serum_sodium,
        ylab = "serum_sodium",
        main = "")
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
```
## no multicollinearity
```r
simple_lm <- lm(DEATH_EVENT ~ ., data = df)
vif(simple_lm)
```
## Rosner’s test
## it is used to detect several outliers at once (unlike Grubbs and Dixon test which must be performed repetitively to screen for multiple outliers), and is designed to avoid the problem of masking, where an outlier that is close in value to another outlier can go undetected.
```r
library(EnvStats)
test <- rosnerTest(heart$creatinine_phosphokinase, k = 36)
test
test$all.stats

test <- rosnerTest(heart$platelets, k = 21)
test
test$all.stats

test <- rosnerTest(heart$serum_creatinine, k = 29)
test
test$all.stats

test <- rosnerTest(heart$ejection_fraction, k = 2)
test
test$all.stats

test <- rosnerTest(heart$serum_sodium, k = 4)
test
test$all.stats

library(nortest)
ad.test(heart$age)
ad.test(heart$time)
ad.test(heart$ejection_fraction)
ad.test(heart$serum_creatinine)
```
## Logit Regression
```r
set.seed(1234)
a=sample(1:299,239)
train=heart[a,]
test=heart[-a,]

lr.model=glm(DEATH_EVENT~.,family=binomial,data=train)
summary(lr.model)
```
## Stepwise model
```r
lr.model.st=step(lr.model)
summary(lr.model.st)
```
