# Predicting Heart Failure 

### Author: Josh Rogers 
### Date: 7/16/2021

### Link to data: https://www.kaggle.com/andrewmvd/heart-failure-clinical-data
### Question: Can we predict heart failure 80% of the time and what variables are most important?
### Here we will be looking at a multitude of things that helped perform a better analysis on this data in Orange when data mining. Below you'll find the code I used for making decisions with the data.
---
## Table of contents
| Topic                      | Description |
| :-                         |    :-       |
| Correlations               | Look at what variables may be directly related and may possibly be boosting output metric based on their significance |
| Multicollinearity          | Check the Variance Inflation Factor scores (VIFs) to see if there is boosting                                         |
| Outliers                   | Willlook at boxplots to see what variables may have outliers and then look at each of those individually              |
| Rosner Test                | Taking outliers a step further to see if they are true outliers                                                       |
| Anderson-Darling Test      | Used to check variable distribution (if normal or not because this can influence outlier removal)                     |
| Logistic Regression        | Used to see how well binary model fits                                                                                |
| Stepwise Regression        | Used to pull out most important variables                                                                             |
---
### RStudio packages and data implementation
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

### Here is a function that helps show what correlations are significant and contributes to creating a well organized correlation matrix
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
### Allows us to look at where most deaths fall based on age
```r
ggplot(df, aes(x=age, y=DEATH_EVENT)) +  geom_bar(stat = "identity", width=0.5) #+ stat_smooth(method=loess)
```
### Helps us identify what variables have a disperse distribution and possible outliers
```r
plot_histogram(heart, ncol = .5L, ggtheme = theme_classic())
plot_bar(heart)
```
### Using what was found, we need to look at Creatinine Phosphokinase, Platelets, Serum Creatinine, Age, Ejection Fraction, and Serum Sodium for outliers. 
#### We find that there a lot of outliers in most of these and that age does not have any and that platelets has the most. A further test will be conducted in order to see if these are all true outliers.
```r
boxplot(heart, col = "orange", main = "Features Boxplot")

out<-boxplot.stats(heart$creatinine_phosphokinase)$out
boxplot(heart$creatinine_phosphokinase,
        ylab = "creatinine_phosphokinase",
        main = "")
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
cp_outliers <- which(heart$creatinine_phosphokinase > 1000)
heart[cp_outliers, "creatinine_phosphokinase"]

boxplot.stats(heart$platelets)$out

out<-boxplot.stats(heart$serum_creatinine)$out
boxplot(heart$serum_creatinine,
        ylab = "serum_creatinine",
        main = "")
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
serumcreatinine_outliers <- which(heart$serum_creatinine > 2.1)
heart[serumcreatinine_outliers, "serum_creatinine"]

boxplot(heart$age, col = "red")

out<-boxplot.stats(heart$ejection_fraction)$out
boxplot(heart$ejection_fraction,
        ylab = "ejection_fraction",
        main = "")
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
ef_outliers <- which(heart$ejection_fraction > 65)
heart[ef_outliers, "ejection_fraction"]

out<-boxplot.stats(heart$serum_sodium)$out
boxplot(heart$serum_sodium,
        ylab = "serum_sodium",
        main = "")
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
serumsodium_outliers <- which(heart$serum_sodium < 125)
heart[serumsodium_outliers, "serum_sodium"]
```
### Multicollinearity check (no issues found)
```r
simple_lm <- lm(DEATH_EVENT ~ ., data = df)
vif(simple_lm)
```
### Rosner’s test: Used to detect several outliers at once (unlike Grubbs and Dixon test which must be performed repetitively to screen for multiple outliers) and is designed to avoid the problem of masking - where an outlier that is close in value to another outlier can go undetected. Using the amount of outliers shown through our boxplot's "$out" function, this was used to tell the test how many outliers we assumed there might be. 
#### Most of these variables had less outliers than assumed with the boxplot, however, ending up with around 40 or so outliers is unfavorable to try and get rid of since our dataset is 300 rows. The idea here is to remove those data points outside the three standard deviations (97%) so 3% was effectively removed via Orange and will be described as to how next.
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
```
### Here we can check if the distribution of these variables is gaussian or not... we find that all have significant p-values meaning that each is not normally distributed. 
#### This is important because while using Orange to data mine and remove outliers, I had to use a specific method purely for removing oultiers in a non-normal distribution (this is addressed deeper in the Power BI presentation)
```r
library(nortest)
ad.test(heart$age)
ad.test(heart$time)
ad.test(heart$ejection_fraction)
ad.test(heart$serum_creatinine)
```
### Logit Regression to see how well a general binary model performs in predicting the death event
```r
set.seed(1234)
a=sample(1:299,239)
train=heart[a,]
test=heart[-a,]

lr.model=glm(DEATH_EVENT~.,family=binomial,data=train)
summary(lr.model)
```
### Stepwise model to show what variables are most important and see what influence they have. 
#### This is compared to the mulitple models tried in Orange and relates to the ending analyses of this project
```r
lr.model.st=step(lr.model)
summary(lr.model.st)
```
