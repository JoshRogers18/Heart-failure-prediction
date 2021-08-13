# Predicting Heart Failure 

## Author: Josh Rogers

## Date: 7/16/2021

### Question: What age range correlates with the most heart failure and can we predict this 80% of the time for those younger? If not, what other variables might be important to look at / should be considered?
##### Here we will be looking at correlations and if there is multicollinearity, outliers and if they pass the Rosner test, and if variables are normally distributed looking at the Anderson-Darling test.


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

```r
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


```r
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
```r
#NEED TO LOOK AT SERUM, PALATELS,CREATININE, AGE, EJECTION, SODIUM? FOR OUTLIERS
plot_histogram(heart, ncol = .5L, ggtheme = theme_classic())
plot_boxplot(heart, by = "DEATH_EVENT", ncol = .5L)

hist(heart$serum_creatinine)

ggplot(df, aes(x=age, y=DEATH_EVENT)) + 
  geom_bar(stat = "identity", width=0.5) #+ stat_smooth(method=loess)
```

```r
plot_bar(heart)
```

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

#no multicollinearity
simple_lm <- lm(DEATH_EVENT ~ ., data = df)
vif(simple_lm)

#Rosner’s test
#it is used to detect several outliers at once (unlike Grubbs and Dixon test which must be performed iteratively to screen for multiple outliers), and
#it is designed to avoid the problem of masking, where an outlier that is close in value to another outlier can go undetected.

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
