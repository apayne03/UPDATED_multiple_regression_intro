# 1 - SET UP...

library(tidyverse)
library(apaTables)

# 2 - LOAD THE DATA...

my.data <- read_csv("regLectureData.csv")
glimpse(my.data)


# 3 - GET INITIAL DESCRIPTIVES...

apa.cor.table(my.data)

psych::pairs.panels(as.data.frame(my.data))
## You should always check for curvilinear relations when reporting correlations.
## In this case, we don't see any curvilinear relations.


# 4- PICK THE SINGLE BEST PREDICTOR...

## What is the best predictor of VidScore?
## Just look at the correlation matrix above - no need for regression (or beta-weights).
## To use beta-weights to answer this question would be an error. 
## Beta-weights only provide info about contribution to prediction in the context of a specific set of other predictors.


# 5 - MULTIPLE REGRESSION ANALYSIS: OLD SCHOOL

## We want to use age and IQ to predict video game score (Y).
## More specifically, we want to combine age and IQ to create a new variable (Y-hat) that correlates as highly as possible with video game score.

my.regression <- lm(VidScore ~ age + iq, data=my.data)
print(my.regression)

### This output shows you how we combined age, IQ and a constant to create Y-hat.
### Y-hat = 102.2333 - 0.3712(age) + 0.3285(IQ)
### What is the b-weight/slope/unstandardized regression coefficient for each predictor?
### What is the intercept?
### What do each of these mean? (*See Andy Field's book if you are not sure*)

## Now let's look a tthe expanded output:

summary(my.regression)

### The expanded output provides lots of extra info.
### Take special note, near the bottom of the output, of the R-squared value and the significance of it (indicated by the corresponding F-value and p-value).
### The R-squared value indicates how successful we were in predicting video game scores (Y) using age and IQ.
### Said another way, R-squared indicates how well Y-hat (created from age and IQ) predicts video game scores.
### R-squared ranges from 0 to 1. 

### In this case, the .29 value indicates that 29% of the variability in video game scores can be explained by age and IQ.
### More accurately, 29% of the variability in video game scores can be explained by a linear combination of age and IQ that we call Y-hat.

### In this output, the b-weight for each variable is presented under the column Estimate.
### These b-weights are also called unstandardized regression weights. 

### The signficance of each variable is inidated in the column Pr(>|t|); SPSS calls this column Sig..
### If this column indicates a predictor is significant, this means that the predictor contributes unqiue variance to Y-hat that can not be contributed by any other predictors.
### The amount of unique variance contributed by a predictor is indicated by sr-squared (semi-partial correlation squared).
### We will calculate sr-squared below.


# 6 - CREATING AN APA TABLE

apa.reg.table(my.regression, filename = "myRegressionTable.doc")


# 7 - PREDICTED SCORES: CIs

## What if we wanted the predicted score for a person who is 43yo with an IQ of 130.
## In other words, what is the estimated population mean video game score for 43yo with an IQ of 130.
## What is the CI on that estimate?

x_axis_range <- data.frame(age=c(43), iq=c(130))

CI_data <- predict(my.regression, newdata = x_axis_range, interval = "confidence", level=0.95)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))
print(CI_data)


# 8 - PREDICTED SCORES: PIs

## What if we wanted the predicted score for a person who is 43yo with an IQ of 130.
## In other words, what is the estimated population mean video game score for 43yo with an IQ of 130.
## What is the range of video game scores we can expect for 43year-olds with an IQ of 130?

x_axis_range <- data.frame(age=c(43), iq=c(130))

PI_data <- predict(my.regression, newdata = x_axis_range, interval = "prediction", level = 0.95)
PI_data <- as.data.frame(cbind(x_axis_range, PI_data))
print(PI_data)




# 9 - UNDERSTANDING IT ALL (INSIDE THE BLACK BOX)


## 9.1 - BIG R AND BIG R-SQUARED: CORRELATING PREDICTED CRITERION SCORES WITH ACTUAL CRITERION SCORES

### What do predicted values represent? They are estimated values of VidScore (i.e., Y-hat) created by combining IQ and age according to the regression equation.
### In this section, we will calculate them in a different (impractical) way that will make it clear what is represted by these statistics. 
### How well did age and IQ predict VidScore?
### One way to do this is to compare the predicted scores on the criterion (Y-hat) with actual scores on the criterion (Y).
### We do this with the correlation coefficient.

### We begin by obtaining predicted video game scores (Y-hat)
predicted.values.VidScore <- predict(my.regression)

### Then we correlate predicted scores (Y-hat) with actual video game scores (Y)
bigR <- cor(predicted.values.VidScore, my.data$VidScore)
print(bigR)
### Compare the bigR2 value here to the R-squared value in the output from print(my.regression) - they match!

### What does R-squared mean? 
### It is the proportion variability in criterion scores (Y) accounted for by (Y-hat).
### In other words, it is the proportion of variability in criterion scores that can be accounted for by (a linear combination of) IQ and age.


## 9.2 - UNDERSTANDING SEMI-PARTIAL CORRELATIONS (SQUARED)

### When an R-squared value is reported, it is useful to know what part of it is accounted for uniquely by each variable in the equation.
### In other words, what amount would R-squared drop by if that variable was removed? 
### The "drop" value indicates the unique contribution of the predictor.
### Semi-partial correlations are a way of determining the unique contribution of a variable. 
### Semi-partial correlation is the correlation of one predictor (with all the other predictors removed) with the criterion.
### Semi-partial correlation squared is the amount R-squared would drop by if that variable was removed from the regression.

### 9.2.1 - CALCULATE A SEMI-PARTIAL CORRELATION SQUARED (SR-SQAURED) "BY HAND"

#### First step is to creation a version of IQ that does not include age. 
#### We do this with a regression equation in which we make IQ the criterion (Y). Then we predict IQ with age.
#### The result is my.iq.regression which has inside of it Y-hat, which in this case is a best guess of IQ based on age.
my.iq.regression <- lm(iq ~ age, data = my.data)
print(my.iq.regression)
#### Thus, we find Y-hat = 124.763 - 0.506(age)

#### We want IQ with the effect of age removed. Therefore, we want IQ (i.e., Y) with the effect of age (i.e., Y-hat) removed.
#### Thus, we want: residual = Y - Y-hat, or another way to think of it: iq.without.age = Y - Y-hat
#### OR, iq.without.age = iq - (124.763 - 0.506(age) )
#### We do this below...

iq.without.age <- resid(my.iq.regression)

#### Then we correlate IQ without age (i.e., iq.without.age) with video game scores (i.e., VidScore).
#### This tells us how IQ correlates with video game scores when the effects of age have been removed from IQ;
#### That is, the semi-partial correlation (i.e., sr)
#### apa.reg.table does this for you - this is for learning/illustration only!!!

sr <- cor(iq.without.age, my.data$VidScore)
sr2 <- sr * sr


### 9.2.2 - COMPARE THE SEMI-PARTIAL CORRELATION SQAURED (SR-SQUARED) TO THE DIFFERENCE BETWEEN R-SQUARED BETWEEN THE 2 REGRESSIONS BELOW.

#### apa.reg.table does this for you - this is for learning/illustration only!!!

block1 <- lm(VidScore ~ age, data = my.data)
block2 <- lm(VidScore ~ age + iq, data = my.data)
summary(block1)
summary(block2)

#### Look at the R-sqaured in block1 (0.0898) and compare it to R-sqaured in block2 (0.291).
#### The increase you see (approx .20) is the sr-squared (or Delta R-squared) for IQ.


### 9.2.3 - IS THE CHANGE IN R-SQUARED (I.E., SR-SQUARED) SIGNIFICANT FOR IQ?

#### apa.reg.table does this for you - this is for learning/illustration only!!!

anova(block1, block2)

#### Think about this approach in detail. For block1, R-squared = 0.0898. For block2, 0.291. 
#### The ANOVA tells us if this increase (approx .20) was significant.

#### This approach helps us to understand a lot about using regression.
#### You conduct 2 regressions that differ by only one variable and see if the increase in variance by running the second regression is signficant.

#### Recall block1 and block 2:
block1 <- lm(VidScore ~ age, data = my.data)
block2 <- lm(VidScore ~ age + iq, data = my.data)

#### Note that the increase in R-squared occurred because we added IQ as a predictor.
#### The increase in R-squared that resulted from adding IQ (.20) is the sr-squared value for IQ.
#### You can easily see this in the apa.reg.table output.
apa.reg.table(block1, block2)



## 9.3 - UNDERSTANDING BETA-WEIGHTS


### 9.3.1 - CALCULATE THE BETA-WEIGHTS THE FAST WAY

#### Just look at the output from apa.reg.table!!! :)


### 9.3.2 - CALCULATE THE BETA-WEIGHTS OLD SCHOOL

zIQ <- scale(my.data$iq, center = TRUE, scale = TRUE)
zAge <- scale(my.data$age, center = TRUE, scale = TRUE)
zVid <- scale(my.data$VidScore, center = TRUE, scale = TRUE)

my.standardized.regression <- lm(zVid ~ zIQ + zAge)
summary(my.standardized.regression)

#### Note that the regression output is in scientific notation so you nee dto shift the decimal as indicated.
#### Compare the weights for zIQ and zAge to the beta-weights calculated above.
#### They are the same (small calculation rounding differences exist though).

#### For the b-weights (far above), describe how a 1 unit change in the IQ influences VidScore.

#### For the beta-weights (here), describe how a 1 unit change in zIQ influences VidScore. 
#### Keep in mind that 1 unit of zIQ (and zVid) is 1 standard deviation. Make sure your wording reflects this.
