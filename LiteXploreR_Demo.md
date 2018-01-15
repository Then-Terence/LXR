# LiteXploreR Demo
Terence Then  
Version 1.0 December 31, 2017  

# Introduction of {LiteXploreR}

This package compiles some functions for quick, dirty, and repetitive
exploratory analysis which may otherwise lengthen the script unnecessarily.
Emphasis is placed on the purpose of credit scoring, where WoE modelling
(Semi Naive Bayesian Classification) is routinely used. Most of functions
caters to the prediction of binary target variable.

In this version of the demonstration document, I have only included the
functions that will be used for modelling the response of a binary target.

There are some functions written for analyses involving a target taking a
continuous value, but they have a long way to go, and will be expanded over
time. Hopefully by then, they will warrant a separate documentation for
the purpose of demonstration.

# How to Download this Package

As {LiteXploreR} is hosted on GitHub, you will need to use the function
install_github() for the package {devtools} to install it.

To do that, you need to run the following lines of code in your R console:

install.packages("devtools")

library(devtools)

install_github("Then-Terence/LiteXploreR")

# Setting Up the Workspace



First of all, I am going to load the package {LiteXploreR} and a credit
dataset obtained from Kaggle (https://www.kaggle.com/c/GiveMeSomeCredit/data).
I have split them into training, validation and testing set.


```r
# Load the Package
library(LiteXploreR)
```

```
## Loading required package: data.table
```

```
## Warning: package 'data.table' was built under R version 3.4.3
```

```r
# Load the Data
load("../Output/PartitionedData.RData")
```

This is how the workspace looks like:


```r
ls()
```

```
## [1] "Test"  "Train"
```

Let us start by looking at the training set.


```r
head(Train)
```

```
##    Delinquency Utilization Age Due30    DebtRatio MonthlyIncome
## 1:           1 0.301491625  65     1    1.3824301          4575
## 2:           0 1.059940060  48     7 3620.0000000            NA
## 3:           0 0.155707651  39     0    0.5136848          3470
## 4:           0 0.172388507  42     0    0.4079970          2700
## 5:           0 0.620568038  39     0    0.3404613         10100
## 6:           0 0.009339405  95     0   24.0000000            NA
##    CreditAndLoans Due90 RealEstateLines Due60 Dependents
## 1:             20     0               2     1          0
## 2:             18     0               3     0          0
## 3:             15     0               1     0          2
## 4:              8     0               1     0          1
## 5:             11     0               1     0          4
## 6:              3     0               0     0          0
```

# Demonstration of Functions

## Overview of a Dataset: CovariateSummary()

In cases where the number of columns/ covariates is overwhelming, they can be
screened out using the function CovariateSummary, which gives the AUC value
of the raw covariates.


```r
Overview <- CovariateSummary(Target = "Delinquency", Data = Train, Type = "Binary")
Overview
```

```
##           Covariate    Type Count of NA Count of 0    AUC
##  1:     Utilization numeric           0       8696 0.7763
##  2:             Age integer           0          1 0.6355
##  3:           Due30 integer           0     100870 0.6899
##  4:       DebtRatio numeric           0       3270 0.5229
##  5:   MonthlyIncome integer       23692       1299 0.5752
##  6:  CreditAndLoans integer           0       1527 0.5436
##  7:           Due90 integer           0     113313 0.6563
##  8: RealEstateLines integer           0      45053 0.5357
##  9:           Due60 integer           0     113969 0.6214
## 10:      Dependents integer        3119      69429 0.5469
```

The column "Type" serves as a check for the classes of the columns, i.e.
whether they are imported correctly.

For the purpose of this demonstration, I will only use the covariates
"Age", "Due30", "Due60", "Due90" in the following sections.

## Exploring and Transforming a Numerical Covariate: NumericalTable(), CategorialTable() and GenerateLogit()

### Covariate "Age""

First thing first, there is one observation with the age of 0. I will replace
it with the second lowest value of age.


```r
Train[Age == 0, Age := 21]
```

Here, a table is constructed by dividing "Age" into 20 categories of roughly
equal sizes.


```r
AgeTable <- NumericalTable(Target = "Delinquency", Covariate = "Age",
                           Data = Train, NumberOfBins = 20)
```

The table looks something like this:

```r
AgeTable
```

```
##          Age Event Non Event Counts Probability     Logit
##  1:  [21,29]   820      6206   7026      0.1167 -2.023968
##  2:  (29,33]   727      5900   6627      0.1097 -2.093781
##  3:  (33,36]   545      4857   5402      0.1009 -2.187390
##  4:  (36,39]   589      5853   6442      0.0914 -2.296283
##  5:  (39,41]   448      4559   5007      0.0895 -2.320065
##  6:  (41,44]   639      7039   7678      0.0832 -2.399317
##  7:  (44,46]   501      5289   5790      0.0865 -2.356778
##  8:  (46,48]   476      5562   6038      0.0788 -2.458295
##  9:  (48,50]   457      5587   6044      0.0756 -2.503514
## 10:  (50,52]   439      5403   5842      0.0751 -2.510210
## 11:  (52,54]   417      5335   5752      0.0725 -2.548958
## 12:  (54,56]   342      5257   5599      0.0611 -2.732505
## 13:  (56,58]   285      5175   5460      0.0522 -2.899105
## 14:  (58,61]   399      7638   8037      0.0496 -2.951930
## 15:  (61,63]   229      5576   5805      0.0394 -3.192505
## 16:  (63,65]   148      4388   4536      0.0326 -3.389417
## 17:  (65,68]   161      5627   5788      0.0278 -3.553927
## 18:  (68,72]   140      5457   5597      0.0250 -3.663012
## 19:  (72,78]   132      5931   6063      0.0218 -3.805146
## 20: (78,109]   112      5355   5467      0.0205 -3.867287
```

For every category, the table shows the number of observations, the number of
observations that take the value of 1 for Delinquency, those who take the value
of 0 for delinquency, the probability, and the logit score.

The logit score is the log odds, which is the numerical equivalent used in
logistic regression.

The cutoffs can be obtained from the function quantile() in base R. Its minimum
and maximum are replaced by 0 and Inf respectively.


```r
AgeBreaks <- quantile(Train[, Age], probs = seq(0, 1, 0.05))
AgeBreaks <- c(0, AgeBreaks[-c(1, 21)], Inf)
```

Then, the table is updated with the thresholds, the covariate is discretized,
and the logit scores are extracted from the table.


```r
AgeTable <- NumericalTable("Delinquency", "Age", Train,
                           CustomBins = T,
                           CustomIntervals = AgeBreaks)
Train[, AgeCat := cut(Age, breaks = AgeBreaks, include.lowest = T)]
Train <- GenerateLogit(Data = Train, CrossTable = AgeTable, Covariate = "AgeCat")
```

The function generates another column with the logit scores and will be named
by appending the original column name followed by "Logit".

In this case, a new column named "AgeCatLogit" is generated.


```r
head(unique(Train[, c("AgeCat", "AgeCatLogit")]))
```

```
##     AgeCat AgeCatLogit
## 1:  [0,29]   -2.023968
## 2: (29,33]   -2.093781
## 3: (33,36]   -2.187390
## 4: (36,39]   -2.296283
## 5: (39,41]   -2.320065
## 6: (41,44]   -2.399317
```

### Covariate "Due30"

While the covariate "Due30" is numerical, it may be helpful for treating it as
categorical in the first inspection. This is due to it having a limited number
of unique values, and not being able to split into equal categories as most of
the borrowers will have the value of 0.


```r
Due30Table <- CategoricalTable(Target = "Delinquency", Covariate = "Due30",
                               Data = Train)
Due30Table
```

```
##     Due30 Event Non Event Counts Probability      Logit
##  1:     0  4025     96845 100870      0.0399 -3.1805869
##  2:     1  1914     10895  12809      0.1494 -1.7391087
##  3:     2   988      2689   3677      0.2687 -1.0012420
##  4:     3   497       901   1398      0.3555 -0.5949152
##  5:     4   252       335    587      0.4293 -0.2847014
##  6:     5   124       143    267      0.4644 -0.1425631
##  7:     6    58        53    111      0.5225  0.0901511
##  8:     7    20        20     40      0.5000  0.0000000
##  9:     8     5        13     18      0.2778 -0.9555114
## 10:     9     3         6      9      0.3333 -0.6931472
## 11:    10     3         1      4      0.7500  1.0986123
## 12:    12     1         1      2      0.5000  0.0000000
## 13:    13     1         0      1      1.0000        Inf
## 14:    96     3         1      4      0.7500  1.0986123
## 15:    98   112        91    203      0.5517  0.2076394
```

There are very little observations having values greater than 4. As such, they
will be grouped together.

The exceptions are 96 and 98, which may indicate a special status, they will be
grouped separately.

Then, the table is updated.


```r
Due30Breaks <- c(-Inf, 0:4, 95, Inf)
Due30Table <- NumericalTable(Target = "Delinquency", Covariate = "Due30",
                             Data = Train, CustomBins = T,
                             CustomIntervals = Due30Breaks)
Due30Table
```

```
##        Due30 Event Non Event Counts Probability       Logit
## 1:  [-Inf,0]  4025     96845 100870      0.0399 -3.18058685
## 2:     (0,1]  1914     10895  12809      0.1494 -1.73910868
## 3:     (1,2]   988      2689   3677      0.2687 -1.00124196
## 4:     (2,3]   497       901   1398      0.3555 -0.59491523
## 5:     (3,4]   252       335    587      0.4293 -0.28470144
## 6:    (4,95]   215       237    452      0.4757 -0.09742211
## 7: (95, Inf]   115        92    207      0.5556  0.22314355
```

The covariate "Due30" is discretized and the logit scores are extracted.

```r
Train[, Due30Cat := cut(Due30, breaks = Due30Breaks, include.lowest = T)]
Train <- GenerateLogit(Data = Train, CrossTable = Due30Table,
                       Covariate = "Due30Cat")
```


```r
head(unique(Train[, c("Due30Cat", "Due30CatLogit")]))
```

```
##    Due30Cat Due30CatLogit
## 1: [-Inf,0]   -3.18058685
## 2:    (0,1]   -1.73910868
## 3:    (1,2]   -1.00124196
## 4:    (2,3]   -0.59491523
## 5:    (3,4]   -0.28470144
## 6:   (4,95]   -0.09742211
```

### Covariate "Due60"

The same procedures are repeated for the covariates "Due60" and "Due90". Feel
free to skip this part and the next.


```r
Due60Table <- CategoricalTable(Target = "Delinquency", Covariate = "Due60",
                               Data = Train)
Due60Table
```

```
##     Due60 Event Non Event Counts Probability       Logit
##  1:     0  5802    108167 113969      0.0509 -2.92547364
##  2:     1  1406      3140   4546      0.3093 -0.80347401
##  3:     2   454       433    887      0.5118  0.04735947
##  4:     3   138       114    252      0.5476  0.19105524
##  5:     4    58        32     90      0.6444  0.59470711
##  6:     5    19        10     29      0.6552  0.64185389
##  7:     6     9         3     12      0.7500  1.09861229
##  8:     7     4         2      6      0.6667  0.69314718
##  9:     8     0         1      1      0.0000        -Inf
## 10:    11     1         0      1      1.0000         Inf
## 11:    96     3         1      4      0.7500  1.09861229
## 12:    98   112        91    203      0.5517  0.20763936
```

There are very little observations having values greater than 2. As such, they
will be grouped together. Again, 96 and 98 will be grouped together separately.

Then, the table is updated.


```r
Due60Breaks <- c(-Inf, 0:2, 95, Inf)
Due60Table <- NumericalTable(Target = "Delinquency", Covariate = "Due60",
                             Data = Train, CustomBins = T,
                             CustomIntervals = Due60Breaks)
Due60Table
```

```
##        Due60 Event Non Event Counts Probability       Logit
## 1:  [-Inf,0]  5802    108167 113969      0.0509 -2.92547364
## 2:     (0,1]  1406      3140   4546      0.3093 -0.80347401
## 3:     (1,2]   454       433    887      0.5118  0.04735947
## 4:    (2,95]   229       162    391      0.5857  0.34612567
## 5: (95, Inf]   115        92    207      0.5556  0.22314355
```

The covariate "Due60" is discretized and the logit scores are extracted.

```r
Train[, Due60Cat := cut(Due60, breaks = Due60Breaks, include.lowest = T)]
Train <- GenerateLogit(Data = Train, CrossTable = Due60Table,
                       Covariate = "Due60Cat")
```


```r
head(unique(Train[, c("Due60Cat", "Due60CatLogit")]))
```

```
##     Due60Cat Due60CatLogit
## 1:  [-Inf,0]   -2.92547364
## 2:     (0,1]   -0.80347401
## 3:     (1,2]    0.04735947
## 4:    (2,95]    0.34612567
## 5: (95, Inf]    0.22314355
```

### Covariate "Due90"


```r
Due90Table <- CategoricalTable(Target = "Delinquency", Covariate = "Due90",
                               Data = Train)
Due90Table
```

```
##     Due90 Event Non Event Counts Probability      Logit
##  1:     0  5244    108069 113313      0.0463 -3.0256853
##  2:     1  1403      2813   4216      0.3328 -0.6956387
##  3:     2   611       636   1247      0.4900 -0.0401016
##  4:     3   316       226    542      0.5830  0.3352072
##  5:     4   151        71    222      0.6802  0.7546000
##  6:     5    66        35    101      0.6535  0.6343067
##  7:     6    42        27     69      0.6087  0.4418328
##  8:     7    24         7     31      0.7742  1.2321437
##  9:     8    13         4     17      0.7647  1.1786550
## 10:     9    13         4     17      0.7647  1.1786550
## 11:    10     4         3      7      0.5714  0.2876821
## 12:    11     1         2      3      0.3333 -0.6931472
## 13:    12     0         1      1      0.0000       -Inf
## 14:    13     1         2      3      0.3333 -0.6931472
## 15:    14     1         1      2      0.5000  0.0000000
## 16:    15     0         1      1      0.0000       -Inf
## 17:    17     1         0      1      1.0000        Inf
## 18:    96     3         1      4      0.7500  1.0986123
## 19:    98   112        91    203      0.5517  0.2076394
```

There are values greater than 3 are grouped together, with the exceptions of
96 and 98.


```r
Due90Breaks <- c(-Inf, 0:3, 95, Inf)
Due90Table <- NumericalTable(Target = "Delinquency", Covariate = "Due90",
                             Data = Train, CustomBins = T,
                             CustomIntervals = Due90Breaks)
Due90Table
```

```
##        Due90 Event Non Event Counts Probability      Logit
## 1:  [-Inf,0]  5244    108069 113313      0.0463 -3.0256853
## 2:     (0,1]  1403      2813   4216      0.3328 -0.6956387
## 3:     (1,2]   611       636   1247      0.4900 -0.0401016
## 4:     (2,3]   316       226    542      0.5830  0.3352072
## 5:    (3,95]   317       158    475      0.6674  0.6963067
## 6: (95, Inf]   115        92    207      0.5556  0.2231436
```

The covariate "Due90" is discretized and the logit scores are extracted.


```r
Train[, Due90Cat := cut(Due90, breaks = Due90Breaks, include.lowest = T)]
Train <- GenerateLogit(Data = Train, CrossTable = Due90Table,
                       Covariate = "Due90Cat")
```


```r
head(unique(Train[, c("Due90Cat", "Due90CatLogit")]))
```

```
##     Due90Cat Due90CatLogit
## 1:  [-Inf,0]    -3.0256853
## 2:     (0,1]    -0.6956387
## 3:     (1,2]    -0.0401016
## 4:     (2,3]     0.3352072
## 5:    (3,95]     0.6963067
## 6: (95, Inf]     0.2231436
```


## Assessing the Performance of a Model: AUROC(), CovariateWeights(), and LogLoss()

Before model building, it is a standard practice to standardize the dataset
in the field of credit scoring.

I have simplified the dataset to only include the target "Delinquency" and all
the logit scores, i.e. columns with the term "Logit".


```r
Train <- data.table(cbind(Train[, Delinquency]),
                    scale(Train[, grepl("Logit", names(Train)),
                                with = F]))
setnames(Train, "V1", "Delinquency")
```

Over here, I have built a simple model using logistic regression, employing
just the four covariates listed above.


```r
ScoringModel <- glm(Delinquency ~ AgeCatLogit + Due30CatLogit + Due60CatLogit +
                      Due90CatLogit, data = Train, family = binomial()) 
```

While the relative importance of the covariates can be obtained from the
coefficients using summary(), it can be a process that gives a bit of hassle.


```r
summary(ScoringModel)
```

```
## 
## Call:
## glm(formula = Delinquency ~ AgeCatLogit + Due30CatLogit + Due60CatLogit + 
##     Due90CatLogit, family = binomial(), data = Train)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.4320  -0.3123  -0.2771  -0.1995   2.9268  
## 
## Coefficients:
##                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -3.099701   0.015654 -198.01   <2e-16 ***
## AgeCatLogit    0.433069   0.016041   27.00   <2e-16 ***
## Due30CatLogit  0.435372   0.009591   45.40   <2e-16 ***
## Due60CatLogit  0.234202   0.008288   28.26   <2e-16 ***
## Due90CatLogit  0.418030   0.007663   54.55   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 58815  on 119999  degrees of freedom
## Residual deviance: 46589  on 119995  degrees of freedom
## AIC: 46599
## 
## Number of Fisher Scoring iterations: 6
```

The function CovariateWeights() can be used to provide the relative importance
of the covariates, from a scale of 0 to 100.


```r
CovariateWeights(ScoringModel)
```

```
##   AgeCatLogit Due30CatLogit Due60CatLogit Due90CatLogit 
##         28.48         28.63         15.40         27.49
```

Other than that, the function AUROC() computes the area under the curve, much
faster than auc() from the package pROC(). Compared to pROC::auc(), the
downside of using LiteXploreR::AUROC() is that it only provides the area under
the curve itself, rather than returning a list of the details on the
computation.


```r
AUROC(Train[, Delinquency], ScoringModel$fitted.values)
```

```
## [1] 0.8167836
```

```r
auc(Train[, Delinquency], ScoringModel$fitted.values)
```

```
## Area under the curve: 0.8168
```

```r
system.time(AUROC(Train[, Delinquency], ScoringModel$fitted.values))
```

```
##    user  system elapsed 
##    0.00    0.02    0.02
```

```r
system.time(auc(Train[, Delinquency], ScoringModel$fitted.values))
```

```
##    user  system elapsed 
##    1.74    0.25    1.98
```

In addition, a function for logarithmic loss is included in this package as
well. This is another commonly used function not covered by base R. The
function LogLoss() in this package only supports binary predictions, as
opposed to predictions involving more than two classes.


```r
LogLoss(Train[, Delinquency], ScoringModel$fitted.values)
```

```
## [1] 0.1941225
```


