Introduction of {LXR}
=====================

This package compiles some functions for quick, dirty, and repetitive
exploratory analysis which may otherwise lengthen the script
unnecessarily. Emphasis is placed on the purpose of credit scoring,
where WoE modelling (Semi Naive Bayesian Classifier) is routinely used.
The functions caters to the prediction of binary target variable.

How to Download this Package
============================

As {LXR} is hosted on GitHub, you will need to use the function
install\_github() for the package {devtools} to install it.

To do that, you need to run the following lines of code in your R
console:

install.packages(“devtools”)

library(devtools)

install\_github(“Then-Terence/LXR”)

Setting Up the Workspace
========================

First of all, I am going to load the package {LXR} and a credit dataset
obtained from Kaggle
(<a href="https://www.kaggle.com/c/GiveMeSomeCredit/data" class="uri">https://www.kaggle.com/c/GiveMeSomeCredit/data</a>).
I have split them into training, validation and testing set.

    # Load the Package
    library(LXR)

    ## Loading required package: data.table

    # Load the Data
    load("../Output/TrainDT.RData")
    TrainDT <- data.table(TrainDT)

This is how the workspace looks like:

    ls()

    ## [1] "TrainDT"

Let us start by looking at the training set.

    head(TrainDT)

    ##    ID Delinquency Age Income DebtRatio Dependents CreditLinesAndLoans
    ## 1: 10           0  57  23684 0.6062909          2                   9
    ## 2: 18           0  53   8800 0.1882741          0                   7
    ## 3: 22           1  38   3000 0.4758414          2                   7
    ## 4: 81           0  52  10833 0.1111316          1                  14
    ## 5: 83           0  53  12414 2.6045107          2                  13
    ## 6: 94           0  34   3050 0.3684038          1                   4
    ##    RealEstateLoans RevolvingUtilization DPD30 DPD60 DPD90
    ## 1:               4           0.18916905     0     0     0
    ## 2:               0           0.16628408     0     0     0
    ## 3:               1           0.02565568     0     0     0
    ## 4:               2           0.18331667     0     0     0
    ## 5:               1           0.01725898     0     0     0
    ## 6:               0           0.88882224     0     0     0

Demonstration of Functions
==========================

Overview of a Dataset: CovariateSummary()
-----------------------------------------

In cases where the number of columns/ covariates is overwhelming, they
can be screened out using the function CovariateSummary, which gives the
AUC value of the raw covariates.

    Overview <- CovariateSummary(Target = "Delinquency", DT = TrainDT, Skip = "ID")
    Overview

    ##                Covariate    Type Count of NA Count of 0    AUC
    ##  1:                  Age integer           0          0 0.6097
    ##  2:               Income integer           0         12 0.5753
    ##  3:            DebtRatio numeric           0        191 0.5668
    ##  4:           Dependents integer           0       5369 0.5393
    ##  5:  CreditLinesAndLoans integer           0         72 0.5294
    ##  6:      RealEstateLoans integer           0       3369 0.5258
    ##  7: RevolvingUtilization numeric           0        677 0.7364
    ##  8:                DPD30 integer           0       8440 0.6726
    ##  9:                DPD60 integer           0       9571 0.5908
    ## 10:                DPD90 integer           0       9545 0.6307

The column “Type” serves as a check for the classes of the columns, i.e.
whether they are imported correctly.

For the purpose of this demonstration, I will only use the covariates
“Age”, “DPD30”, “DPD60”, “DPD90” in the following sections.

Exploring and Transforming a Numerical Covariate: NumericalTable(), CategorialTable() and GenerateLogit()
---------------------------------------------------------------------------------------------------------

### Covariate “Age”"

Here, a table is constructed by dividing “Age” into 5 categories of
roughly equal sizes.

    AgeTable <- NumericalTable(Target = "Delinquency", Covariate = "Age",
                               DT = TrainDT, NumberOfBins = 5)

The table looks something like this:

    AgeTable

    ##        Age Event Non Event Counts Probability   Logit
    ## 1: [22,39]   190      1997   2187      0.0869 -2.3524
    ## 2: (39,47]   152      1766   1918      0.0792 -2.4526
    ## 3: (47,55]   137      1955   2092      0.0655 -2.6582
    ## 4: (55,63]    84      1813   1897      0.0443 -3.0719
    ## 5: (63,80]    54      1852   1906      0.0283 -3.5350

For every category, the table shows the number of observations, the
number of observations that take the value of 1 for Delinquency, those
who take the value of 0 for delinquency, the probability, and the logit
score.

The logit is the log odds, which is the numerical equivalent used in
logistic regression.

The cutoffs can be obtained from the function quantile() in base R. Its
minimum and maximum are replaced by 0 and Inf respectively.

    AgeBreaks <- quantile(TrainDT[, Age], probs = seq(0, 1, 1/5))
    AgeBreaks <- c(0, AgeBreaks[-c(1, 6)], Inf)

Then, the table is updated with the thresholds, the covariate is
discretized, and the logit scores are extracted from the table.

    AgeTable <- NumericalTable("Delinquency", "Age", TrainDT,
                               UseCustomIntervals =  T,
                               CustomIntervals = AgeBreaks)
    Cut(TrainDT, "AgeGroup", Age, AgeBreaks)
    GenerateLogit(DT = TrainDT, CrossTable = AgeTable, Covariate = "AgeGroup")

The function generates another column with the logit scores and will be
named by appending the original column name followed by “Logit”.

In this case, a new column named “AgeGroupLogit” is generated.

    unique(TrainDT[, .(AgeGroup, AgeGroupLogit)][order(AgeGroup), ])

    ##    AgeGroup AgeGroupLogit
    ## 1:   [0,39]       -2.3524
    ## 2:  (39,47]       -2.4526
    ## 3:  (47,55]       -2.6582
    ## 4:  (55,63]       -3.0719
    ## 5: (63,Inf]       -3.5350

### Covariate “DPD30”

While the covariate “DPD30” is numerical, it may be helpful for treating
it as categorical in the first inspection. This is due to it having a
limited number of unique values, and not being able to split into equal
categories as most of the borrowers will have the value of 0.

    DPD30Table <- CategoricalTable(Target = "Delinquency", Covariate = "DPD30",
                                   DT = TrainDT)
    DPD30Table

    ##    DPD30 Event Non Event Counts Probability   Logit
    ## 1:     0   328      8112   8440      0.0389 -3.2081
    ## 2:     1   157       923   1080      0.1454 -1.7714
    ## 3:     2    70       216    286      0.2448 -1.1268
    ## 4:     3    38        78    116      0.3276 -0.7191
    ## 5:     4    16        30     46      0.3478 -0.6286
    ## 6:     5     5        17     22      0.2273 -1.2238
    ## 7:     6     2         3      5      0.4000 -0.4055
    ## 8:     7     1         3      4      0.2500 -1.0986
    ## 9:     8     0         1      1      0.0000    -Inf

There are very little observations having values greater than 4. As
such, they will be grouped together.

Then, the table is updated.

    DPD30Breaks <- c(-Inf, 0:2, Inf)
    DPD30Table <- NumericalTable(Target = "Delinquency", Covariate = "DPD30",
                                 DT = TrainDT, UseCustomIntervals = T,
                                 CustomIntervals = DPD30Breaks)
    DPD30Table

    ##       DPD30 Event Non Event Counts Probability   Logit
    ## 1: [-Inf,0]   328      8112   8440      0.0389 -3.2081
    ## 2:    (0,1]   157       923   1080      0.1454 -1.7714
    ## 3:    (1,2]    70       216    286      0.2448 -1.1268
    ## 4: (2, Inf]    62       132    194      0.3196 -0.7557

The covariate “Due30” is discretized and the logit scores are extracted.

    Cut(TrainDT, "DPD30Group", DPD30, DPD30Breaks)
    GenerateLogit(DT = TrainDT, CrossTable = DPD30Table, Covariate = "DPD30Group")

    unique(TrainDT[, .(DPD30Group, DPD30GroupLogit)][order(DPD30Group), ])

    ##    DPD30Group DPD30GroupLogit
    ## 1:   [-Inf,0]         -3.2081
    ## 2:      (0,1]         -1.7714
    ## 3:      (1,2]         -1.1268
    ## 4:   (2, Inf]         -0.7557

### Covariate “DPD60”

The same procedures are repeated for the covariates “DPD60” and “Due90”.
Feel free to skip this part and the next.

    DPD60Table <- CategoricalTable(Target = "Delinquency", Covariate = "DPD60",
                                   DT = TrainDT)
    DPD60Table

    ##    DPD60 Event Non Event Counts Probability   Logit
    ## 1:     0   486      9085   9571      0.0508 -2.9282
    ## 2:     1    92       255    347      0.2651 -1.0195
    ## 3:     2    26        32     58      0.4483 -0.2076
    ## 4:     3     9         8     17      0.5294  0.1178
    ## 5:     4     3         3      6      0.5000  0.0000
    ## 6:     6     1         0      1      1.0000     Inf

There are very little observations having values greater than 1. As
such, they will be grouped together.

Then, the table is updated.

    DPD60Breaks <- c(-Inf, 0:1, Inf)
    DPD60Table <- NumericalTable(Target = "Delinquency", Covariate = "DPD60",
                                 DT = TrainDT, UseCustomIntervals = T,
                                 CustomIntervals = DPD60Breaks)
    DPD60Table

    ##       DPD60 Event Non Event Counts Probability   Logit
    ## 1: [-Inf,0]   486      9085   9571      0.0508 -2.9282
    ## 2:    (0,1]    92       255    347      0.2651 -1.0195
    ## 3: (1, Inf]    39        43     82      0.4756 -0.0976

The covariate “DPD60” is discretized and the logit scores are extracted.

    Cut(TrainDT, "DPD60Group", DPD60, DPD60Breaks)
    GenerateLogit(DT = TrainDT, CrossTable = DPD60Table, Covariate = "DPD60Group")

    unique(TrainDT[, .(DPD60Group, DPD60GroupLogit)])

    ##    DPD60Group DPD60GroupLogit
    ## 1:   [-Inf,0]         -2.9282
    ## 2:      (0,1]         -1.0195
    ## 3:   (1, Inf]         -0.0976

### Covariate “DPD90”

    DPD90Table <- CategoricalTable(Target = "Delinquency", Covariate = "DPD90",
                                   DT = TrainDT)
    DPD90Table

    ##     DPD90 Event Non Event Counts Probability   Logit
    ##  1:     0   439      9106   9545      0.0460 -3.0322
    ##  2:     1    87       211    298      0.2919 -0.8860
    ##  3:     2    49        39     88      0.5568  0.2283
    ##  4:     3    23        13     36      0.6389  0.5705
    ##  5:     4     6         6     12      0.5000  0.0000
    ##  6:     5     5         1      6      0.8333  1.6094
    ##  7:     6     3         2      5      0.6000  0.4055
    ##  8:     7     3         2      5      0.6000  0.4055
    ##  9:     8     1         0      1      1.0000     Inf
    ## 10:     9     1         2      3      0.3333 -0.6931
    ## 11:    13     0         1      1      0.0000    -Inf

    DPD90Breaks <- c(-Inf, 0:2, Inf)
    DPD90Table <- NumericalTable(Target = "Delinquency", Covariate = "DPD90",
                                 DT = TrainDT, UseCustomIntervals = T,
                                 CustomIntervals = DPD90Breaks)
    DPD90Table

    ##       DPD90 Event Non Event Counts Probability   Logit
    ## 1: [-Inf,0]   439      9106   9545      0.0460 -3.0322
    ## 2:    (0,1]    87       211    298      0.2919 -0.8860
    ## 3:    (1,2]    49        39     88      0.5568  0.2283
    ## 4: (2, Inf]    42        27     69      0.6087  0.4418

The covariate “DPD90” is discretized and the logit scores are extracted.

    Cut(TrainDT, "DPD90Group", DPD90, DPD90Breaks)
    GenerateLogit(DT = TrainDT, CrossTable = DPD90Table, Covariate = "DPD90Group")

    unique(TrainDT[, .(DPD90Group, DPD90GroupLogit)][order(DPD90Group), ])

    ##    DPD90Group DPD90GroupLogit
    ## 1:   [-Inf,0]         -3.0322
    ## 2:      (0,1]         -0.8860
    ## 3:      (1,2]          0.2283
    ## 4:   (2, Inf]          0.4418

Assessing the Performance of a Model: AUROC(), CovariateWeights()
-----------------------------------------------------------------

Before model building, it is a standard practice to standardize the
dataset in the field of credit scoring.

I have simplified the dataset to only include the target “Delinquency”
and all the logit scores, i.e. columns with the term “Logit”.

    DT <- data.table(cbind(TrainDT[, Delinquency]),
                     scale(TrainDT[, grepl("Logit", names(TrainDT)),
                                    with = F]))
    setnames(DT, "V1", "Delinquency")

Over here, I have built a simple model using logistic regression,
employing just the four covariates listed above.

    ScoringModel <-
      glm(Delinquency ~ AgeGroupLogit + DPD30GroupLogit + DPD60GroupLogit +
            DPD90GroupLogit, binomial(), DT) 

While the relative importance of the covariates can be obtained from the
coefficients using summary(), it can be a process that gives a bit of
hassle.

    summary(ScoringModel)

    ## 
    ## Call:
    ## glm(formula = Delinquency ~ AgeGroupLogit + DPD30GroupLogit + 
    ##     DPD60GroupLogit + DPD90GroupLogit, family = binomial(), data = DT)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.2833  -0.3022  -0.2672  -0.2260   2.8469  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     -3.11456    0.05358 -58.124  < 2e-16 ***
    ## AgeGroupLogit    0.35634    0.05359   6.649 2.95e-11 ***
    ## DPD30GroupLogit  0.45144    0.03319  13.604  < 2e-16 ***
    ## DPD60GroupLogit  0.19552    0.02836   6.894 5.44e-12 ***
    ## DPD90GroupLogit  0.40108    0.02555  15.698  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 4632.4  on 9999  degrees of freedom
    ## Residual deviance: 3819.5  on 9995  degrees of freedom
    ## AIC: 3829.5
    ## 
    ## Number of Fisher Scoring iterations: 6

The function CovariateWeights() can be used to provide the relative
importance of the covariates, from a scale of 0 to 100.

    CovariateWeights(ScoringModel)

    ##   AgeGroupLogit DPD30GroupLogit DPD60GroupLogit DPD90GroupLogit 
    ##           25.37           32.14           13.92           28.56

The function AUROC() computes the area under the curve. However, the
downside of using LXR::AUROC() is that it only provides the area under
the curve itself, rather than returning a list of the details on the
computation.

    AUROC(DT[, Delinquency], ScoringModel[["fitted.values"]])

    ## [1] 0.7830845
