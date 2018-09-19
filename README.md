
imlplots: interpretable machine learning plots
==============================================

`imlplots` is an R package that provides an interactive Shiny based dashboard for visualizing feature effects in supervised learning models. The following plots are supported:

-   Partial Dependence Plots (PDP)
-   Individual Conditional Expectation (ICE) plots
-   Accumulated Local Effect (ALE) plots

A video showcases the functionalities of the `imlplots` package:  
https://www.youtube.com/watch?v=V6NIHtrKKEA

Installation
============

``` r
# install.packages("devtools")
devtools::install_github('compstat-lmu/imlplots')
library(imlplots)
```

Quickstart
==========

For a quickstart we take the popular Boston Housing dataset. We want to predict the median housing price in Boston.

``` r
print(summarizeColumns(boston)[, -c(5, 6, 7)], digits = 4)
```

    ##       name    type na     mean       min     max nlevs
    ## 1     crim numeric  0   3.6135   0.00632  88.976     0
    ## 2       zn numeric  0  11.3636   0.00000 100.000     0
    ## 3    indus numeric  0  11.1368   0.46000  27.740     0
    ## 4     chas  factor  0       NA  35.00000 471.000     2
    ## 5      nox numeric  0   0.5547   0.38500   0.871     0
    ## 6       rm numeric  0   6.2846   3.56100   8.780     0
    ## 7      age numeric  0  68.5749   2.90000 100.000     0
    ## 8      dis numeric  0   3.7950   1.12960  12.127     0
    ## 9      rad  factor  0       NA  17.00000 132.000     9
    ## 10     tax numeric  0 408.2372 187.00000 711.000     0
    ## 11 ptratio numeric  0  18.4555  12.60000  22.000     0
    ## 12   black numeric  0 356.6740   0.32000 396.900     0
    ## 13   lstat numeric  0  12.6531   1.73000  37.970     0
    ## 14    medv numeric  0  22.5328   5.00000  50.000     0

There is a single function `imlplots() to access the user interface. You need to specify three input arguments:

-   `data` - the input data frame
-   `task`- a regression or classification task, created with MLR
-   `models` - one or several models, trained with MLR

We create a regression task with `medv` as the target variable.

``` r
boston.task = makeRegrTask(data = boston, target = "medv")
```

You can train multiple models and compare them visually. We fit two different models: a random forest and a generalized linear model.

``` r
rf.mod = train("regr.randomForest", boston.task)
glm.mod = train("regr.glm", boston.task)
```

The trained model objects are saved in a list and passed to the `imlplots()` function:

``` r
mod.list = list(rf.mod, glm.mod)

imlplots(data = boston, task = boston.task, models = mod.list)
```

Code for Copy & Paste
---------------------

``` r
boston.task = makeRegrTask(data = boston, target = "medv")

rf.mod = train("regr.randomForest", boston.task)
glm.mod = train("regr.glm", boston.task)
mod.list = list(rf.mod, glm.mod)

imlplots(data = boston, task = boston.task, models = mod.list)
```

Further Examples
----------------

-   Check out our [Wiki](https://github.com/juliafried/imlplots/wiki)
-   [Vignette](https://github.com/juliafried/imlplots/raw/master/vignettes/imlplots.pdf)

References
==========

-   [References](https://github.com/juliafried/imlplots/raw/master/paper/references.pdf)
