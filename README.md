
imlplots: interpretable machine learning plots
==============================================

`imlplots` is an R package that provide an interactive Shiny dashboard for three kinds of Interpretable Machine Learning (IML) plots

-   Partial Dependence Plots (PDP)
-   Individual Conditional Expectation (ICE) plots
-   Accumulated Local Effect (ALE) plots

The following video showcases the functionalities of the `imlplots` package:  
https://www.youtube.com/watch?v=V6NIHtrKKEA

Installation
============

The package can be installed directly from github with devtools

``` r
# install.packages("devtools")
devtools::install_github('juliafried/imlplots')
library(imlplots)
```

Quickstart
==========

You can fit classification and regression problems from the `mlr` package and analyse possible interaction effects in a Shiny dasbhoard.

For quickstart we take the popular Boston Housing data, where we want to predict the median housing price in Boston.

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

For using `imlplots` Shiny dashboard, three input arguments need to be specified

-   `data` - the input data
-   `task`- the learning task
-   `models` - one or several trained models

We create a regression task with `medv` as target variable. The task structure is determined by `mlr` package.

``` r
boston.task = makeRegrTask(data = boston, target = "medv")
```

The `imlplots` dashboard allows the comparison of multiple learning algorithms, therefore we fit two different models - first a random forest and second a GLM.

``` r
rf.mod = train("regr.randomForest", boston.task)
glm.mod = train("regr.glm", boston.task)
```

The input for the Shiny app is a list of learners.

``` r
mod.list = list(rf.mod, glm.mod)
```

Now the Shiny app can be used.

``` r
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
