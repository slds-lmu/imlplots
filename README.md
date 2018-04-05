
imlplots: interpretable machine learning plots
==============================================

`imlplots` is an R package that provide an interactive Shiny dashboard for three kinds of Interpretable Machine Learning (IML) plots

-   Partial Dependence Plots (PDP)
-   Individual Conditional Expectation (ICE) plots
-   Accumulated Local Effect (ALE) plots

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

You can fit classification and regression problems from the `mlr` package and analyse possible interaction effects in the Shiny dasbhoard.

For quickstart we take the popular Boston Housing data, where we want to predict the median housing price in Boston.

``` r
head(boston)
```

    ##      crim zn indus chas   nox    rm  age    dis rad tax ptratio  black
    ## 1 0.00632 18  2.31    0 0.538 6.575 65.2 4.0900   1 296    15.3 396.90
    ## 2 0.02731  0  7.07    0 0.469 6.421 78.9 4.9671   2 242    17.8 396.90
    ## 3 0.02729  0  7.07    0 0.469 7.185 61.1 4.9671   2 242    17.8 392.83
    ## 4 0.03237  0  2.18    0 0.458 6.998 45.8 6.0622   3 222    18.7 394.63
    ## 5 0.06905  0  2.18    0 0.458 7.147 54.2 6.0622   3 222    18.7 396.90
    ## 6 0.02985  0  2.18    0 0.458 6.430 58.7 6.0622   3 222    18.7 394.12
    ##   lstat medv
    ## 1  4.98 24.0
    ## 2  9.14 21.6
    ## 3  4.03 34.7
    ## 4  2.94 33.4
    ## 5  5.33 36.2
    ## 6  5.21 28.7

For using `imlplots` Shiny dashboard, three input arguments need to be specified

-   `data` - the input data
-   `task`- the learning task
-   `models` - one or several trained models

We create a regression task with `medv` as target variable. The task structure is determined by `mlr` package.

``` r
boston.task = makeRegrTask(data = boston, target = "medv")
```

The `imlplots` dashboard allows the comparison of multiple learning algorithms, therefore we fit two different models - first a random forest and second a SVM.

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

References
==========

-   [PDP and ICE plots](https://arxiv.org/pdf/1309.6392.pdf)
-   [mmpf R package](https://github.com/zmjones/mmpf)
-   [ALE plots](https://arxiv.org/abs/1612.08468)
-   [ALE R package](https://cran.r-project.org/web/packages/ALEPlot/index.html)
