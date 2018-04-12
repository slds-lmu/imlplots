#' Boston housing
#'
#' A dataset that contains entries that shows median housing values for Boston.
#'
#' @docType data
#'
#' @usage data(boston)
#'
#' @format A data frame with 506 rows and 14 columns:
#' \describe{
#' \item{\code{crim}}{per capita crime rate by town}
#' \item{\code{zn}}{proportion of residential land zoned for lots over 25,000 sq.ft}
#' \item{\code{indus}}{proportion of non-retail business acres per town}
#' \item{\code{chas}}{Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)}
#' \item{\code{nox}}{nitrogen oxides concentration (parts per 10 million)}
#' \item{\code{rm}}{average number of rooms per dwelling}
#' \item{\code{age}}{proportion of owner-occupied units built prior to 1940}
#' \item{\code{dis}}{weighted mean of distances to five Boston employment centres}
#' \item{\code{rad}}{index of accessibility to radial highways}
#' \item{\code{tax}}{full-value property-tax rate per \$10,000}
#' \item{\code{ptratio}}{pupil-teacher ratio by town}
#' \item{\code{black}}{1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town}
#' \item{\code{lstat}}{lower status of the population (percent)}
#' \item{\code{medv}}{target variable, median value of owner-occupied homes in \$1000s}
#' }
#' @source \code{library(MASS)}
#' @source Harrison, D. and Rubinfeld, D.L. (1978) Hedonic prices and the demand for clean air. J. Environ. Economics and Management 5, 81 - 102
#' @source Belsley D.A., Kuh, E. and Welsch, R.E. (1980) Regression Diagnostics. Identifying Influential Data and Sources of Collinearity. New York: Wiley.
"boston"




#' Burned area due to fire
#'
#' A dataset that contains entries that report the burned area due to fires.
#'
#' @docType data
#'
#' @usage data(fire)
#'
#' @format A data frame with 517 rows and 11 columns:
#' \describe{
#' \item{\code{month}}{month of the year: "jan" to "dec"}
#' \item{\code{day}}{day of the week: "mon" to "sun"}
#' \item{\code{FFMC}}{FFMC index from the FWI system: 18.7 to 96.20}
#' \item{\code{DMC}}{DMC index from the FWI system: 1.1 to 291.3}
#' \item{\code{DC}}{DC index from the FWI system: 7.9 to 860.6}
#' \item{\code{ISI}}{ISI index from the FWI system: 0.0 to 56.10}
#' \item{\code{temp}}{temperature in Celsius degrees: 2.2 to 33.30}
#' \item{\code{RH}}{relative humidity in \%: 15.0 to 100}
#' \item{\code{wind}}{wind speed in km/h: 0.40 to 9.40}
#' \item{\code{rain}}{outside rain in mm/m2 : 0.0 to 6.4}
#' \item{\code{area}}{target variable, the burned area of the forest (in ha): 0.00 to 1090.84}
#' }
#' @source \url{https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/}
"fire"




#' Titanic Passenger Survival Data Set
#'
#' A data set that has survival data of the titanic passengers
#'
#' @docType data
#'
#' @usage data(titanic)
#'
#' @format A data frame with 1309 rows and 8 columns:
#' \describe{
#' \item{\code{Pclass}}{Ticket class, 1 = 1st, 2 = 2nd, 3 = 3rd}
#' \item{\code{Survived}}{Survival, 0 = No, 1 = Yes}
#' \item{\code{Sex}}{Sex}
#' \item{\code{Age}}{Age in years}
#' \item{\code{Sibsp}}{# of siblings / spouses aboard the Titanic}
#' \item{\code{Parch}}{# of parents / children aboard the Titanic}
#' \item{\code{Fare}}{Passenger fare}
#' \item{\code{Embarked}}{Port of Embarkation, C = Cherbourg, Q = Queenstown, S = Southampton}
#' }
#' @source \url{https://www.kaggle.com/c/titanic/data}
"titanic"

