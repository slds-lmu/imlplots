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
#' \item{\code{month}}{month of the year: "1" (Jan) to "12" (Dec)}
#' \item{\code{day}}{day of the week: "1" (Mon) to "7" (Sun)}
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
