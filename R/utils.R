#' Model check function
#' @description
#' Some mlr learners, like \code{classif.lda} or \code{classif.qda}, can't be
#' used in the marginalPrediction function from the \pkg{mmpf} package.
#' This \code{modelCheck()} allows only the usage of models that work together
#' with the \pkg{mmpf} package.
#' @param data The input data for `marginalPrediction()`
#' @param var The variables of data
#' @param models One or several models that are checked from this function
#' @note The main function is \code{imlplots(data, task, models)}, check out
#' \code{?imlplots} for more information.
modelCheck <- function(data, var, models) {
  error_list <- lapply(models, FUN = function(mod) {
    tryCatch(
      {
        marginalPrediction(
          data, vars = var, n = c(1, 1),
          model = mod[["learner.model"]],
          predict.fun = function(object, newdata) {
            predict(object, newdata = newdata)
          },
          aggregate.fun = function(x) {
            c(identity(x), ave = mean(x))
          }
        )
      },
      error = function(e) return(e),
      warning = function(w) return(w)
    )
  })
  error_list <- vapply(error_list, FUN = function(i) {
    any(class(i) == "error")}, FUN.VALUE = logical(1)
  )
  if (any(TRUE %in% error_list)) {
    message("Model check failed. The following models cannot be used to predict properly.")
    print(models[error_list])
    stop("Aborting..", call. = FALSE)
  } else {
    message("Model check successful. Proceeding..")
  }
}
