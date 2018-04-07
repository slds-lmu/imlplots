
modelCheck <- function(data, var, models) {
  # provides a basic model check upon initialization, whether marginalPrediction
  # can use each model to properly predict
  # if a model cannot be used, the session will be stopped and a warning printed
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
