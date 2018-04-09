
checkSingleModel = function(data, features, model) {
  # checks for a single model, whether marginalPrediction() can use it to
  # predict with
  #
  # Args:
  #   data (data frame): data frame of test data containing exactly the
  #                      same variables as training data
  #   features (string): single or multiple features
  #   model (a trained model): a trained model that supports the predict()
  #                            function
  # Returns:
  #   a string. "error" if any model / feature combination threw an error
  #             "pass" if otherwise
  error.list <- lapply(features, FUN = function(feature.name) {
    tryCatch(
      {
        marginalPrediction(
          data,
          vars = feature.name,
          n = c(1, 1),
          model = model,
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
  error.list <- vapply(error.list, FUN = function(i) {
    any(class(i) == "error")}, FUN.VALUE = logical(1)
  )
  if (any(TRUE %in% error.list)) {
    return("error")
  } else {
    return("pass")
  }
}

checkAllModels = function(data, features, models) {
  # checks for multiple models, whether marginalPrediction() can use them to
  # predict with
  #
  # Args:
  #   data (data frame): data frame of test data containing exactly the
  #                      same variables as training data
  #   features (string): single or multiple features
  #   models (list): a list of trained models that support the predict() function
  # Returns:
  #   a string vector of length = length(models). "error" if error ocurred
  #   during marginalPrediction() call. "pass" otherwise
  check.list = lapply(models, FUN = function(mod) {
    do.call(
      checkSingleModel,
      list(data = data, model = mod$learner.model, features = features))
  })
}

modelCheck = function(data, features, models, model.check) {
  # function call to provide a model check for imlplots()
  # calls checkSingleModel() or checkAllModels() internally
  #
  # Args:
  #   data (data frame): data frame of test data containing exactly the
  #                      same variables as training data
  #   features (string): single or multiple features
  #   models (list): a list of trained models that support the predict() function
  #   model.check (string): "all.features" if all features are to be checked
  #                         "sample.feature" if one feature is to be sampled
  #                         (faster)
  #   Returns:
  #   prints an error message if any model threw error and stops the imlplots
  #   call or prints a message, that no errors occurred.
  if (model.check == "all.features") {
    check.list = checkAllModels(data, features, models)
  } else if (model.check == "sample.feature") {
    feature.sampled = sample(features, 1)
    check.list = checkAllModels(data, feature.sampled, models)
  } else {
    error.msg = paste(
      "Wrong value for model.check. Insert either 'all.features' or",
      "'sample.feature'")
    stop(error.msg, call. = FALSE)
  }
  if (any("error" %in% check.list)) {
    message("Model check failed. The following models cannot be used to predict properly.")
    print(models[check.list %in% "error"])
    stop("Aborting..", call. = FALSE)
  } else {
    message("Model check successful. Proceeding..")
  }
}
