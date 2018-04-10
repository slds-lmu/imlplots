makePredictionsIceSampled = function(data, var, knots, lines, model, task.type) {
  # create Monte Carlo estimates for ICE and PDP curve with random sampling
  #
  # Args:
  #   data (data frame): data frame of test data containing exactly the
  #                      same variables as training data
  #   var (string): selected variable of interest on horizontal axis
  #   knots (numeric): sampled unique values of var
  #   lines (numeric): sampled observations to create ICE curves for
  #   model (obj): mlr trained model
  #   task.type (string): "regr" or "classif" for regression and classification tasks
  # Returns:
  #   a data frame with one column containing all sampled unique values of var;
  #   as many columns as lines with predictions produced by model (ICE curves)
  #   one additonal column that averages the ICE curves to a PDP estimate
  if (task.type == "regr") {

    prediction = marginalPrediction(
      data = data,
      vars = var,
      n = c(knots, lines),
      model = model,
      aggregate.fun = function(x){
        c(identity(x), "ave" = mean(x))
      }
    )

  } else if (task.type == "classif") {

    prediction = marginalPrediction(
      data = data,
      vars = var,
      n = c(knots, lines),
      model = model,
      aggregate.fun = function(x) {
        c("preds" = identity(x), "ave" = mean(x))
      },
      predict.fun = function(object, newdata) {
        predict(object, newdata = newdata, type = "prob")
      }
    )
  }
  var.vector = prediction[, 1, which = FALSE]
  if (is.numeric(var.vector)) {
    prediction[ , 1] = round(var.vector, digits = 5)
  } else {}
  # marginalPrediction (per default) samples from a uniform distribution on the
  # [min; max] interval; the sampled values do not have a decimal point cutoff
  # this can cause problems during the centering call when comparing the sampled
  # horizontal axis values with selections from the UI, which have a cut off at
  # 5 decimal digits
  return(prediction)
}

makePredictionsIceSelected = function(data, var, model, knots, selected.rows,
                                       task.type) {
  # create Monte Carlo estimates for ICE and PDP curves, marginalize only over
  # specific observations/rows
  #
  # Args:
  #   data (data frame): data frame of test data containing exactly the
  #                      same variables as training data
  #   var (string): selected variable of interest on horizontal axis
  #   model (obj): mlr trained model
  #   knots (numeric): sampled unique values of var
  #   selected.rows (numeric): row IDs of data to marginalize over
  #   task.type (string): "regr" or "classif" for regression and classification tasks
  # Returns:
  #   a data frame with one column containing all sampled unique values of var;
  #   as many columns as selected.rows with predictions produced by model
  #   (ICE curves);
  #   one additonal column that averages the ICE curves to a PDP estimate
  if (task.type == "regr") {

    prediction = marginalPrediction(
      data = data,
      vars = var,
      n = knots,
      int.points = which(
        selected.rows %in% rownames(data)
      ),
      model = model,
      aggregate.fun = function(x) {
        c(identity(x), "ave" = mean(x))
      })
  } else if (task.type == "classif") {

    prediction = marginalPrediction(
      data = data,
      vars = var,
      n = knots,
      int.points = which(
        selected.rows %in% rownames(data)
      ),
      model = model,
      aggregate.fun = function(x) {
        c(preds = identity(x), "ave" = mean(x))
      },
      predict.fun = function(object, newdata) {
        predict(object, newdata = newdata, type = "prob")
      }
    )
  }
  var.vector = prediction[, 1, which = FALSE]
  if (is.numeric(var.vector)) {
    prediction[ , 1] = round(var.vector, digits = 5)
  } else {}
  # marginalPrediction (per default) samples from a uniform distribution on the
  # [min; max] interval; the sampled values do not have a decimal point cutoff
  # this can cause problems during the centering call when comparing the sampled
  # horizontal axis values with selections from the UI, which have a cut off at
  # 5 decimal digits
  return(prediction)
}

centerPredictions = function(predictions, center.x, var) {
  # centers ICE predictions for centered ICE plots
  #
  # Args:
  #   predictions (data frame): outputs of makePredictionsIce...()
  #   center.x (numeric): specifies the sampled knot / value on the
  #   horizontal axis where all ICE curves are 'pinched' to 0.
  #   var (string): selected variable of interest on horizontal axis
  # Returns:
  #   see makePredictionsIce...(); center.x row values pinched to 0.
  pred.var.dropped =
    predictions[, !(colnames(predictions) %in% var), with = FALSE]
  # predictions without variable column
  match.index = match(center.x, predictions[ , 1, which = FALSE])
  # match center.x index with according value in variable column
  centered.var.dropped = apply(
    pred.var.dropped, 1, '-', pred.var.dropped[match.index, ])
  # substract the row that contains the center.x from all rows
  centered.var.dropped = do.call(rbind.data.frame, centered.var.dropped)
  # create data frame from list structure containing centered predictions
  pred.centered = cbind(
    predictions[, var, with = FALSE, drop = FALSE],
    centered.var.dropped)
  # bind centered data frame together with variable column
  return(pred.centered)
}

makePredictionsAleRegr = function(data, target, model, var1, var2 = NULL, knots) {
  # create predictions for ALE plots
  #
  # Args:
  #   data (data frame): data frame of test data containing exactly the
  #                      same variables as training data
  #   target (string): target variable for ALE predictions
  #   var1 (string): selected variable of interest on horizontal axis
  #   var2 (string): optional interaction variable for ale plots
  #   knots (numeric): number of intervals into which the predictor range is
  #                    divided when calculating ALE plot effects.
  # Returns:
  #   a data frame with one column containing all sampled unique values of var1;
  #   if var2 is not NULL, one column with sampled unique values of var2
  #   one column with the according ALE effects
  pred.function = function(X.model, newdata) {
    as.numeric(predict(X.model, newdata))
  }
  obj = tryCatch({
    ALEPlot::ALEPlot(
      data[ , -which(names(data) == target)],
      model,
      pred.fun = pred.function,
      J = c(var1, var2),
      K = knots)},
    error = function(e) return(e),
    warning = function(w) return(w)
  )
  # ALEPlot function not (yet) completely reliable
  if (any(class(obj) == "warning") | any(class(obj) == "error")) {
    invisible(return("error"))
  } else {
    # no error or warning
    if (is.null(var2)) {
      df = data.frame(matrix(nrow = length(obj$x.values), ncol = 2))
      colnames(df) = c(var1, "ale.effect")
      df[[var1]] = obj$x.values
      df[["ale.effect"]] = obj$f.values
    } else {
      df = obj$f.values
      rownames(df) = obj$x.values[[1]]
      colnames(df) = obj$x.values[[2]]
      df = melt(df, na.rm = TRUE)
      colnames(df) = c(var1, var2, "ale.effect")
    }
    return(df)
  }
}

makePredictionsAleClassif = function(data, target, model, var) {

  var.levels = levels(data[[target]])

  ale.outputs = lapply(1:length(var.levels), FUN = function(i) {
    pred.function = function(X.model, newdata) {
      predict(X.model, newdata, type = "prob")[, i]}
    # get ALEPlot outputs for each class
    obj = tryCatch({
      ALEPlot::ALEPlot(
      data[ , -which(names(data) == target)],
      model,
      pred.fun = pred.function,
      J = var)},
      error = function(e) return(e),
      warning = function(w) return(w)
    )
    return(obj)
  })
  # ALEPlot function not (yet) completely reliable
  error.check = vapply(ale.outputs, FUN = function(obj) {
    bool = (any(class(obj) == "warning") | any(class(obj) == "error"))
    # if any error or warning is found in a class prediction, print to console
    if (bool == TRUE) {
      warning("ALEPlot error msg:", call. = FALSE)
      print(obj)}
    return(bool)
    },
    FUN.VALUE = logical(1))
  if (TRUE %in% error.check) {
    return("error")
  } else {
    # no errors or warnings
    var.values = ale.outputs[[1]]$x.values
    pred = lapply(ale.outputs, FUN = function(obj) return(obj$f.values))
    pred = do.call(cbind.data.frame, pred)
    pred = do.call(cbind.data.frame, list(var.values, pred))
    colnames(pred) = c(var, var.levels)

    return(pred)
  }
}

makePredictionsAle = function(data, target, model, var1, var2 = NULL, knots,
                              task.type) {
  if (task.type == "regr") {
    pred = makePredictionsAleRegr(data, target, model, var1, var2, knots)
  } else if (task.type == "classif") {
    pred = makePredictionsAleClassif(data, target, model, var = var1)
  }
  return(pred)
}