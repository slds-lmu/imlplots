makePredictionsIceSampled <- function(data, var, knots, lines, model, type) {
  # generates the predictions for ICE and PDP curves if sampling mode is
  # selected
  # inputs: see help(marginalPrediction)
  # output: a data table (nrow = knots, ncol = 2 + lines);
  # the first column contains all sampled values of data[var];
  # lines specifies how many lines are to be predicted/displayed;
  # each sampled ICE line consists of one column of predictions;
  # one additonal column aggregates all individual predictions to the PDP
  # average

  if (type == "regr") {

    prediction <- marginalPrediction(
      data = data,
      vars = var,
      n = c(knots, lines),
      model = model,
      aggregate.fun = function(x){
        c(identity(x), "ave" = mean(x))
      }
    )

  } else if (type == "classif") {

    prediction <- marginalPrediction(
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
  prediction[ , 1] <- round(prediction[, 1, which = FALSE, drop = FALSE],
                            digits = 5)
  # marginalPrediction (per default) samples from a uniform distribution on the
  # [min; max] interval; the sampled values do not have a decimal point cutoff
  # this can cause problems during the centering call when comparing the sampled
  # horizontal axis values with selections from the UI, which have a cut off at
  # 5 decimal digits
  return(prediction)
}

makePredictionsIceSelected <- function(data, var, model, knots, selected_rows, type) {
  # generates predictions for ICE and PDP curves if individual mode is selected
  # inputs: see help(marginalPrediction)
  # selected_rows indicates the observations to be marginalized over;
  # output: a data table (nrow = knots, ncol = 2 + selected_rows);
  # the first column contains all sampled values of data[var];
  # the last column contains the average predictions for the PDP
  # each sampled ICE line consists of one column of predictions;
  if (type == "regr") {

    prediction <- marginalPrediction(
      data = data,
      vars = var,
      n = knots,
      int.points = which(
        selected_rows %in% rownames(data)
      ),
      model = model,
      aggregate.fun = function(x) {
        c(identity(x), "ave" = mean(x))
      })
  } else if (type == "classif") {

    prediction <- marginalPrediction(
      data = data,
      vars = var,
      n = knots,
      int.points = which(
        selected_rows %in% rownames(data)
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
  prediction[ , 1] <- round(prediction[, 1, which = FALSE, drop = FALSE],
                            digits = 5)
  # marginalPrediction (per default) samples from a uniform distribution on the
  # [min; max] interval; the sampled values do not have a decimal point cutoff
  # this can cause problems during the centering call when comparing the sampled
  # horizontal axis values with selections from the UI, which have a cut off at
  # 5 decimal digits
  return(prediction)
}

centerPredictions <- function(predictions, centerpoint, var) {
  # centers ICE predictions for centered ICE plots
  # predictions: outputs of makePredictionsIce...()
  # centerpoint: specifies the sampled knot / value on the horizontal axis
  # where all ICE curves are 'pinched' to 0.
  # While a column indicates an individual observation / line,
  # centerpoint indicates a specific row that is to be substracted from each
  # column
  # output: see makePredictionsIce...(); centerpoint row values pinchted to 0.
  dropped_var = predictions[, !(colnames(predictions) %in% var), with = FALSE]
  match_index = match(centerpoint, predictions[ , 1, which = FALSE])
  centered = apply(dropped_var, 1, '-', dropped_var[match_index, ])
  centered = do.call(rbind.data.frame, centered)
  centered_pred = cbind(predictions[, var, with = FALSE, drop = FALSE], centered)
  return(centered_pred)
}

makePredictionsAle <- function(data, target, model, var1, var2 = NULL, knots) {
  # creates ALE plot predictions via ALEPlot package
  # outputs a data.frame with one or two column(s) containing data[var1] and
  # optionally data[var2]; additional column contains ALE predictions
  pred_function = function(X.model, newdata) {
    as.numeric(predict(X.model, newdata))
  }
  obj <- tryCatch(
    {ALEPlot::ALEPlot(
    data[ , -which(names(data) == target)],
    model,
    pred.fun = pred_function,
    J = c(var1, var2),
    K = knots)},
    error = function(e) return(e),
    warning = function(w) return(w)
  )
  # ALEPlot function not (yet) completely reliable
  if (any(class(obj) == "warning") | any(class(obj) == "error")) {
    return(obj)
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
