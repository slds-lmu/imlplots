#' Generate Prediction Sample
#' @description
#' This function generates samples for the prediction function.
#' @param data The input data for model
#' @param var The variables of model
#' @param knots The amout of knots
#' @param lines The amout of lines
#' @param model The fitted model
#' @param type The type of learning problem (classification or regression)
#' @note The main function is \code{imlplots(data, task, models)}. Get more
#' information with \code{?imlplots}.
makePredictionsSampled <- function(data, var, knots, lines, model, type) {

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

makePredictionsSelected <- function(data, var, model, knots, selected_rows, type) {

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
  dropped_var = predictions[, !(colnames(predictions) %in% var), with = FALSE]
  match_index = match(centerpoint, predictions[ , 1, which = FALSE])
  centered = apply(dropped_var, 1, '-', dropped_var[match_index, ])
  centered = do.call(rbind.data.frame, centered)
  centered_pred = cbind(predictions[, var, with = FALSE, drop = FALSE], centered)
  return(centered_pred)
}
