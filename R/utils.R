getDecimalPlaces = function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    decimal.places = nchar(
      strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
    return(decimal.places)
  } else {
    return(0L)
  }
}

createSliderStepLength = function(decimal.places) {
  if (decimal.places > 0) {
    step.vec = c("0.", as.character(rep(0, decimal.places -1)), "1")
    step = paste(step.vec, collapse = "")
    step.length = as.numeric(step)
  } else {
    step.length = 1L
  }
  return(step.length)
}
