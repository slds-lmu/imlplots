test_that(
  "ICE prediction in sampling mode returns right dimensions for
  regression tasks", {

    data = fire
    target = "area"
    tsk = makeRegrTask(data = data, target = target)
    mod = train("regr.randomForest", task = tsk)
    n.rows = nrow(data)
    lines = sample(seq(n.rows), 1)
    knots = sample(seq(n.rows), 1)
    features = colnames(data)[-match(target, colnames(data))]
    var = sample(features, 1)
    prediction = makePredictionsIce(
      data = data,
      var = var,
      model = mod$learner.model,
      knots = knots,
      lines = lines,
      task.type = "regr",
      selected.rows = NULL,
      data.selection.mode = "sampling")

    if (is.factor(data[[var]])) {
      expected.rows = length(levels(data[[var]]))
    } else {
      expected.rows = knots
    }
    expected.cols = lines + 2
    expect_data_frame(
      prediction,
      nrows = expected.rows,
      ncols = expected.cols
    )
    expect_true(var %in% colnames(prediction))
    expect_true("preds.ave" %in% colnames(prediction))
  }
)

test_that(
  "ICE prediction in sampling mode returns right dimensions for
  classification tasks", {

    data = iris
    target = "Species"
    tsk = makeClassifTask(data = data, target = target)
    mod = train("classif.randomForest", task = tsk)
    n.rows = nrow(data)
    lines = sample(seq(n.rows), 1)
    knots = sample(seq(n.rows), 1)
    features = colnames(data)[-match(target, colnames(data))]
    var = sample(features, 1)
    levels = levels(data[[target]])
    prediction = makePredictionsIce(
      data = data,
      var = var,
      model = mod$learner.model,
      knots = knots,
      lines = lines,
      task.type = "classif",
      selected.rows = NULL,
      data.selection.mode = "sampling")
    expect_data_frame(
      prediction,
      nrows = knots,
      ncols = length(levels) * lines + 3 + 1
    )
    expect_true(var %in% colnames(prediction))
  }
)

######

test_that(
  "ICE prediction in selection mode returns right dimensions for
  regression tasks", {

    data = fire
    target = "area"
    tsk = makeRegrTask(data = data, target = target)
    mod = train("regr.randomForest", task = tsk)
    n.rows = nrow(data)
    lines = sample(seq(n.rows), 1)
    knots = sample(seq(n.rows), 1)
    features = colnames(data)[-match(target, colnames(data))]
    var = sample(features, 1)
    row.selection = sample(seq(n.rows), sample(seq(n.rows), 1))
    prediction = makePredictionsIce(
      data = data,
      var = var,
      model = mod$learner.model,
      knots = knots,
      task.type = "regr",
      selected.rows = row.selection,
      data.selection.mode = "individual")

    if (is.factor(data[[var]])) {
      expected.rows = length(levels(data[[var]]))
    } else {
      expected.rows = knots
    }
    expected.cols = length(row.selection) + 2
    expect_data_frame(
      prediction,
      nrows = expected.rows,
      ncols = expected.cols
    )
    expect_true(var %in% colnames(prediction))
    expect_true("preds.ave" %in% colnames(prediction))
    }
)

test_that(
  "ICE prediction in selection mode returns right dimensions for
  classification tasks", {

    data = iris
    target = "Species"
    tsk = makeClassifTask(data = data, target = target)
    mod = train("classif.randomForest", task = tsk)
    n.rows = nrow(data)
    lines = sample(seq(n.rows), 1)
    knots = sample(seq(n.rows), 1)
    features = colnames(data)[-match(target, colnames(data))]
    var = sample(features, 1)
    levels = levels(data[[target]])
    row.selection = sample(seq(n.rows), sample(seq(n.rows), 1))
    prediction = makePredictionsIce(
      data = data,
      var = var,
      model = mod$learner.model,
      knots = knots,
      task.type = "classif",
      selected.rows = row.selection,
      data.selection.mode = "individual")
    dim(prediction)
    expected.rows = knots
    expected.cols = length(row.selection) * length(levels) + 1 + length(levels)

    expect_data_frame(
      prediction,
      nrows = expected.rows,
      ncols = expected.cols)
    expect_true(var %in% colnames(prediction))
  }
)
