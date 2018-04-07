#' Interactive Plots for Interpretable Machine Learning
#' @description
#' The function \code{imlplots()} creates an interactive shiny based dashboard
#' for visualizing the effects of statistical models.
#' The utilization of mlr (Machine Learning in R) is necessary.
#' For more infos go to \url{https://github.com/mlr-org}
#'
#' There are three types of plots: Partial Dependence Plots (PDP), Individual Conditional
#' Expectation (ICE) plots and Accumulated Local Effects (ALE) plots.
#' @param data A data frame of the test data.
#' Has to contain exactly the same variables as the training data.
#' @param task The mlr task the models were being trained on,
#' e.g. iris.task = makeClassifTask(data = iris, target = "Species").
#' Classification and regression tasks are supported.
#' @param models A list of mlr trained models, e.g. list(rf.mod, glm.mod).
#'
#' You can provide differently tuned models of the same learner by assigning
#' a unique ID to the learner, e.g.
#' \code{makeLearner("regr.randomForest", id = "ownId")} \cr
#' 
#' @examples
#' library(mlr)
#' tsk = makeRegrTask(data = boston, target = "medv")
#' mod.rf = train("regr.randomForest", task = tsk)
#' mod.svm = train("regr.svm", task = tsk)
#' imlplots(boston, tsk, list(mod.rf, mod.svm))
#' 
#' @note
#' The plots display combinations of different inputs and outputs/predictions.
#' Therefore they are highly sensitive to the trained and provided models.
#'
#' The variable of interest provides variations of different inputs, while all other
#' variables are held constant. You can look at how the predictions change,
#' if you had provided different test data, by either filtering/subsetting
#' the data or manually setting a variable to a fixed value for all observations.
#' 
#' The function performs a basic check upon initialization,
#' whether the provided models can be used to properly predict.
#' If the check fails, it is recommended to manually test the model with the 
#' \code{marginalPrediction()} function of the mmpf package.
#' 
#' @author Julia Fried, Tobias Riebe, Christian Scholbeck; in cooperation with
#' the working group for computational statistics at
#' Ludwigs-Maximilians-University Munich.
#' 
#' @references
#' 
#' Apley (2016). "Visualizing the Effects of Predictor Variables in Black Box Supervised
#' Learning Models"
#' 
#' Bischl et. al (2016). "mlr: Machine Learning in R." Journal of Machine Learning
#' Research, 17(170), pp.
#' 
#' Friedman, J.H. (2001). “Greedy Function Approximation: A Gradient Boosting
#' Machine.” Annals of Statistics 29: 1189–1232.
#' 
#' Goldstein et al. (2013). "Peeking Inside the Black Box: Visualizing Statistical Learning with Plots of
#' Individual Conditional Expectation"
#' 
#' Jones (2017). "mmpf: Monte-Carlo Methods for Prediction Functions "The R Journal Vol. XX/YY, AAAA 20ZZ

#' @export

imlplots = function(data, task, models) {

  # ~ Preprocessing ~ ----
  # First we extract all relevant features from the input
  # We get all models from the list of models
  if (!(is.vector(models))) {models = list(models)}

  learner.models = lapply(models, function(x) x[["learner.model"]])
  learner.models.names = lapply(models, function(x) x[["learner"]][["id"]])

  # Next target and type are extracted from the task description
  target = getTaskDesc(task)$target
  type = getTaskDesc(task)$type

  # We support numeric and factor variables
  # Numeric and factor variables will be handled different (e.g. for filtering)
  features = names(data)[!names(data) %in% target]
  features.numeric = features[sapply(data[!names(data) %in% target], is.numeric)]
  features.factor = features[sapply(data[!names(data) %in% target], is.factor)]

  do.call(modelCheck,
    list(data = data, models = models, var = sample(features, 1))
  )
  # basic check whether provided models throw error when using
  # marginalPrediction(...)

  # ~ User Interfache ~ ----
  app.ui = dashboardPage(
    shinyjs::useShinyjs(),
    header = dashboardHeader(
      title = "LMU Data Science Innovationslabor",
      titleWidth = 350,
      tags$li(
        class = "dropdown",
        actionButton(
          "reload", "Reload application",
          width = "100%",
          icon("refresh"),
          style = "font-size: 16px; color: #fff; background-color: #337ab7; border-color: #2e6da4; padding: 13px"))),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      h2("Interactive plots for interpretable machine learning"),
      fluidRow(
        tabBox(
          width = 12,
          height = NULL,
          tabPanel(title = "Data", {
            # data table with filters
            fluidRow(
              column(
                width = 12,
                box(
                  width = NULL,
                  status = "primary",
                  title = "Data used for plotting ICE curves",
                  selectInput("select_lines",
                    "Select observations to sample from",
                    choices = c(
                      "Plot all sampled observations",
                      "Plot individual observations"
                    )
                  ),
                  div(style = "overflow-x: scroll",
                    DT::dataTableOutput("table")
                  )
                )
              )
            )
          }),
          tabPanel(
            # plot settings and preview
            title = "Settings", {
              fluidRow(
                column(
                  width = 3,
                  # left column with plot settings
                  box(
                    title = "Plot settings",
                    width = NULL,
                    status = "primary",
                    selectInput("gfx_package", "Select graphics package",
                      choices = c("ggplot2",
                        "plotly (resource intensive)"
                      )
                    ),
                    selectInput("models", "Choose predictive model",
                      choices = learner.models.names,
                      selected = 1, multiple = FALSE),
                    selectInput(
                      "plot_type",
                      "Choose plot type",
                      choices = c(
                        "Individual Conditional Expectation",
                        "Partial Dependence",
                        "Accumulated Local Effects"),
                      selected = 1, multiple = FALSE),
                    conditionalPanel(
                      condition =
                        "input.plot_type == 'Individual Conditional Expectation'",
                      selectInput(
                        "iceplot_mode", "Ice plot mode",
                        choices = c("Regular",
                          "Centered"
                        ),
                        selected = "Regular"
                      )
                    ),
                    conditionalPanel(
                      condition =
                        "input.plot_type == 'Individual Conditional Expectation'
                        & input.iceplot_mode == 'Centered'",
                      uiOutput("iceplot_center")
                    ),
                    conditionalPanel(
                      condition =
                        "input.plot_type == 'Accumulated Local Effects'",
                      selectInput(
                        "aleplot_mode", "ALE Plot Mode",
                        choices = c("Main Effects",
                                    "Second Order Effects"
                        ),
                        selected = "ALE Main Effects"
                      )
                    ),
                    selectInput(
                      "var", "Variable of interest",
                      choices = features,
                      selected = NULL, multiple = FALSE),
                    conditionalPanel(
                      condition =
                        "input.plot_type == 'Accumulated Local Effects' & input.aleplot_mode == 'Second Order Effects'",
                      uiOutput("ale_interaction")
                    ),
                    uiOutput("checkbox"), # checkbox for adjusting the data
                    uiOutput("knots"),
                    uiOutput("lines")
                  ),
                  box(
                    title = "Adjust feature values",
                    width = NULL,
                    uiOutput("sliders"),
                    uiOutput("selectors")
                  )
                ),
                column(
                  width = 9,
                  # right column with plot preview
                  fluidRow(
                    style = "position:fixed; width:70%;",
                    uiOutput("plot")
                  )
                )
              )}
          ),
          tabPanel(
            title = "Plots",
            tabsetPanel(
              tabPanel(
                title = "Zoomed Plot",
                fluidRow(
                  column(
                    width = 12,
                    box(
                      width = NULL,
                      height = "600px",
                      status = "primary",
                      uiOutput("zoomed_plot")
                    )
                  )
                )
              ),
              tabPanel(
                title = "Scatterplot",
                fluidRow(
                  column(
                    width = 12,
                    box(
                      width = NULL,
                      status = "primary",
                      h2("Filtered data"),
                      uiOutput("scatter_filtered"),
                      HTML('<hr style="color: purple;">'),
                      h2("Unfiltered data"),
                      uiOutput("scatter_unfiltered")
                    )
                  )
                )
              )
            )
          ),
          tabPanel(
            title = "Learner Summary",
            fluidRow(
              column(
                width = 12,
                box(
                  width = NULL,
                  status = "primary",
                  verbatimTextOutput("learner_summary")
                )
              )
            )
          )
        )
      )
    )
  )


  # ~ Server ~ ----
  app.server = function(input, output, session) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #                                                                          #
    #                      Extract model relevant features                     #
    #                                                                          #
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

    observeEvent(input$reload, {
      session$reload()
    })

    # Reactive values for current data frame, available features and prediction
    df = reactiveValues(values_adj = data, features = NULL, pred = NULL, table_rows_selected = NULL)

    observeEvent({
      input$iceplot_mode}
      , {
        selected$iceplot_mode = input$iceplot_mode
        if (input$iceplot_mode == "Centered" && selected$plot == "ice") {
          selected$centered = TRUE
        } else {
          selected$centered = FALSE
        }
      })

    observeEvent(input$gfx_package, {
      if (input$gfx_package == "ggplot2") {
        selected$gfx_package <- "ggplot2"
      } else if (input$gfx_package == "plotly (resource intensive)") {
        selected$gfx_package <- "plotly"
      }
    })

    # Select model for marginal prediction function based on selected string
    # in UI
    observeEvent(input$models, {
      for (i in 1:length(learner.models.names)) {
        model = learner.models[[i]]
        if (input$models == learner.models.names[[i]]) {
          selected$model = model
        } else {}
      }
    })

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #                                                                          #
    #                              Slider options                              #
    #                                                                          #
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

    output$iceplot_center = renderUI({
      knot_values <- df$pred[, 1, with = FALSE]
      selectInput(
        "iceplot_centerpoint",
        "Select horizontal axis value to center ICE curves around (depends on knots)",
        choices = knot_values, selected = selected$iceplot_centerpoint
      )
    })

    output$ale_interaction = renderUI({
      var_options = features[!features %in% c(selected$var)]
      selectInput("ale_interaction_var",
                  "ALE interaction variable",
                  choices = var_options)
    })

    observeEvent({
      input$ale_interaction_var
      selected$plot
      input$aleplot_mode
      selected$gfx_package}
      , ignoreNULL = FALSE, {

      if (selected$plot == "ale" &&
          input$aleplot_mode == "Second Order Effects" &&
          selected$gfx_package == "ggplot2") {

          selected$ale_interact = input$ale_interaction_var

      } else if (selected$plot == "ale" &&
                 input$aleplot_mode == "Second Order Effects" &&
                 selected$gfx_package == "plotly") {

        selected$ale_interact = input$ale_interaction_var

      } else {
        selected$ale_interact = NULL
      }
    })

    observeEvent(input$var, {
      df$values_adj = data
    })

    # Adjusts internal variable for plot type based on selected string in UI
    observeEvent({
      selected$type}
      , {
        if (selected$type == "Individual Conditional Expectation") {
          selected$plot = "ice"
        } else if (selected$type == "Partial Dependence") {
          selected$plot = "pdp"
        } else if (selected$type == "Accumulated Local Effects") {
          selected$plot = "ale"
          shinyjs::disable("lines")
        }
      })

    # Differentiates checked input features into numeric and factor features
    observeEvent({
      input$checks}, ignoreNULL = FALSE
      , {
        numerics = c()
        factors = c()
        if (is.null(input$checks)) {
          selected$features.numeric = NULL
          selected$features.factor = NULL
        } else {
          for (elem in input$checks) {
            if (elem %in% features.numeric) {
              numerics = c(numerics, elem)
            } else  if (elem %in% features.factor) {
              factors = c(factors, elem)
            }
          }
          selected$features.numeric = numerics
          selected$features.factor = factors
        }
      })

    # Reactive values for plot settings
    selected = reactiveValues(knots = 30, lines = 30)

    # Transfers input plot settings from UI to reactive values
    observeEvent({
      input$var
      input$knots
      input$lines
      input$modifdata
      input$plot_type
      input$aleplot_mode}
      , {
        selected$var = input$var
        selected$knots = input$knots
        selected$lines = input$lines
        selected$modifdata = input$modifdata
        selected$type = input$plot_type
        selected$aleplot_mode = input$aleplot_mode
      })

    # captures values of fixed slider for plotting the vertical line

    # adjusts available features for modification based on the selected variable
    # of interest
    observeEvent({
      selected$var
      selected$ale_interact}
      , {
      df$features = names(data)[!names(data) %in% c(target, selected$var,
                                                    selected$ale_interact)]
    })

    # capture slider and selector values
    # set reactive values to NULL if input contains NA or NULL values
    # this sometimes happens when the input value is evaluated before the UI has
    # finished rendering
    observeEvent({
      feature.sliders()
      feature.selectors()}
      , {
        if ((TRUE %in% lapply(feature.sliders(), is.null)) ||
            (TRUE %in% lapply(feature.sliders(), is.na))) {
          selected$values.numeric = NULL
        } else {
          selected$values.numeric = unlist(feature.sliders(),
            function(x) return(as.numeric(x)))
        }
        if ((TRUE %in% lapply(feature.selectors(), is.null)) ||
            (TRUE %in% lapply(feature.selectors(), is.na))) {
          selected$values.factor = NULL
        } else {
          selected$values.factor = unlist(feature.selectors(),
            function(x) return(as.numeric(x)))
        }
      })

    # update numeric feature data when user adjusts sliders
    observeEvent(ignoreNULL = FALSE, {
      selected$values.numeric
      selected$features.numeric}
      , {
        if (!(is.null(selected$values.numeric))) {
          for (i in 1:length(selected$features.numeric)) {
            numeric.feature = selected$features.numeric[i]
            numeric.value = selected$values.numeric[i]
            df$values_adj[numeric.feature] = numeric.value
          }
        }
        unselected.features = df$features[!df$features %in% c(selected$features)]
        df$values_adj[unselected.features] = data[unselected.features]
      })

    # update factor feature data when user adjusts selectors
    observeEvent(ignoreNULL = FALSE, {
      selected$values.factor
      selected$features.factor}
      , {
        if (!(is.null(selected$features.factor))) {
          for (j in 1:length(selected$features.factor)) {
            factor.feature = selected$features.factor[j]
            factor.value = selected$values.factor[j]
            factor.levels = levels(data[[factor.feature]])
            df$values_adj[factor.feature] = factor(x = factor.value,
              levels = factor.levels)
          }
        }
        unselected.features = df$features[!df$features %in% c(selected$features)]
        df$values_adj[unselected.features] = data[unselected.features]
      })

    # Checkbox for adjustable features
    output$checkbox = renderUI({
      checkboxGroupInput("checks", "Select adjustable features",
        choices = df$features)
    })

    # Rendering feature sliders
    observeEvent({
      selected$features.numeric}
      , {
        output$sliders = renderUI({
          if (is.null(selected$features.numeric)) {
          } else {
            sliders = lapply(1:length(selected$features.numeric), function(i) {
              input.name = selected$features.numeric[i]
              min = min(data[[input.name]])
              max = max(data[[input.name]])
              mean = mean(data[[input.name]])
              sliderInput(input.name, input.name, min = min, max = max,
                value = mean, step = NULL)})

            do.call(tagList, sliders)
          }
        })
      })

    # Transfers checked features from UI to reactive values
    observeEvent(input$checks, ignoreNULL = FALSE, {
      selected$features = input$checks
    })

    # Render feature selectors
    observeEvent({
      selected$features.factor}
      , {
        output$selectors = renderUI({
          if (is.null(selected$features.factor)) {
          } else {
            selectors = lapply(1:length(selected$features.factor), function(i) {
              input.name = selected$features.factor[i]
              factor.levels = levels(data[[input.name]])
              selectInput(input.name, input.name, choices = factor.levels)})

            do.call(tagList, selectors)
          }
        })
      })


    # list of strings with input$[feature] for numeric features
    slider.list = reactive({
      lapply(selected$features.numeric, FUN = function(feature) {
        text = paste("input$", feature, sep = "")})
    })


    # list of strings with input$[feature] for factor features
    selector.list = reactive({
      lapply(selected$features.factor, FUN = function(feature) {
        text = paste("input$", feature, sep = "")})
    })

    # make reactive list of slider input expressions by parsing strings from
    # slider.list
    # use this function to capture inputs from feature sliders
    feature.sliders = reactive({
      lapply(slider.list(), FUN = function(x) eval(parse(text = x)))
    })

    feature.selectors = reactive({
      lapply(selector.list(), FUN = function(x) eval(parse(text = x)))
    })

    # >> Data ----
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #                                                                          #
    #                               Data filters                               #
    #                                                                          #
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

    output$knots <- renderUI({
      if (is.null(selected$knots)) {
        selected$knots = 30
      }
      # setting to 30 upon initialization; setting init value in selected
      # does not work
      if (selected$plot == "ale") {
        sliderInput(
          "knots",
          "Number of intervals into which the predictor range is divided",
          min = 1,
          max = 500,
          value = selected$knots,
          step = 1)
      } else {
      sliderInput(
        "knots",
        "Number of knots for each line",
        min = 1,
        max = nrow(df$values_filtered),
        value = selected$knots,
        step = 1)
      }
    })

    output$lines <- renderUI({
      if (is.null(selected$lines)) {
        selected$lines = 30
      }
      # setting to 30 upon initialization; setting init value in selected
      # does not work

      sliderInput(
        "lines",
        "Number of individual observations (lines) to sample from data",
        min = 1,
        max = nrow(df$values_filtered),
        value = selected$lines,
        step = 1)
    })

    observeEvent({
      df$table_rows_filtered
      df$values_adj}
      , ignoreNULL = FALSE, ignoreInit = FALSE, {
        df$values_filtered <- df$values_adj[df$table_rows_filtered, ]

      })

    proxy <- DT::dataTableProxy("table")

    observeEvent({
      input$select_lines
      input$table_rows_all
      input$table_rows_selected}
      , ignoreNULL = FALSE, {
        if (input$select_lines == "Plot all sampled observations") {

          df$selection_mode <- list(mode = "none")

          df$table_rows_filtered <- input$table_rows_all
          df$table_rows_selected <- NULL
          # automatically use all filtered observations

        } else if (input$select_lines == "Plot individual observations") {

          shinyjs::disable("lines")

          df$selection_mode <- list(mode = "multiple")

          df$table_rows_filtered <- input$table_rows_all
          df$table_rows_selected <- input$table_rows_selected
        }
      })

    observeEvent(input$table_rows_all, {
      DT::selectRows(proxy, list())
    })

    # renders the current reactive data frame
    output$table = DT::renderDataTable({
      shiny::req(!is.null(df$selection_mode))
      DT::datatable(
        df$values_adj,
        filter = list(position = "top", clear = TRUE, plain = TRUE),
        selection = df$selection_mode
      )},
      server = TRUE
      # options = list(
      #   stateSave = TRUE,
      #   # columnDefs = list(list(width = '200px', targets = "_all")),
      #   autoWidth = TRUE,
      #   scrollX = TRUE
      # )
    )

    observeEvent(input$iceplot_centerpoint, {
      selected$iceplot_centerpoint <- as.numeric(input$iceplot_centerpoint)
    })

    # >> Plot ----
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    #                                                                          #
    #                             Create the plot                              #
    #                                                                          #
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

    observeEvent({
      df$values_filtered
      df$table_rows_selected
      input$select_lines
      input$iceplot_centerpoint
      selected$iceplot_mode
      selected$model
      selected$plot
      selected$var
      selected$ale_interact
      selected$knots
      selected$lines
    }
      , {
        req(!(TRUE %in% apply(df$values_filtered, MARGIN = 2,
                              function(column) {NA %in% column})))
        req(nrow(df$values_filtered) > 0)

        if (selected$plot == "ale") {
          # use ALEPlot::ALEPlot(..) to predict for ale
          shiny::withProgress(
            message = "Calculating Predictions..",
            detail = "Please wait.",
            min = 0, max = 100, value = 100,
            {
              df$pred = makePredictionsAle(
                data = df$values_filtered,
                target = target,
                model = selected$model,
                var1 = selected$var,
                var2 = selected$ale_interact,
                knots = selected$knots
              )
            }
          )
        } else {
          # use mmpf::marginalPrediction(..) to predict for ice/pdp
          req(nrow(df$values_filtered) >= selected$knots)
          req(nrow(df$values_filtered) >= selected$lines)

          if (input$select_lines == "Plot all sampled observations") {
            # Sample selected$lines from df$values_filtered
            shiny::withProgress(
              message = "Calculating predictions..",
              detail = "Please wait.",
              min = 0, max = 100, value = 100,
              {
                prediction <- makePredictionsIceSampled(
                  data = df$values_filtered,
                  var = selected$var,
                  model = selected$model,
                  knots = selected$knots,
                  lines = selected$lines,
                  type = type
                )
              }
            )
            if (selected$iceplot_mode == "Centered") {
              # req(input$iceplot_centerpoint %in% prediction[, 1, with = FALSE])
              req(!is.null(selected$iceplot_centerpoint))
              req(selected$var %in% names(prediction))

              shiny::withProgress(
                message = "Centering predictions..",
                detail = "Please wait.",
                min = 0, max = 100, value = 100,
                {
                  df$pred <- centerPredictions(
                    predictions = prediction,
                    centerpoint = selected$iceplot_centerpoint,
                    var = selected$var
                  )
                }
              )
            } else if (selected$iceplot_mode == "Regular") {
              df$pred <- prediction
            }
          } else if (input$select_lines == "Plot individual observations") {
            # mmpf::marginalPrediction(...) marginalizes only over selected
            # observations
            shinyjs::disable("lines")

            req(!is.null(df$table_rows_selected))
            req(df$table_rows_selected %in% as.numeric(row.names(df$values_filtered)))

            prediction <- makePredictionsIceSelected(
              data = df$values_filtered,
              var = selected$var,
              model = selected$model,
              knots = selected$knots,
              selected_rows = df$table_rows_selected,
              type = type
            )
            if (selected$iceplot_mode == "Centered") {
              req(!is.null(selected$iceplot_centerpoint))
              req(selected$var %in% names(prediction))

              shiny::withProgress(
                message = "Centering predictions..",
                detail = "Please wait.",
                min = 0, max = 100, value = 100,
                {
                  df$pred <- centerPredictions(
                    predictions = prediction,
                    centerpoint = selected$iceplot_centerpoint,
                    var = selected$var
                  )
                }
              )
            } else if (selected$iceplot_mode == "Regular") {
              df$pred <- prediction
            }
          }
        }
      })

    output$scatter_unfiltered_basic = renderPlot({
      scatterPlotUnfiltered()},
      width = 800,
      height = 400
    )
    output$scatter_unfiltered_plotly = renderPlotly({
      p <- plotly_build(scatterPlotUnfiltered())
      p$elementId <- NULL
      p$x$layout$width <- 800
      p$x$layout$height <- 400
      p$width <- NULL
      p$height <- NULL
      return(p)
    })

    output$scatter_filtered_basic = renderPlot({
      scatterPlotFiltered()},
      width = 800,
      height = 400
    )
    output$scatter_filtered_plotly = renderPlotly({
      p <- plotly_build(scatterPlotFiltered())
      p$elementId <- NULL
      p$x$layout$width <- 800
      p$x$layout$height <- 400
      p$width <- NULL
      p$height <- NULL
      return(p)
    })

    output$scatter_unfiltered <- renderUI({
      if (selected$gfx_package == "plotly") {
        plotlyOutput("scatter_unfiltered_plotly")
      } else if (selected$gfx_package == "ggplot2") {
        plotOutput("scatter_unfiltered_basic")
      }
    })
    output$scatter_filtered <- renderUI({
      if (selected$gfx_package == "plotly") {
        plotlyOutput("scatter_filtered_plotly")
      } else if (selected$gfx_package == "ggplot2") {
        plotOutput("scatter_filtered_basic")
      }
    })

    scatterPlotUnfiltered <- eventReactive(ignoreNULL = FALSE, {
      df$table_rows_selected
      selected$plot
      selected$aleplot_mode
      selected$gfx_package
      selected$var
      selected$ale_interact}
      , {
        if (selected$plot == "ale" &&
            selected$aleplot_mode == "Second Order Effects" &&
            selected$gfx_package == "plotly") {

          scatterPlot3D(
            data = data, target = target,
            var = c(selected$var, selected$ale_interact),
            highlighted = df$table_rows_selected
          )
        } else {

          scatterPlot(
            data = data, target = target,
            var = selected$var,
            highlighted = df$table_rows_selected
          )

        }
      })

    scatterPlotFiltered <- eventReactive(ignoreNULL = FALSE, {
      df$values_filtered
      df$table_rows_selected
      selected$plot
      selected$aleplot_mode
      selected$gfx_package
      selected$var
      selected$ale_interact}
      , {
        if (selected$plot == "ale" &&
            selected$aleplot_mode == "Second Order Effects" &&
            selected$gfx_package == "plotly") {
          scatterPlot3D(
            data = df$values_filtered, target = target,
            var = c(selected$var, selected$ale_interact),
            highlighted = df$table_rows_selected
          )
        } else {
          scatterPlot(
            data = df$values_filtered, target = target,
            var = selected$var,
            highlighted = df$table_rows_selected
          )
        }
      })

    output$iml_plotly_plot <- renderPlotly({
      p <- plotly_build(imlPlot())
      p$elementId <- NULL
      p$x$layout$width <- 800
      p$x$layout$height <- 400
      p$width <- NULL
      p$height <- NULL
      return(p)
    })

    output$iml_basic_plot <- renderPlot({
      imlPlot()},
      width = 800,
      height = 400
    )

    output$plot <- renderUI({
      if (selected$gfx_package == "plotly") {
        plotlyOutput("iml_plotly_plot")
      } else if (selected$gfx_package == "ggplot2") {
        plotOutput("iml_basic_plot")
      }
    })

    imlPlot = eventReactive(ignoreInit = FALSE, ignoreNULL = FALSE, {
      # plots the predicted values by calling predefined plot functions with
      # current reactive values
      df$pred
      df$values_filtered
      df$table_rows_selected
      df$pred_ale
      input$select_lines
      selected$plot
      selected$iceplot_centerpoint
      selected$gfx_package
      selected$var
      selected$ale_interact
      selected$knots
      selected$lines}
      , {
        req(selected$lines)
        req(!is.null(df$pred))
        req(!(TRUE %in% apply(df$pred, MARGIN = 2,
          function(column) {NA %in% column})))
        req(selected$var %in% names(df$pred))

        withProgress(
          message = "Rendering plot..",
          detail = "Please wait.",
          min = 0, max = 100, value = 100,
          {
            if (nrow(df$values_filtered) == 0) {
              plot <- placeholderPlot()
              return(plot)
            } else if (
              (input$select_lines == "Plot individual observations") &
                (is.null(df$table_rows_selected))
            ) {
              plot <- placeholderPlot()
              return(plot)
            } else {
              if (type == "regr") {
                if (selected$plot == "ice") {
                  plot <- regrIcePlot(
                    pred = df$pred,
                    var = selected$var,
                    target  = target,
                    knots = selected$knots,
                    lines = selected$lines,
                    centered = selected$centered,
                    centerpoint = selected$iceplot_centerpoint
                  )
                  return(plot)
                } else if (selected$plot == "pdp") {
                  plot <- regrPartialDependencePlot(
                    pred = df$pred,
                    var = selected$var,
                    target = target,
                    knots = selected$knots
                  )
                  return(plot)
                } else if (selected$plot == "ale") {

                  plot <- regrAlePlot(
                    data = df$pred,
                    target = target,
                    var1 = selected$var,
                    var2 = selected$ale_interact,
                    knots = selected$knots,
                    gfx_package = selected$gfx_package
                  )
                  return(plot)
                }
              } else if (type == "classif") {
                if (selected$plot == "ice") {
                  plot <- classifIcePlot(
                    pred = df$pred,
                    var = selected$var,
                    knots = selected$knots,
                    lines = selected$lines,
                    centered = selected$centered,
                    centerpoint = selected$iceplot_centerpoint
                  )
                  return(plot)
                } else if (selected$plot == "pdp") {

                  plot <- classifPartialDependencePlot(
                    pred = df$pred,
                    var = selected$var,
                    target = target,
                    knots = selected$knots
                  )
                  return(plot)
                } else if (selected$plot == "ale") {
                  
                  plot <- classifAlePlot(
                    data = df$pred,
                    target = target,
                    var1 = selected$var,
                    var2 = selected$ale_interact,
                    knots = selected$knots,
                    gfx_package = selected$gfx_package
                  )
                  return(plot)
                } 
              } 
            } 
          } 
        ) # ending: withProgress(...)
      }) 
    output$zoomed_plotly_plot = renderPlotly({
      p <- plotly_build(imlPlot())
      p$elementId <- NULL
      p$x$layout$width <- 1200
      p$x$layout$height <- 600
      p$width <- NULL
      p$height <- NULL
      return(p)
    })
    output$zoomed_basic_plot = renderPlot({
      imlPlot()},
      width = 1200,
      height = 600
    )

    output$zoomed_plot <- renderUI({
      if (selected$gfx_package == "plotly") {
        plotlyOutput("zoomed_plotly_plot")
      } else if (selected$gfx_package == "ggplot2") {
        plotOutput("zoomed_basic_plot")
      }
    })
    output$learner_summary = renderPrint({
      capture.output(selected$model)
    })

  } # end server

  shinyApp(ui = app.ui, server = app.server)
}
