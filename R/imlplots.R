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
#' e.g. \code{iris.task = makeClassifTask(data = iris, target = "Species")}.
#' Classification and regression tasks are supported.
#' @param models A list of mlr trained models, e.g. \code{list(rf.mod, glm.mod)}. \cr
#' You can provide differently tuned models of the same learner by assigning
#' a unique ID to the learner, e.g.
#' \code{makeLearner("regr.randomForest", id = "ownId")}
#'
#' @param model.check A string. A model check is performed upon initialization,
#' whether the provided models can be used to properly predict. \cr
#' 'all.features' iteratively checks all model/feature combinations. 'sample.feature' randomly
#' selects a single feature from the feature space and checks all models with
#' it.
#'
#' @examples
#' tsk = makeRegrTask(data = boston, target = "medv")
#' mod.rf = train("regr.randomForest", task = tsk)
#' mod.glm = train("regr.glm", task = tsk)
#' imlplots(boston, tsk, list(mod.rf, mod.glm))
#'
#' @note

#' The plots display combinations of different inputs and outputs/ predictions.
#' Therefore they are highly sensitive to the trained and provided models.
#'
#' The variable of interest provides variations of different inputs, while all other
#' variables are held constant. You can look at how the predictions change,
#' if you had provided different test data, by either filtering/ subsetting
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
#' Friedman, J.H. (2001). "Greedy Function Approximation: A Gradient Boosting
#' Machine." Annals of Statistics 29: 1189 - 1232.
#'
#' Goldstein et al. (2013). "Peeking Inside the Black Box: Visualizing Statistical Learning with Plots of
#' Individual Conditional Expectation"
#'
#' Jones (2017). "mmpf: Monte-Carlo Methods for Prediction Functions "The R Journal Vol. XX/YY, AAAA 20ZZ
#' @export

imlplots = function(data, task, models, model.check = "all.features") {

  if (!(is.vector(models))) {models = list(models)}
  assertDataFrame(data)
  assertClass(task, classes = "Task")
  lapply(models, FUN = function(elem) assertClass(elem, class = "WrappedModel"))

  learner.models = lapply(models, function(x) x[["learner.model"]])
  learner.models.names = lapply(models, function(x) x[["learner"]][["id"]])

  target = getTaskDesc(task)$target
  task.type = getTaskDesc(task)$type

  features = names(data)[!names(data) %in% target]
  features.numeric = features[sapply(data[!names(data) %in% target], is.numeric)]
  features.factor = features[sapply(data[!names(data) %in% target], is.factor)]

  do.call(
    modelCheck,
    list(data = data, models = models, features = features, model.check))
  # basic check whether provided models throw error when using
  # marginalPrediction(...)

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
          style = "font-size: 16px; color: #fff; background-color: #337ab7;
                  border-color: #2e6da4; padding: 13px"))),
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
                  selectInput("data_selection_mode",
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
            title = "Settings",
              fluidRow(
                column(
                  width = 3,
                  # left column with plot settings
                  box(
                    title = "Plot settings",
                    width = NULL,
                    status = "primary",
                    selectInput("gfx.package", "Select graphics package",
                      choices = c("ggplot2",
                        "plotly (resource intensive)")
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
                        "input.plot_type ==
                        'Individual Conditional Expectation'",
                      selectInput(
                        "iceplot_mode", "Ice plot mode",
                        choices = c("Regular",
                          "Centered"),
                        selected = "Regular")
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
                                    "Second Order Effects"),
                        selected = "ALE Main Effects")
                    ),
                    selectInput(
                      "var", "Variable of interest",
                      choices = features,
                      selected = NULL, multiple = FALSE),
                    conditionalPanel(
                      condition =
                        "input.plot_type == 'Accumulated Local Effects' &
                        input.aleplot_mode == 'Second Order Effects'",
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
              )
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
  app.server = function(input, output, session) {

    # --------------------------------------------------------------------------
    # reactive values

    df = reactiveValues(
      # reactive values for current (adjusted) data frame, available features
      # and prediction values
      values.adj = data, features = NULL, pred = NULL,
      table.rows.selected = NULL)

    selected = reactiveValues(
      # reactive values only for selected values
      knots = 30, lines = 30)

    # --------------------------------------------------------------------------
    # ui outputs

    output$iceplot_center = renderUI({
      knot.values = df$pred[, 1]
      # sampled values appearing on the horizontal axis in ICE and PDP
      selectInput(
        "iceplot_center_x",
        "Select horizontal axis value to center ICE curves around
        (depends on knots)",
        choices = knot.values, selected = selected$iceplot.center.x)
    })

    output$ale_interaction = renderUI({
      variable.options = features[!features %in% c(selected$var)]
      selectInput("ale_interaction_var",
                  "ALE interaction variable",
                  choices = variable.options)
    })

    output$checkbox = renderUI({
      # checkbox for adjustable features
      checkboxGroupInput("checks", "Select adjustable features",
                         choices = df$features.unused)
    })

    sliderList = reactive({
      # list of strings with input$[feature] for numeric features
      lapply(selected$features.numeric, FUN = function(feature) {
        text = paste("input$", feature, sep = "")})
    })

    selectorList = reactive({
      # list of strings with input$[feature] for factor features
      lapply(selected$features.factor, FUN = function(feature) {
        text = paste("input$", feature, sep = "")})
    })

    featureSliders = reactive({
      # make reactive list of slider input expressions (for numeric variables)
      # by parsing strings from slider.list
      # use this function to capture inputs from feature sliders
      lapply(sliderList(), FUN = function(x) eval(parse(text = x)))
    })

    featureSelectors = reactive({
      # make reactive list of selector input expressions (for factor variables)
      # by parsing strings from selector.list
      # use this function to capture inputs from feature selectors
      lapply(selectorList(), FUN = function(x) eval(parse(text = x)))
    })

    output$knots = renderUI({
      if (is.null(selected$knots)) {
        selected$knots = 30
      }
      # setting to 30 upon initialization; setting init value in selected
      # does not work
      if (selected$plot.type == "ale") {
        sliderInput(
          "knots",
          "Number of intervals into which the predictor range is divided",
          min = 1,
          max = 100,
          value = selected$knots,
          step = 1)
      } else {
        sliderInput(
          "knots",
          "Number of knots for each line",
          min = 1,
          max = nrow(df$values.filtered),
          value = selected$knots,
          step = 1)
      }
    })

    output$lines = renderUI({
      if (is.null(selected$lines)) {
        selected$lines = 30
      }
      # setting to 30 upon initialization; setting init value in selected
      # does not work

      sliderInput(
        "lines",
        "Number of individual observations (lines) to sample from data",
        min = 1,
        max = nrow(df$values.filtered),
        value = selected$lines,
        step = 1)
    })

     output$learner_summary = renderPrint({
      capture.output(selected$model)
    })

     output$table = DT::renderDataTable({
       shiny::req(!is.null(selected$datatable.select))
       DT::datatable(
         df$values.adj,
         filter = list(position = "top", clear = TRUE, plain = TRUE),
         selection = selected$datatable.select
       )},
       server = TRUE
     )

    # --------------------------------------------------------------------------
    # plot outputs and event reactive functions

     output$scatter_unfiltered_basic = renderPlot({
       # render ggplot2 object of unfiltered scatter plot
       scatterPlotUnfiltered()},
       width = 800,
       height = 400
     )

     output$scatter_unfiltered_plotly = renderPlotly({
       # render plotly object of unfiltered scatter plot
       p = plotly_build(scatterPlotUnfiltered())
       p$elementId = NULL
       p$x$layout$width = 800
       p$x$layout$height = 400
       p$width = NULL
       p$height = NULL
       return(p)
     })
     # both ggplot2 and plotly versions are ready to be rendered upon
     # changing the UI selections; lazy loading keeps one version from not
     # being rendered

     output$scatter_filtered_basic = renderPlot({
       # render ggplot2 object of filtered scatter plot
       scatterPlotFiltered()},
       width = 800,
       height = 400
     )
     output$scatter_filtered_plotly = renderPlotly({
       # render plotly object of filtered scatter plot
       p = plotly_build(scatterPlotFiltered())
       p$elementId = NULL
       p$x$layout$width = 800
       p$x$layout$height = 400
       p$width = NULL
       p$height = NULL
       return(p)
     })
     # both ggplot2 and plotly versions are ready to be rendered upon
     # changing the UI selections; lazy loading keeps one version from not
     # being rendered

     output$scatter_unfiltered = renderUI({
       # decide which rendered unfiltered scatter plot to display in UI based on
       # selected$gfx.package
       if (selected$gfx.package == "plotly") {
         plotlyOutput("scatter_unfiltered_plotly")
       } else if (selected$gfx.package == "ggplot2") {
         plotOutput("scatter_unfiltered_basic")
       }
     })

     output$scatter_filtered = renderUI({
       # decide which rendered filtered scatter plot to display in UI based on
       # selected$gfx.package
       if (selected$gfx.package == "plotly") {
         plotlyOutput("scatter_filtered_plotly")
       } else if (selected$gfx.package == "ggplot2") {
         plotOutput("scatter_filtered_basic")
       }
     })

     scatterPlotUnfiltered = eventReactive({
       # plot function for unfiltered scatter plot
       selected$table.rows
       selected$plot.type
       selected$aleplot.mode
       selected$gfx.package
       selected$var
       selected$ale.interaction},
       ignoreNULL = FALSE,
       {
         if (selected$plot.type == "ale" &&
             selected$aleplot.mode == "Second Order Effects" &&
             selected$gfx.package == "plotly") {
           scatterPlot3D(
             data = data, target = target,
             var = c(selected$var, selected$ale.interaction),
             highlighted = selected$table.rows)
         } else {
           scatterPlot(
             data = data, target = target,
             var = selected$var,
             highlighted = selected$table.rows)
         }
       }
     )

     scatterPlotFiltered = eventReactive({
       # plot function for filtered scatter plot
       df$values.filtered
       selected$table.rows
       selected$plot.type
       selected$aleplot.mode
       selected$gfx.package
       selected$var
       selected$ale.interaction},
       ignoreNULL = FALSE,
       {
         if (selected$plot.type == "ale" &&
             selected$aleplot.mode == "Second Order Effects" &&
             selected$gfx.package == "plotly") {
           scatterPlot3D(
             data = df$values.filtered, target = target,
             var = c(selected$var, selected$ale.interaction),
             highlighted = selected$table.rows)
         } else {
           scatterPlot(
             data = df$values.filtered, target = target,
             var = selected$var,
             highlighted = selected$table.rows)
         }
       })

     output$iml_plotly_plot = renderPlotly({
       # rendering plotly version of imlplot output
       p = plotly_build(imlPlot())
       p$elementId = NULL
       p$x$layout$width = 800
       p$x$layout$height = 400
       p$width = NULL
       p$height = NULL
       return(p)
     })

     output$iml_basic_plot = renderPlot({
       # rendering ggplot2 version of imlplot output
       imlPlot()},
       width = 800,
       height = 400
     )

     output$plot = renderUI({
       # decide which plot to display in UI based on selected$gfx.package
       if (selected$gfx.package == "plotly") {
         plotlyOutput("iml_plotly_plot")
       } else if (selected$gfx.package == "ggplot2") {
         plotOutput("iml_basic_plot")
       }
     })

     output$zoomed_plotly_plot = renderPlotly({
       # render plotly version of zoomed imlplot
       p = plotly_build(imlPlot())
       p$elementId = NULL
       p$x$layout$width = 1200
       p$x$layout$height = 600
       p$width = NULL
       p$height = NULL
       return(p)
     })

     output$zoomed_basic_plot = renderPlot({
       # render ggplot2 version of zoomed imlplot
       imlPlot()},
       width = 1200,
       height = 600
     )

     output$zoomed_plot = renderUI({
       # decide which zoomed plot version to display in UI based on
       # selected$gfx.package
       if (selected$gfx.package == "plotly") {
         plotlyOutput("zoomed_plotly_plot")
       } else if (selected$gfx.package == "ggplot2") {
         plotOutput("zoomed_basic_plot")
       }
     })

     imlPlot = eventReactive({
       # plots the predicted values by calling predefined plot functions with
       # current reactive values
       df$pred
       df$values.filtered
       selected$table.rows
       selected$data.selection.mode
       selected$plot.type
       selected$iceplot.center.x
       selected$gfx.package
       selected$var
       selected$ale.interaction
       selected$knots
       selected$lines},
       ignoreInit = FALSE,
       ignoreNULL = FALSE,
       {
         shiny::req(!is.null(df$pred))
         shiny::req((selected$var %in% names(df$pred)) ||
                      "error" %in% df$pred)
         shiny::req(selected$lines)
         if (!"error" %in% df$pred) {
           shiny::req(
             !(TRUE %in% apply(df$pred, MARGIN = 2,
                               function(column) {NA %in% column})))
         }

         withProgress(
           message = "Rendering plot..",
           detail = "Please wait.",
           min = 0, max = 100, value = 100,
           {
             if (nrow(df$values.filtered) == 0) {
               plot = placeholderPlot()
               return(plot)
             } else if (
               ((selected$data.selection.mode == "individual") &&
               (is.null(selected$table.rows)) &&
               (!selected$plot.type == "ale"))) {
               plot = placeholderPlot()
               return(plot)
             } else {
               if (task.type == "regr") {
                 if (selected$plot.type == "ice") {
                   plot = regrIcePlot(
                     pred = df$pred,
                     var = selected$var,
                     target  = target,
                     knots = selected$knots,
                     lines = selected$lines,
                     centered = selected$centered,
                     center.x = selected$iceplot.center.x)
                   return(plot)
                 } else if (selected$plot.type == "pdp") {
                   plot = regrPartialDependencePlot(
                     pred = df$pred,
                     var = selected$var,
                     target = target,
                     knots = selected$knots)
                   return(plot)
                 } else if (selected$plot.type == "ale") {
                   # if (!is.null(selected$ale.interaction)) {
                   #   shiny::req(selected$ale.interaction %in% colnames(df$pred))
                   # }
                   plot = regrAlePlot(
                     data = df$pred,
                     target = target,
                     var1 = selected$var,
                     var2 = selected$ale.interaction,
                     knots = selected$knots,
                     gfx.package = selected$gfx.package)
                   return(plot)
                 }
               } else if (task.type == "classif") {
                 if (selected$plot.type == "ice") {
                   plot = classifIcePlot(
                     pred = df$pred,
                     var = selected$var,
                     knots = selected$knots,
                     lines = selected$lines,
                     centered = selected$centered,
                     center.x = selected$iceplot.center.x)
                   return(plot)
                 } else if (selected$plot.type == "pdp") {
                   plot = classifPartialDependencePlot(
                     pred = df$pred,
                     var = selected$var,
                     target = target,
                     knots = selected$knots)
                   return(plot)
                 } else if (selected$plot.type == "ale") {
                   plot = classifAlePlot(
                     data = df$pred,
                     target = target,
                     var = selected$var)
                   return(plot)
                 }
               }
             }
           }
         ) # ending: withProgress(...)
       })

    # --------------------------------------------------------------------------
    # Observers

     observeEvent({
       # observer for calculating predictions
       df$values.filtered
       selected$table.rows
       selected$data.selection.mode
       selected$iceplot.center.x
       selected$centered
       selected$model
       selected$plot.type
       selected$var
       selected$ale.interaction
       selected$knots
       selected$lines},
       {
         shiny::req(nrow(df$values.filtered) > 0)
         shiny::req(!(TRUE %in% apply(df$values.filtered, MARGIN = 2,
                               function(column) {NA %in% column})))
         if (selected$plot.type == "ale") {
           # use ALEPlot::ALEPlot(..) to predict for ale
           shiny::withProgress(
             message = "Calculating Predictions..",
             detail = "Please wait.",
             min = 0, max = 100, value = 100,
             {
               df$pred = makePredictionsAle(
                 data = df$values.filtered,
                 target = target,
                 model = selected$model,
                 var1 = selected$var,
                 var2 = selected$ale.interaction,
                 knots = selected$knots,
                 task.type = task.type)
             }
           )
         } else {
           # use mmpf::marginalPrediction(..) to predict for ice/pdp
           shiny::req(nrow(df$values.filtered) >= selected$knots)
           shiny::req(nrow(df$values.filtered) >= selected$lines)

           if (selected$data.selection.mode == "sampling") {
             # Sample selected$lines from df$values.filtered
             shiny::withProgress(
               message = "Calculating predictions..",
               detail = "Please wait.",
               min = 0, max = 100, value = 100,
               {
                 prediction = makePredictionsIceSampled(
                   data = df$values.filtered,
                   var = selected$var,
                   model = selected$model,
                   knots = selected$knots,
                   lines = selected$lines,
                   task.type = task.type)
               }
             )
             if (selected$centered == TRUE) {
               # shiny::req(
               #   selected$iceplot.center.x %in% prediction[, 1, with = FALSE])
               shiny::req(!is.null(selected$iceplot.center.x))
               shiny::req(!is.na(selected$iceplot.center.x))
               shiny::req(selected$var %in% names(prediction))

               shiny::withProgress(
                 message = "Centering predictions..",
                 detail = "Please wait.",
                 min = 0, max = 100, value = 100,
                 {
                   df$pred = centerPredictions(
                     predictions = prediction,
                     center.x = selected$iceplot.center.x,
                     var = selected$var)
                 }
               )
             } else if (selected$centered == FALSE) {
               df$pred = prediction
             }
           } else if (selected$data.selection.mode == "individual") {
             # mmpf::marginalPrediction(...) marginalizes only over selected
             # observations
             shiny::req(!is.null(selected$table.rows))
             shiny::req(selected$table.rows %in% as.numeric(
               row.names(df$values.filtered)))

             prediction = makePredictionsIceSelected(
               data = df$values.filtered,
               var = selected$var,
               model = selected$model,
               knots = selected$knots,
               selected.rows = selected$table.rows,
               task.type = task.type)

             if (selected$centered == TRUE) {
               # shiny::req(
               #   selected$iceplot.center.x %in% prediction[, 1, with = FALSE])
               shiny::req(!is.null(selected$iceplot.center.x))
               shiny::req(!is.na(selected$iceplot.center.x))
               shiny::req(selected$var %in% names(prediction))

               shiny::withProgress(
                 message = "Centering predictions..",
                 detail = "Please wait.",
                 min = 0, max = 100, value = 100,
                 {
                   df$pred = centerPredictions(
                     predictions = prediction,
                     center.x = selected$iceplot.center.x,
                     var = selected$var)
                 }
               )
             } else if (selected$centered == FALSE) {
               df$pred = prediction
             }
           }
         }
       }
     )

     observeEvent({
       # reload button action on top right corner
       input$reload},
       {
         session$reload()
       }
     )

     observeEvent({
       # line sampling not necessary when individual observations are selected
       # or in ale plot mode
       selected$data.selection.mode
       selected$plot.type},
       {
         req(!is.null(selected$data.selection.mode))
         req(!is.null(selected$plot.type))
         if (selected$data.selection.mode == "individual" ||
             selected$plot.type == "ale") {
           shinyjs::disable("lines")
         } else {
           shinyjs::enable("lines")
         }
       }
     )

     observeEvent({
       input$iceplot_mode
       selected$plot.type},
       ignoreNULL = FALSE,
       {
         if (input$iceplot_mode == "Centered" && selected$plot.type == "ice") {
           selected$centered = TRUE
         } else {
           selected$centered = FALSE
         }
       }
     )

    observeEvent({
      input$gfx.package},
      {
        if (input$gfx.package == "ggplot2") {
          selected$gfx.package = "ggplot2"
        } else if (input$gfx.package == "plotly (resource intensive)") {
          selected$gfx.package = "plotly"
        }
      }
    )

    observeEvent({
      selected$plot.type},
      {
        if (task.type == "classif" && selected$plot.type == "ale") {
          updateSelectInput(
            session = session,
            inputId = "aleplot_mode",
            label = "For classification ALE plots only main effects are
                    supported",
            choices = "Main Effects")
        }
        selected$ale.interaction = NULL
    })

    observeEvent({
      # select model for marginal prediction function based on selected string
      # in UI
      input$models},
      {
        for (i in 1:length(learner.models.names)) {
          model = learner.models[[i]]
          if (input$models == learner.models.names[[i]]) {
            selected$model = model
          } else {}
        }
      }
    )

    observeEvent({
      input$ale_interaction_var
      selected$plot.type
      input$aleplot_mode
      selected$gfx.package},
      ignoreNULL = FALSE,
      {
        if (selected$plot.type == "ale" &&
            input$aleplot_mode == "Second Order Effects" &&
            selected$gfx.package == "ggplot2") {

          selected$ale.interaction = input$ale_interaction_var

        } else if (selected$plot.type == "ale" &&
                   input$aleplot_mode == "Second Order Effects" &&
                   selected$gfx.package == "plotly") {

          selected$ale.interaction = input$ale_interaction_var

        } else {
          selected$ale.interaction = NULL
        }
      }
    )

    observeEvent({
      # reset the reactive data values when variable of interest changes
      input$var},
      {
        df$values.adj = data
      }
    )

    observeEvent({
      # differentiates checked input features into numeric and factor features;
      # important, because factor features are adjusted with selectors and need
      # to be provided the right values when adjusting
      input$checks},
      ignoreNULL = FALSE,
      {
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
      }
    )

    observeEvent({
      # set reactive values to UI values
      input$var
      input$knots
      input$lines
      input$plot_type
      input$aleplot_mode
      input$data_selection_mode
      input$checks
      input$iceplot_center_x},
      ignoreNULL = FALSE,
      {
        selected$var = input$var
        selected$knots = input$knots
        selected$lines = input$lines
        selected$aleplot.mode = input$aleplot_mode
        selected$features = input$checks
        selected$iceplot.center.x = input$iceplot_center_x

        if (input$data_selection_mode == "Plot all sampled observations") {
          selected$data.selection.mode = "sampling"
        } else if (input$data_selection_mode ==
                   "Plot individual observations") {
          selected$data.selection.mode = "individual"
        }

        if (input$plot_type == "Individual Conditional Expectation") {
          selected$plot.type = "ice"
        } else if (input$plot_type == "Partial Dependence") {
          selected$plot.type = "pdp"
        } else if (input$plot_type == "Accumulated Local Effects") {
          selected$plot.type = "ale"
        }
      }
    )

    observeEvent({
      selected$data.selection.mode
      input$table_rows_all
      input$table_rows_selected},
      ignoreNULL = FALSE,
      {
        if (selected$data.selection.mode == "sampling") {

          selected$datatable.select = list(mode = "none")
          df$table.rows.filtered = input$table_rows_all
          # use all filtered observations from data table
          selected$table.rows = NULL

        } else if (selected$data.selection.mode == "individual") {
          # only the selected observations in data are marginalized over,
          # so sampling observations/lines is not necessary
          selected$datatable.select = list(mode = "multiple")
          df$table.rows.filtered = input$table_rows_all
          # use all filtered observations from data table
          selected$table.rows = input$table_rows_selected
          # row indicies of selected observations in data table
        }
      }
    )

    observeEvent({
      # available features for adjusting the data cannot contain variable
      # of interest and (if ale selected) ale interaction variable
      selected$var
      selected$ale.interaction},
      ignoreNULL = FALSE,
      {
        df$features.unused = names(data)[!names(data) %in% c(
          target, selected$var,
          selected$ale.interaction)]
      }
    )

    proxy = DT::dataTableProxy("table")

    observeEvent({
      df$table.rows.filtered},
      {
        DT::selectRows(proxy, list())
      }
    )

    observeEvent({
      df$table.rows.filtered
      df$values.adj},
      ignoreNULL = FALSE,
      ignoreInit = FALSE,
      {
        df$values.filtered = df$values.adj[df$table.rows.filtered, ]
      }
    )

    observeEvent({
      # capture slider and selector values
      # set reactive values to NULL if input contains NA or NULL values
      # this sometimes happens when the input value is evaluated before the UI
      # has finished rendering
      featureSliders()
      featureSelectors()},
      {
        if ((TRUE %in% lapply(featureSliders(), is.null)) ||
            (TRUE %in% lapply(featureSliders(), is.na))) {
          selected$values.numeric = NULL
        } else {
          selected$values.numeric = unlist(featureSliders(),
                                           function(x) return(as.numeric(x)))
        }

        # now do the same with selectors
        if ((TRUE %in% lapply(featureSelectors(), is.null)) ||
            (TRUE %in% lapply(featureSelectors(), is.na))) {
          selected$values.factor = NULL
        } else {
          selected$values.factor = unlist(featureSelectors(),
                                          function(x) return(as.numeric(x)))
        }
      }
    )

    observeEvent({
      # update numeric feature data when user adjusts sliders
      selected$values.numeric
      selected$features.numeric},
      ignoreNULL = FALSE,
      {
        if (!(is.null(selected$values.numeric))) {
          for (i in 1:length(selected$features.numeric)) {
            numeric.feature = selected$features.numeric[i]
            numeric.value = selected$values.numeric[i]
            df$values.adj[numeric.feature] = numeric.value
          }
        }
        unselected.features = df$features.unused[
          !df$features.unused %in% c(selected$features)]
        df$values.adj[unselected.features] = data[unselected.features]
      }
    )

    observeEvent({
      # update factor feature data when user adjusts selectors
      selected$values.factor
      selected$features.factor},
      ignoreNULL = FALSE,
      {
        if (!(is.null(selected$features.factor))) {
          for (j in 1:length(selected$features.factor)) {
            factor.feature = selected$features.factor[j]
            factor.value = selected$values.factor[j]
            factor.levels = levels(data[[factor.feature]])
            df$values.adj[factor.feature] = factor(x = factor.value,
                                                   levels = factor.levels)
          }
        }
        unselected.features =
          df$features.unused[!df$features.unused %in% c(selected$features)]
        df$values.adj[unselected.features] = data[unselected.features]
      }
    )

    observeEvent({
      # rendering feature sliders for numeric features
      selected$features.numeric},
      {
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
      }
    )

    observeEvent({
      # render feature selectors
      selected$features.factor},
      {
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
      }
    )

  } # end server

  shinyApp(ui = app.ui, server = app.server)
}
