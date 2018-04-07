classifIcePlot = function(pred, var, knots, lines, centered, centerpoint) {
  longformat_pred = melt(pred, id.vars = var) %>%
    mutate(class = sub('\\..*$','', variable))
  pdp_data = filter(longformat_pred, grepl('ave', variable))
  iceplot_data = filter(longformat_pred, !grepl('ave', variable))

  if (lines <= 15) {
    line.alpha = 1
    line.size = 0.7
  } else if (lines > 15 & lines <= 45) {
    line.alpha = 0.7
    line.size = 0.5
  } else if (lines > 45 & lines <= 100) {
    line.alpha = 0.6
    line.size = 0.4
  } else if (lines > 10) {
    line.alpha = 0.4
    line.size = 0.3
  }
  # do not show points on lines
  plot <- ggplot() +
    geom_line(data = iceplot_data, aes_string(
      x = var, y = "value", group = "variable", color = "class"),
      size = line.size,
      alpha = line.alpha
    ) +
    geom_line(data = pdp_data, aes_string(
      x = var, y = "value", group = "class", color = "class"),
      size = 1, linetype = "dashed") +
    theme_pubr()

  if (centered == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = as.numeric(centerpoint), linetype = "dashed")
  } else {}
  return(plot)
}

classifPartialDependencePlot = function(pred, var, target, knots) {
  longformat_pred = melt(pred, id.vars = var) %>%
    mutate(class = sub('\\..*$','', variable))
  pdp_data = filter(longformat_pred, grepl('ave', variable))

  ggplot(data = pdp_data, aes_string(x = var, y = "value", group = "class",
                                     color = "class")) +
    geom_line(size = 0.3) +
    labs(y = paste("Probabilityfor classifying", target, "as..", sep = " "),
         color = "Class") +
    theme(legend.position= "bottom", legend.direction = "vertical") +
    theme_pubr()
}

regrPartialDependencePlot = function(pred, var, target, knots) {
  ggplot() +
    geom_line(
      data = pred,
      aes_string(x = var, y = "preds.ave"),
      color = "steelblue",
      size = 1) +
    labs(y =  target) +
    theme_pubr()
  # }
}

regrIcePlot = function(pred, var, target, knots, lines, centered, centerpoint) {
  if (lines <= 15) {
    line.alpha = 1
    line.size = 0.7
  } else if (lines > 15 & lines <= 45) {
    line.alpha = 0.7
    line.size = 0.5
  } else if (lines > 45 & lines <= 100) {
    line.alpha = 0.6
    line.size = 0.4
  } else if (lines > 10) {
    line.alpha = 0.4
    line.size = 0.3
  }

  iceplot.data = melt(pred, id.vars = var)

  plot <- ggplot() +
    geom_line(
      data = iceplot.data,
      aes_string(
        x = var, y = "value", group = "variable"),
      color = "steelblue",
      size = line.size,
      alpha = line.alpha) +
    geom_line(
      data = pred,
      aes_string(x = var, y = "preds.ave"),
      color = "brown",
      size = 1) +
    labs(y =  target) +
    theme_pubr()
  if (centered == TRUE) {
    plot <- plot +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = as.numeric(centerpoint), linetype = "dashed")
  } else {}
  return(plot)
}


regrAlePlot = function(data, target, var1, var2 = NULL, knots, gfx_package) {

  if (any(class(data) == "warning") |
      any(class(data) == "error")) {
    ggplot() +
      annotate(
        geom = "text",
        x = 1, y = 1,
        label = paste(
          "ALEPlot function returned error or warning message: \n",
          data,
          "You might have selected a factor (like) variable.
          Second order effect ALE plots are not yet reliably supported for factor (like) variables."),
        size = 5
      ) +
      theme_pubr()
  } else {
    # no error or warning
    if (is.null(var2)) {
      # line plot
      plot = ggplot(
        data = data,
        aes_string(
          x = var1,
          y = "ale.effect")) +
        geom_line(size = 1, color = "steelblue") +
        labs(y = paste("ALE main effect on", target), x = var1) +
        theme_pubr()
    } else {
      # two variables
      if (gfx_package == "ggplot2") {
        # 2d heat map
        plot = ggplot(
          data = data, aes_string(x = var1, y = var2, color = "ale.effect")) +
          stat_summary_2d(aes(z = ale.effect), fun = mean, bins = 50) +
          theme_pubr()
      } else if (gfx_package == "plotly") {
        # 3d scatter
        df = acast(data, get(var1) ~ get(var2), value.var = "ale.effect", drop = FALSE)
        x = as.numeric(rownames(df))
        y = as.numeric(colnames(df))

        plot = plot_ly(x = x, y = y, z = df, type = "surface") %>%
        layout(scene = list(
          xaxis = list(title = var1),
          yaxis = list(title = var2),
          zaxis = list(title = paste("ALE effect on", target)))
        )
      }
    }
  }
  return(plot)
}

classifAlePlot = function(data, model, target, var, knots, gfx_package) {
  Sys.sleep(1)
  # give function time until all reactive variables are updated correctly
  aleplot_obj <- tryCatch({
    createRawAlePlot(
      data = data,
      target = target,
      model = model,
      var = var,
      knots = knots)},
    error = function(e) return(e),
    warning = function(w) return(w)
  )
  # ALEPlot package does not yet reliably support second order interactions
  if (any(class(aleplot_obj) == "warning") |
      any(class(aleplot_obj) == "error")) {
    ggplot() +
      annotate(geom = "text",
               x = 1, y = 1,
               label = paste(
                 "ALEPlot function returned error or warning message: \n",
                 aleplot_obj,
                 "You might have selected a factor (like) variable.
                 Second order effect ALE plots are not yet reliably supported for factor (like) variables."),
               size = 5
               ) +
      theme_pubr()
  } else {

    if (length(var) == 1) {
      ale_df = data.frame(x = aleplot_obj$x.values, y = aleplot_obj$f.values)
      plot = ggplot(data = ale_df,
                    aes_string(x = "x",
                               y = "y")
      ) +
        geom_line(size = 1, color = "steelblue") +
        labs(y = paste("ALE main effect of", var, "on", target), x = var) +
        theme_pubr()

      if (knots >= 40) {
        plot = plot +
          geom_point(size = 1, color = "steelblue")
      } else {}
      return(plot)

    } else if (length(var) == 2) {
      x = aleplot_obj$x.values
      y = aleplot_obj$f.values
      x1 = x[[1]]
      x2 = x[[2]]
      rownames(y) = x1
      colnames(y) = x2

      if (gfx_package == "ggplot2") {
        # heat_cols = colorRampPalette(c("white", "steelblue"))
        # image(x1, x2, y, col = heat_cols(12),
        #       xlim = range(x1),
        #       ylim = range(x2),
        #       xlab = var[[1]],
        #       ylab = var[[2]]
        # )
        # contour(x1, x2, y, add = TRUE, col = "brown", lwd = 2, labcex = 1.5)
        # plot = recordPlot()
        # return(plot)
        # base package way of producing 2nd order effects ale plot
        df = melt(y, na.rm = TRUE)
        colnames(df) = c(var[[1]], var[[2]], "ALE.effect")
        ggplot(data = df, aes_string(x = var[[1]], y = var[[2]], color = "ALE.effect")) +
          stat_summary_2d(aes(z = ALE.effect), fun = mean, bins = 50) +
          theme_pubr()
      } else if (gfx_package == "plotly") {
        plot = plot_ly(x = x1, y = x2, z = y, type = "surface") %>%
          layout(scene = list(
            xaxis = list(title = var[[1]]),
            yaxis = list(title = var[[2]]),
            zaxis = list(title = paste("ALE effect on", target)))
          )
        return(plot)
      }
    }
  }
}


scatterPlot <- function(data, target, var, highlighted) {
  if (nrow(data) <= 100) {
    pointsize = 1.5
  } else if (nrow(data) > 100 & nrow(data) <= 600) {
    pointsize = 1.2
  } else if (nrow(data > 600) & nrow(data) <= 1000) {
    pointsize = 0.8
  } else {
    pointsize = 0.6
  }
  plot = ggplot(data = data, aes_string(y = target, x = var)) +
    geom_point(size = pointsize, color = "steelblue", shape = 1) +
    geom_point(data = data[which(rownames(data) %in% highlighted), ], shape = 19, color = "brown", size = 3) +
    theme_pubr()
  return(plot)
}

scatterPlot3D <- function(data, target, var, highlighted = NULL) {
  plot = plot_ly(x = ~get(var[[1]]), y = ~get(var[[2]]), z = ~get(target)) %>%
    add_markers(data = data, marker = list(size = 4)) %>%
    layout(scene = list(xaxis = list(title = var[[1]]),
                        yaxis = list(title = var[[2]]),
                        zaxis = list(title = target))
    )
  if (!is.null(highlighted)) {
    plot = plot %>%
      add_markers(data = data[which(rownames(data) %in% highlighted), ],
                  marker = list(size = 5, color = "brown"))
    return(plot)
  } else {
    return(plot)
  }
}

placeholderPlot <- function() {
  ggplot() +
    annotate(geom = "text",
             x = 1, y = 1,
             label = "No observations detected.
             Change filter selections and / or sampling mode in data tab.",
             size = 5
    ) +
    theme_pubr()
}
