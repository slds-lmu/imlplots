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
    geom_line(data = pred,
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
    geom_line(data = iceplot.data,
              aes_string(x = var, y = "value",
                         group = "variable"),
              color = "steelblue",
              size = line.size,
              alpha = line.alpha) +
    geom_line(data = pred,
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

createRawAlePlot <- function(data, target, model, var, knots) {
  pred_function = function(X.model, newdata) {
    as.numeric(predict(X.model, newdata))
  }
  # tmp = tempfile()
  # png(tmp)
  
  obj <- ALEPlot::ALEPlot(
    data[ , -which(names(data) == target)],
    model,
    pred.fun = pred_function,
    J = var,
    K = knots
  )
  # file.remove(tmp)
  return(obj)
}

regrAlePlot = function(data, model, target, var, knots, gfx_package) {
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
        df = melt(y, na.rm = TRUE)
        colnames(df) = c(var[[1]], var[[2]], "value")
        ggplot(data = df, aes_string(x = var[[1]], y = var[[2]], color = "value")) +
          stat_summary_2d(aes(z = value), fun = mean, bins = 50) +
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
      
    # #Produce data frame to produce ggplot
    # #df.3 is f.values the conture coordinates
    # df.3 = as.vector(ALE.DATA$f.values)
    # #df.1 are the x values
    # df.1 = rep(ALE.DATA$x.values[[1]], ncol(ALE.DATA$f.values))
    # #df.2 are y values
    # df.2 = vector(length = ncol(ALE.DATA$f.values)*nrow(ALE.DATA$f.values))
    # for(i in 1:length(ALE.DATA$x.values[[2]])){
    #   df.2[(1 + (i-1)*
    #           nrow(ALE.DATA$f.values)):(nrow(ALE.DATA$f.values)*i)]<- rep(
    #             ALE.DATA$x.values[[2]][i], nrow(ALE.DATA$f.values))
    # }
    # #produce data frame
    # df <- data.frame(df.1, df.2, df.3)
    # #plot
    # ggplot(data = df, mapping = aes(x = df.1, y = df.2, z = df.3)) +
    #   geom_contour(aes(colour = ..level..)) +
    #   labs(y =  var[2], x = var[1]) +
    #   theme_pubr()
    
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
  p = plot_ly(x = ~get(var[[1]]), y = ~get(var[[2]]), z = ~get(target)) %>%
    add_markers(data = data, marker = list(size = 1.5)) %>%
    layout(scene = list(xaxis = list(title = var[[1]]),
                        yaxis = list(title = var[[2]]),
                        zaxis = list(title = target))
    )
  if (!is.null(highlighted)) {
    p = p %>%
      add_markers(data = data[highlighted, ],
                  marker = list(size = 3, color = "brown"))
    return(p)
  } else {
    return(p)
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
