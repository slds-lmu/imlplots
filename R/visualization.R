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

regrAlePlot = function(data, model, target, var, knots) {
  AlePlot.invisible <- function(){
    ff <- tempfile()
    png(filename=ff)
    pred = function(X.model, newdata) as.numeric(predict(X.model, newdata))
    res <- ALEPlot(data[,-which(names(data) == target)], model,
                   pred.fun = pred, J=var)
    dev.off()
    unlink(ff)
    res
  }
  ALE.DATA <- AlePlot.invisible()
  #If target is of dimension 1
  if (length(var) == 1){
    #less than 40 knots
    if (knots <= 40) {
      #produce data frame
      df = data.frame(ALE.DATA$x.values, ALE.DATA$f.values)
      #plot
      ggplot() +
        geom_line(data = df, aes_string(x = "ALE.DATA.x.values",
                                        y = "ALE.DATA.f.values"), color = "steelblue",
                  size = 1)  +
        geom_point(data = df, aes_string(x = "ALE.DATA.x.values",
                                         y = "ALE.DATA.f.values"), color = "steelblue",
                   size = 1) +
        labs(y = paste("ALE main effect for", var, "on", target), x = var) +
        theme_pubr()
      #more than 40 knots
    } else {
      #produce data frame
      df = data.frame(ALE.DATA$x.values, ALE.DATA$f.values)
      #plot
      ggplot() +
        geom_line(data = df, aes_string(x = "ALE.DATA.x.values",
                                        y = "ALE.DATA.f.values"), color = "steelblue",
                  size = 1)  +
        labs(y = paste("ALE main effect for", var, "on", target), x = var) +
        theme_pubr()
    }
    #if var is of dimension 2
  } else {
    #Produce data frame to produce ggplot
    #df.3 is f.values the conture coordinates
    df.3 = as.vector(ALE.DATA$f.values)
    #df.1 are the x values
    df.1 = rep(ALE.DATA$x.values[[1]], ncol(ALE.DATA$f.values))
    #df.2 are y values
    df.2 = vector(length = ncol(ALE.DATA$f.values)*nrow(ALE.DATA$f.values))
    for(i in 1:length(ALE.DATA$x.values[[2]])){
      df.2[(1 + (i-1)*
              nrow(ALE.DATA$f.values)):(nrow(ALE.DATA$f.values)*i)]<- rep(
                ALE.DATA$x.values[[2]][i], nrow(ALE.DATA$f.values))
    }
    #produce data frame
    df <- data.frame(df.1, df.2, df.3)
    #plot
    ggplot(data = df, mapping = aes(x = df.1, y = df.2, z = df.3)) +
      geom_contour(aes(colour = ..level..)) +
      labs(y =  var[2], x = var[1]) +
      theme_pubr()
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
  ggplot(data = data, aes_string(y = target, x = var)) +
    geom_point(size = pointsize, color = "steelblue", shape = 1) +
    geom_point(data = data[which(rownames(data) %in% highlighted), ], shape = 19, color = "brown", size = 3) +
    theme_pubr()
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
