library(ggrepel)
library(ggplot2)

plotmat <- function(mar = 2, marl = 1.2,
                    E = FALSE, T_ = FALSE,
                    r, c, l = NULL, k = NULL) {
  
  xmin <- 0
  xmax <- c
  ymin <- 0
  ymax <- r
  
  xmat <- c/2
  ymat <- r/2
  
  xcol <- c(-marl, c/2)
  ycol <- c(r/2, r+marl)
  vjust <- c(0.5, 0)
  hjust <- c(1, 0.5)
  
  labmat <- 'italic(Y)'
  labcol <- c('italic(r)', 'italic(c)')
  
  xlim <- c(-2*marl, c+marl)
  ylim <- c(-marl, r + 2*marl)
  
  if (E) {
    xmin <- c(xmin, c+mar)
    xmax <- c(xmax, c+mar+l)
    ymin <- c(ymin, 0)
    ymax <- c(ymax, r)
    
    xmat <- c(xmat, mar+c+l/2)
    ymat <- c(ymat, r/2)
    
    xcol <- c(xcol, c+mar+l/2)
    ycol <- c(ycol, r+marl)
    vjust <- c(vjust, 0)
    hjust <- c(hjust, 0.5)
    
    labmat <- c(labmat, 'italic(E)')
    labcol <- c(labcol, 'italic(l)')
    
    xlim[2] <- xlim[2] +mar+l
  }
  if (T_) {
    xmin <- c(xmin, 0)
    xmax <- c(xmax, c)
    ymin <- c(ymin, -mar)
    ymax <- c(ymax, -mar-k)
    
    xmat <- c(xmat, c/2)
    ymat <- c(ymat, -mar-k/2)
    
    xcol <- c(xcol, -marl)
    ycol <- c(ycol,  -mar-l/2)
    vjust <- c(vjust, 0.5)
    hjust <- c(hjust, 1)
    
    labmat <- c(labmat, 'italic(T)')
    labcol <- c(labcol, 'italic(k)')
    
    ylim[1] <- ylim[1] - mar-k
  }
  
  ggplot() +
    annotate("rect", 
             xmin = xmin, 
             xmax = xmax, 
             ymin = ymin, 
             ymax = ymax, 
             fill = "white", color = "black") +
    annotate("text",
             x = xmat,
             y = ymat,
             label = labmat, 
             parse = TRUE, size = 12, family = "serif") +
    annotate("text",
             x = xcol,
             y = ycol,
             vjust = vjust,
             hjust = hjust,
             label = labcol, 
             parse = TRUE, size = 8, family = "serif") +
    xlim(xlim) +
    ylim(ylim) +
    coord_fixed() + 
    theme_void()
  
}


#' Plot bi, tri or quadriplots
#'
#' @param indiv_row Matrix of individuals coordinates for rows
#' @param indiv_col Matrix of individuals coordinates for columns
#' @param indiv_row_lab Labels for row individuals
#' @param indiv_col_lab Labels for columns individuals
#' @param var_row Matrix of variables coordinates for rows
#' @param var_col Matrix of variables coordinates for columns
#' @param var_row_lab Labels for row variables
#' @param var_col_lab Labels for columns variables
#' @param row_color Color for the row individuals or variables
#' @param col_color Color for the columns individuals or variables
#' @param eig Eigenvalues vector
#' @param x Axis to use for the x-axis
#' @param y Axis to use for the y-axis
#' @param max.overlaps max.overlaps for ggrepel
#' @param mult factor to multiply the vectors (arrows on the plot)
#'
#' @return a ggplot
multiplot <- function(indiv_row = NULL, indiv_col = NULL, 
                      indiv_row_lab = NULL, indiv_col_lab = NULL, 
                      var_row = NULL, var_col = NULL, 
                      var_row_lab = NULL, var_col_lab = NULL, 
                      row_color = NULL, col_color = NULL,
                      eig = NULL,
                      x = 1, y = 2,
                      mult = 1,
                      max.overlaps = 10) {
  
  xlab <- paste0("Axis ", x, " (", round(eig[x]/sum(eig)*100, 1), " % variability)")
  ylab <- paste0("Axis ", y, " (", round(eig[y]/sum(eig)*100, 1), " % variability)")
  
  g <- ggplot() +
    geom_vline(xintercept = 0) + 
    geom_hline(yintercept = 0) +
    coord_cartesian() +
    theme_linedraw() +
    xlab(xlab) +
    ylab(ylab)
  
  # Plot row individuals
  if (!is.null(indiv_row)) {
    g <- g +
      geom_point(aes(x = indiv_row[, x], y = indiv_row[, y]), 
                 col = row_color) +
      geom_text_repel(aes(x = indiv_row[, x], y = indiv_row[, y], 
                          label = indiv_row_lab),
                      col = row_color, 
                      max.overlaps = max.overlaps)
  }
  
  # Plot column individuals
  if (!is.null(indiv_col)) {
    g <- g +
      geom_point(aes(x = indiv_col[, x], y = indiv_col[, y]), 
                 col = col_color) +
      geom_text_repel(aes(x = indiv_col[, x], y = indiv_col[, y], 
                          label = indiv_col_lab),
                      col = col_color, 
                      max.overlaps = max.overlaps)
  }
  
  # Plot rows variables
  if (!is.null(var_row)) {
    g <- g +
      geom_segment(aes(x = 0, y = 0, 
                       xend = var_row[, x]*mult, yend = var_row[, y]*mult),
                   arrow = arrow(length=unit(0.20,"cm"))) +
      geom_label(aes(x = var_row[, x]*mult, y = var_row[, y]*mult,
                     label = var_row_lab),
                 col = row_color,
                 vjust = ifelse(var_row[, y] > 0, 0, 1))
  }
  
  
  # Plot columns variables
  if (!is.null(var_col)) {
    g <- g +
      geom_segment(aes(x = 0, y = 0, 
                       xend = var_col[, x]*mult, yend = var_col[, y]*mult),
                   arrow = arrow(length=unit(0.20,"cm"))) +
      geom_label(aes(x = var_col[, x]*mult, y = var_col[, y]*mult,
                     label = var_col_lab),
                 col = col_color,
                 vjust = ifelse(var_col[, y] > 0, 0, 1))
  }
  
  g
}

