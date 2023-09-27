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
