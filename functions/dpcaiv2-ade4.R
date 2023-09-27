"dpcaiv2" <- function (dudi, dfR = NULL, dfQ = NULL, scannf = TRUE, nf = 2) 
{
  lm.pcaiv <- function(x, df, weights, use) {
    if (!inherits(df, "data.frame")) 
      stop("data.frame expected")
    reponse.generic <- x
    begin <- "reponse.generic ~ "
    fmla <- stats::as.formula(paste(begin, paste(names(df), collapse = "+")))
    df <- cbind.data.frame(reponse.generic, df)
    lm0 <- stats::lm(fmla, data = df, weights = weights)
    if (use == 0) 
      return(predict(lm0))
    else if (use == 1) 
      return(residuals(lm0))
    else if (use == -1) 
      return(lm0)
    else stop("Non convenient use")
  }
  if (!inherits(dudi, "dudi")) 
    stop("dudi is not a 'dudi' object")
  if(!is.null(dfR)){    
    dfR <- data.frame(dfR)
    if (!inherits(dfR, "data.frame")) 
      stop("dfR is not a 'data.frame'")
    if (nrow(dfR) != length(dudi$lw)) 
      stop("Non convenient dimensions")
    
    isfactor1 <- unlist(lapply(as.list(dfR), is.factor))
    for (i in 1:ncol(dfR)) {
      if (!isfactor1[i]) 
        # scale = TRUE changed to FALSE to match ter Braak paper
        dfR[, i] <- scalewt(dfR[, i], wt = dudi$lw, scale = FALSE, center = TRUE)
    }
  }
  if(!is.null(dfQ)){
    dfQ <- data.frame(dfQ)
    if (!inherits(dfQ, "data.frame")) 
      stop("dfQ is not a 'data.frame'")
    if (nrow(dfQ) != length(dudi$cw)) 
      stop("Non convenient dimensions")
    
    isfactor2 <- unlist(lapply(as.list(dfQ), is.factor))
    for (i in 1:ncol(dfQ)) {
      if (!isfactor2[i]) 
        # scale = TRUE changed to FALSE to match ter Braak paper
        dfQ[, i] <- scalewt(dfQ[, i], wt = dudi$cw, scale = FALSE, center = TRUE)
    }
  }
  tab <- dudi$tab
  if(!is.null(dfR)){
    tab <- data.frame(apply(dudi$tab, 2, lm.pcaiv, df = dfR, use = 0, 
                            weights = dudi$lw))}
  if(!is.null(dfQ)){    
    tab <- data.frame(apply(tab, 1, lm.pcaiv, df = dfQ, use = 0, 
                            weights = dudi$cw))
    tab <- as.data.frame(t(tab))
  }
  res <- as.dudi(tab, dudi$cw, dudi$lw, scannf = scannf, nf = nf, 
               call = match.call(), type = "dpcaiv")
  if(!is.null(dfR)){res$R <- dfR} ## could be removed
  if(!is.null(dfQ)){res$Q <- dfQ} ## could be removed
  res$L <- dudi$tab ## could be removed
  if(!is.null(dfR)){
    
    U <- as.matrix(res$c1) * unlist(res$cw)
    U <- as.matrix(dudi$tab) %*% U
    U <- data.frame(U)
    row.names(U) <- row.names(dudi$tab)
    names(U) <- names(res$li)
    res$lsR <- U
    U <- as.matrix(res$c1) * unlist(res$cw)
    U <- data.frame(t(as.matrix(dudi$c1)) %*% U)
    row.names(U) <- names(dudi$li)
    names(U) <- names(res$li)
    res$asR <- U
    
    w <- apply(res$lsR, 2, function(x) stats::coefficients(lm.pcaiv(x, dfR, dudi$lw, -1)))
    w <- data.frame(w)
    names(w) <- names(res$l1)
    res$faR <- w
    fmla <- stats::as.formula(paste("~ ", paste(names(dfR), collapse = "+")))
    w <- scalewt(stats::model.matrix(fmla, data = dfR)[, -1], dudi$lw) * dudi$lw
    w <- t(w) %*% as.matrix(res$l1)
    w <- data.frame(w)
    res$corR <- w
    
  }
  if(!is.null(dfQ)){
    U <- as.matrix(res$l1) * unlist(res$lw)
    U <- t(as.matrix(dudi$tab)) %*% U
    U <- data.frame(U)
    row.names(U) <- names(dudi$tab)
    names(U) <- names(res$co)
    res$lsQ <- U
    U <- as.matrix(res$l1) * unlist(res$lw)
    U <- data.frame(t(as.matrix(dudi$l1)) %*% U)
    row.names(U) <- names(dudi$co)
    names(U) <- names(res$co)
    res$as <- U
    
    w <- apply(res$lsQ, 2, function(x) stats::coefficients(lm.pcaiv(x, dfQ, dudi$cw, -1)))
    w <- data.frame(w)
    names(w) <- names(res$c1)
    res$faQ <- w
    fmla <- stats::as.formula(paste("~ ", paste(names(dfQ), collapse = "+")))
    w <- scalewt(stats::model.matrix(fmla, data = dfQ)[, -1], dudi$cw) * dudi$cw
    w <- t(w) %*% as.matrix(res$c1)
    w <- data.frame(w)
    res$corQ <- w
  }

  return(res)
}