##===========================================
## Interrogation Functions for TempSED models
##===========================================

##------------------------------------
## Get parameters and values
##------------------------------------

TempSED_getparms <- function(out = NULL, as.vector = FALSE, which = NULL) {
  if (is.null(out))
    Parms <- .TMPSED$parms
  else if ("deSolve" %in% class(out))
    Parms <- attr(out, "parms")[1:length(.TMPSED$parms)]
  else stop("object 'out' not supported")
  
  if (as.vector) {
    if (! is.null(which))
      Parms <- Parms[which]
    return(Parms)
  } else {
    Units <- .TMPSED$parms_units
    Parms <- data.frame(parms = Parms, 
                        units = Units, 
                        description = .TMPSED$parms_description)
    if (! is.null(which))
      Parms <- Parms[which, ]
    return(Parms)
  }
}

##------------------------------------
## Grid
##------------------------------------

TempSED_getgrid <- function(out) {
  if (missing(out))
    out <- TempSED_run1D()
  if (inherits (out, "deSolve"))
    D <- attr(out, "grid.z")
  else 
    stop("object 'out' not supported for grid calculation")
  D
} 

TempSED_getdepth <- function(out) {
  if (missing(out))
    out <- TempSED_run1D()
  if ("deSolve" %in% class(out))
    D <- attr(out, "seddepth")
  else stop("object 'out' not supported")
  D
} 

TempSED_getdx <- function(out) {
  if (missing(out))
    out <- TempSED_run1D()
  if ("deSolve" %in% class(out))
    D <- attr(out, "dx")
  else stop("object 'out' not supported")
  D
} 

TempSED_getpor <- function(out) {
  if (missing(out))
    out <- TempSED_run1D()
  if ("deSolve" %in% class(out))
    D <- attr(out, "porosity")
  else stop("object 'out' not supported")
  D
} 

TempSED_getirr <- function(out) {
  if (missing(out))
    out <- TempSED_run1D()
  
  if ("deSolve" %in% class(out))
    D <- attr(out, "irrigation")
  else stop("object 'out' not supported")
  D
} 

##------------------------------------
## Get variables 
##------------------------------------
MeanVal <- function(out){  # takes into account unequal timing 
  if (is.vector(out)) return (out[-1])
  
  else if (nrow(out) <=  2) return (colMeans(out[,-1]))
  
  return((colSums(diff(out[,1])*(out[-1,]+out[-nrow(out),])*0.5)/
            (out[nrow(out),1]-out[1, 1]))[-1])
}

TempSED_get0Dvars <- function(out, subset, as.vector = FALSE, which = NULL) {
  
  if (missing(out)) {
    Dnames <- .TMPSED$out0D_names
    D <- rep(NA, times = length(Dnames))
    names(D) <- Dnames
    
  } else {
    
    if (missing(subset)) 
      r <- TRUE
    else {
      e <- substitute(subset)
      r <- eval(e, as.data.frame(out), parent.frame())
      if (is.numeric(r)) {
        isub <- r
      }
      else {
        if (!is.logical(r)) 
          stop("'subset' must evaluate to logical or be a vector with integers")
        r <- r & !is.na(r)
      }
      
    }
    if(sum(r) == 0) stop("'subset' did not lead to a selection")
    
    if ("deSolve" %in% class(out))
      D <- MeanVal(out[r, c("time", .TMPSED$out0D_names)])
    
    else stop("object 'out' not supported")
    
  }
  
  if (! as.vector)
    D <- data.frame(names = names(D), values = D, 
                    units = c(.TMPSED$out0D_units),
                    description = c(.TMPSED$out0D_description))
  
  if (! is.null(which)){
    if (is.vector(D))
      D <- D[which]
    else D <- D[which,]  
  } 
  row.names(D) <- NULL
  D
} 

TempSED_get1Dvars <- function(out, subset, which = NULL) {
  
  if (missing(out)) {
    D <- data.frame(names = c(.TMPSED$y_names, .TMPSED$out1D_names), 
                    units = c(.TMPSED$y_units, .TMPSED$out1D_units), 
                    description = c(.TMPSED$y_description, .TMPSED$out1D_description))
    if (! is.null(which))
      D <- D[(D$names %in% which), ]
    return(D)
  }   else{
    if (missing(subset)) 
      r <- TRUE
    else {
      e <- substitute(subset)
      r <- eval(e, as.data.frame(out), parent.frame())
      if (is.numeric(r)) {
        isub <- r
      }
      else {
        if (!is.logical(r)) 
          stop("'subset' must evaluate to logical or be a vector with integers")
        r <- r & !is.na(r)
      }
      if(sum(r) == 0) stop("'subset' did not lead to a selection")
    }
    subset <- NULL
    if ("deSolve" %in% class(out))  {
      AO <- attributes(out)[-(1:2)]
      cAO <- class(out)
      out <- out[r, ]
      if (is.vector(out)) out <- rbind(out, out)
      attributes(out) <- c(attributes(out), AO)
      class(out) <- cAO
      D <- NULL
      for (cc in c(.TMPSED$y_names, .TMPSED$out1D_names))
        D <- cbind(D,MeanVal(cbind(out[, 1],subset(out, which = cc))))
      rownames(D) <- NULL
      colnames(D) <- c(.TMPSED$y_names,
                       .TMPSED$out1D_names)
      D <- as.data.frame(D)  
    }
    else stop("object 'out' not supported")
  }
  D <- cbind(x   = TempSED_getdepth(out), 
             por = TempSED_getpor(out), D)
  if (! is.null(which))
    D <- D[ ,c( "x", "por", which)]
  D  
} 

TempSED_getstates <- function(out, subset, which = NULL) {
  if (missing(out)) 
    return(data.frame(names = .TMPSED$y_names, 
                      units = .TMPSED$y_units, 
                      description = .TMPSED$y_description))
  
  if (missing(subset)) 
    r <- TRUE
  else {
    e <- substitute(subset)
    r <- eval(e, as.data.frame(out), parent.frame())
    if (is.numeric(r)) {
      isub <- r
    }
    else {
      if (!is.logical(r)) 
        stop("'subset' must evaluate to logical or be a vector with integers")
      r <- r & !is.na(r)
    }
    if(sum(r) == 0) stop("'subset' did not lead to a selection")
  }
  subset <- NULL
  if ("deSolve" %in% class(out))  {
    AO <- attributes(out)[-(1:2)]
    cAO <- class(out)
    out <- out[r, ]
    if (is.vector(out)) out <- rbind(out, out)
    attributes(out) <- c(attributes(out), AO)
    class(out) <- cAO
    D <- NULL
    for (cc in .TMPSED$y_names)
      D <- cbind(D, MeanVal(cbind(out[,1], subset(out, which = cc))))
    rownames(D) <- NULL
    colnames(D) <- .TMPSED$y_names
    D <- as.data.frame(D)  
  }
  else stop("object 'out' not supported")
  
  D <- cbind(x   = TempSED_getdepth(out), 
             por = TempSED_getpor(out), D)
  if (! is.null(which))
    D <- D[ ,c( "x", "por", which)]
  D  
} 

