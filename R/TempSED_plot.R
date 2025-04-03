# ==============================================================================
##############   matplot, plot of TempSED objects
# ==============================================================================

# ------------------------------------------------------------------------------
# S3 methods
# ------------------------------------------------------------------------------

matplot1D.TempSED <- function (z, which, ...) {

  if (!"deSolve"%in% class(z)) stop ("cannot make an image from output that is not dynamic")
  ldots <- list(...)
  nmdots <- names(ldots)
  
  Dec <- rbind(TempSED_getstates(), TempSED_get1Dvars())

  if ("grid" %in% nmdots){
    grid <- ldots$grid
    if (is.list(grid))
      grid <- grid$x.mid
    ldots$grid <- NULL
  } else 
    grid <- TempSED_getdepth(z)
  
  
  Which <- which
  if (is.null(Which) | missing(Which))
    Which <- attributes(z)$ynames

  if (is.numeric(Which))
    Which <- Dec$names[Which]
  
  nv <- length(Which)
  
  if ("main" %in% nmdots)
    Main <- rep(ldots$main, length.out = nv)
  else
    Main <- NULL
  ldots$main <- NULL

  if ("xlab" %in% nmdots)
    Xlab <- rep(ldots$xlab, length.out = nv)
  else{
    Xlab <- Dec[match(Which, Dec$names),"units"]
  }    
  ldots$xlab <- NULL

  if ("ylab" %in% nmdots)
    Ylab <- rep(ldots$ylab, length.out = nv)
  else{
    Ylab <- "m"
  }    
  ldots$ylab <- NULL  
  
  if (!any(match(nmdots, c("mfrow", "mfcol"), nomatch = 0))) {
     nc <- min(ceiling(sqrt(nv)), 3)
     nr <- min(ceiling(nv/nc), 3)
     mfrow <- c(nr, nc)
  } else if ("mfcol" %in% nmdots)
     mfrow <- rev(ldots$mfcol)
  else mfrow <- ldots$mfrow

  if (! is.null(mfrow))  mf <- par(mfrow = mfrow)
  ## interactively wait if there are remaining figures

  if (is.null(ldots$ask))
    ldots$ask <- prod(par("mfrow")) < nv && dev.interactive()
  par(ask = ldots$ask)
  ldots$ask <- NULL

  if (is.null(ldots$ylim))
    ldots$ylim <- rev(range(grid))
  
  if ("xyswap" %in% nmdots) {
    xyswap <- ldots$xyswap
    ldots$xyswap <- NULL
  } else xyswap <- TRUE
    
  class(z) <- class(z)[-1]  
  do.call("matplot.1D",
            c(alist(x = z, which = Which, grid = grid, main = Main,
                    xlab = Xlab, ylab = Ylab, xyswap = xyswap), ldots))
}

matplot1D.TempSEDdyn <- function (z, 
                                  type = "l", col = "grey", lty = 1, ...) {
   which <- NULL
   
   matplot1D.TempSED(z = z, which = which, 
     type = type, col = col, lty = lty, ...)
  }


# ------------------------------------------------------------------------------
# S3 methods
# ------------------------------------------------------------------------------
matplot1D <- function (z, ...) UseMethod("matplot1D")

matplot1D.default <- function (z, ...) {
if (inherits (z, "TempSEDdyn"))
  matplot1D.TempSEDdyn(z,...)
else
  deSolve::matplot.1D(x = z,...)
#  graphics::matplot(x,...)
  NextMethod()
}

#setGeneric("matplot1D", function(z, ...) matplot1D(z, ...))
#setOldClass("TempSEDdyn")
#setMethod("matplot1D", signature("TempSEDdyn"), matplot1D.TempSEDdyn)

# ==============================================================================
# Plot function for class TempSEDdyn that also adds 
# the units to the y labels.
# ==============================================================================

plot.TempSEDdyn <- function(x, ..., select = NULL, which = select, 
                            time_unit = c("sec", "hour", "day", "year"), 
                            xlab = time_unit, ylab = NULL, lty = 1, las = 1){
  time_unit <- match.arg(time_unit)
  times <- 
    switch(time_unit,
           sec  = x[,1],
           hour = x[,1]/3600,
           day  = x[,1]/86400,
           year = x[,1]/86400/365)
  x[,1] <- times
  
  plot.TempSED(x = x, ..., which = which, #time_origin = time_origin, 
                          xlab = xlab, ylab = ylab, lty = lty, las = las)
}


plot.TempSED <- function(x, ..., select = NULL, which = select, 
                         xlab = NULL, ylab = NULL, lty = 1, las = 1){
  
  if (is.null(which))
    which <- 1:(ncol(x)-1)
  else if (is.character(which[1]))
    which <- unlist(lapply(which, 
                           FUN = function(X) which(colnames(x)[-1] %in% X))) 
  if (is.null(ylab)) 
    ylab <- attributes(x)$units[which]
  
  classx <- class(x)   
  class(x) <- classx[-which(classx %in% c("TempSEDdyn"))]
  
  plot(x, ..., which = which, ylab = ylab, lty = lty, las = las)  
}

# ------------------------------------------------------------------------------
# S3 methods
# ------------------------------------------------------------------------------

setOldClass("TempSEDdyn")
setMethod("plot", signature("TempSEDdyn"), plot.TempSEDdyn)


# ------------------------------------------------------------------------------
# S3 method for 
# ------------------------------------------------------------------------------
matplot0D <- function (x, ...) UseMethod("matplot0D")

matplot0D.default <- function (x, ...) {
  if (inherits (x, "TempSEDdyn"))
    matplot0D.TempSEDdyn(x,...)
  else
    deSolve::matplot.0D(x = x,...)
  #  graphics::matplot(x,...)
  NextMethod()
}


matplot0D.TempSEDdyn <- function(x, ..., select = NULL, which = select, 
                                 time_unit = c("sec", "hour", "day", "year"), 
                            xlab = time_unit, lty = 1){
  time_unit <- match.arg(time_unit)
  times <- 
    switch(time_unit,
           sec  = x[,1],
           hour = x[,1]/3600,
           day  = x[,1]/86400,
           year = x[,1]/86400/365)
  x[,1] <- times
  
  matplot.0D(x, ..., which = which,
             xlab = xlab, lty = lty)
}
