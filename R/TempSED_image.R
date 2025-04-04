#==========================================================
# helper functions
#==========================================================

repdots <- function(dots, n) {
  if (is.function(dots))
    dots
  else rep(dots, length.out = n)
}
#==========================================================
expanddots <- function(dots, default, n) {
  dots <- if (is.null(dots))
    default
  else dots
  rep(dots, length.out = n)
}
#==========================================================
expanddotslist <- function(dots, n) {
  if (is.null(dots))
    return(dots)
  dd <- if (!is.list(dots))
    list(dots)
  else dots
  rep(dd, length.out = n)
}
#==========================================================
extractdots <- function(dots, index) {
  ret <- lapply(dots, "[", index)
  ret <- lapply(ret, unlist)
  return(ret)
}
#==========================================================
setMain <- function(nv, ldots, nmdots){
  
  if (!any(match(nmdots, c("mfrow", "mfcol"), nomatch = 0))) {
    nc <- min(ceiling(sqrt(nv)), 3)
    nr <- min(ceiling(nv/nc), 3)
    mfrow <- c(nr, nc)
  }
  else if ("mfcol" %in% nmdots)
    mfrow <- rev(ldots$mfcol)
  else mfrow <- ldots$mfrow
  if (!is.null(mfrow))
    mf <- par(mfrow = mfrow)
  ask <- ldots$ask
  if (is.null(ask))
    ask <- prod(par("mfrow")) < nv && dev.interactive()
  return(ask)
}
#==========================================================
cleanLdots <- function(ldots, nmdots, nv, Which, att){
  if ("legend" %in% nmdots) {
    if ("colkey" %in% nmdots)
      stop("'legend' and 'colkey' cannot both be specified")
    if (ldots$legend == FALSE)
      ldots$colkey <- FALSE
    ldots$legend <- NULL
  }
  Dotmain      <- lapply(ldots, repdots, nv)
  if ("clab" %in% nmdots)
    Clab <- rep(ldots$clab, length.out = nv)
  else {
    Un <- att$units[Which]
    Clab <- Un
  }
  Dotmain$clab <- Clab
  return(Dotmain)
}

#==========================================================
# the main image function
#==========================================================

image2D.TempSEDdyn <- function (z, which = NULL, subset = NULL,
                                time_unit = c("sec", "hour", "day", "year"), 
                                xlab = time_unit, time_origin = NULL, plot_water = FALSE, plot_air = FALSE,
                                colkey = list(cex.clab = 0.8, line.clab = 0.5, cex.axis = 0.8),...)
{
  if (!"deSolve" %in% class(z))
    stop("cannot make an image from output that is not dynamic")
  
  time_unit <- match.arg(time_unit)
  times <- 
    switch(time_unit,
           sec  = z[,1],
           hour = z[,1]/3600,
           day  = z[,1]/86400,
           year = z[,1]/86400/365)
  z[,1] <- times
  
  att <- attributes(z)
  if (!missing(subset)) {
    e <- substitute(subset)
    r <- eval(e, as.data.frame(z), parent.frame())
    if (is.numeric(r)) {
      isub <- r
    }
    else {
      if (!is.logical(r))
        stop("'subset' must evaluate to logical or be a vector with integers")
      isub <- r & !is.na(r)
    }
    z   <- z[isub, ]
    attributes(z) <- c(attributes(z), att[-(1:2)])
  }
  
  if (att$model == "TempSED2D")  # not yet included
    image2D.TempSED2D (z = z, which = which, colkey = colkey,
                       time_origin = time_origin, 
                       plot_water = plot_water, xlab = xlab, 
                       ...)
  else # (att$model == "TempSED1D")
    image2D.TempSED1D (z = z, which = which, colkey = colkey,
                       time_origin = time_origin, xlab = xlab, 
                       plot_water = plot_water, plot_air = plot_air, ...)
  #  else
  #     image (x=z, which=which, colkey=colkey, method="image2D", legend=FALSE,
  #       ...)
}

setGeneric("image2D", function(z, ...) plot3D::image2D(z, ...))
setOldClass("TempSEDdyn")
setMethod("image2D", signature("TempSEDdyn"), image2D.TempSEDdyn)

#==========================================================
# the actual image functions
#==========================================================

image2D.TempSED1D <- function (z, which, colkey, time_origin, 
                               plot_water, plot_air = FALSE, ...)
{
  x  <- z
  if (!"deSolve" %in% class(x))
    stop("cannot make an image from output that is not dynamic")
  ldots  <- list(...)
  nmdots <- names(ldots)
  
  att    <- attributes(x)
  model  <- att$model
  
  if (! model %in% c("TempSED1D", "sw1DH"))
    stop("image2D.TempSED1D works with TempSED1D models only")
  
  Dec <- c(att$ynames, att$var1D)
  if ("grid" %in% nmdots) {
    grid <- ldots$grid
    if (is.list(grid)){
      grid <- grid$x.mid
      gridint <- grid$x.int
    }
    ldots$grid <- NULL
  }
  else {
    grid <- att$seddepth
    gridint <- att$grid$x.int
    
  }
  
  Which <- which
  if (is.null(Which) | missing(which))
    Which <- 1:att$nspec
  if (is.numeric(Which))
    Which <- Dec[Which]
  
  nv  <- length(Which)
  ask <- setMain(nv, ldots, nmdots)
  ldots$ask <- NULL
  
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  
  Dotmain      <- cleanLdots(ldots, nmdots, nv, Which, att)
  Dotmain$main <- expanddots(ldots$main, Which, nv)
  Dotmain$xlab <- expanddots(ldots$xlab, "times [s]", nv)
  
  if (model == "sw1DH")  # removed
    Dotmain$ylab <- expanddots(ldots$ylab, "distance [m]", nv)
  else #TempSED1D
    Dotmain$ylab <- expanddots(ldots$ylab, "depth [m]", nv)
  
  xxlim <- expanddotslist(ldots$xlim, nv)
  yylim <- expanddotslist(ldots$ylim, nv)
  zzlim <- expanddotslist(ldots$zlim, nv)
  
  times <- x[  , 1]
  isPOSIXct <- FALSE
  
  if (! is.null(time_origin)){
    if (inherits(time_origin, c("POSIXct", "POSIXlt", "character"))){
      times <- as.POSIXct(times, origin = time_origin)
      isPOSIXct <- TRUE
      if (is.null(ldots$xlab)) Dotmain$xlab <- "time"      
    } else
      times <- times - time_origin
  }
  
  for (ip in 1:nv) {
    z <- subset(x, which = Which[ip])
    dotmain <- extractdots(Dotmain, ip)
    if (!is.null(xxlim))
      dotmain$xlim <- xxlim[[ip]]
    if (!is.null(yylim))
      dotmain$ylim <- yylim[[ip]]
    else if (model == "TempSED1D")
      dotmain$ylim <- rev(range(grid))
    if (!is.null(zzlim))
      dotmain$zlim <- zzlim[[ip]]
    else dotmain$zlim <- range(z, na.rm = TRUE)
    List <- alist(z = z, x = times, y = grid, colkey = colkey)
    
    if ((plot_water | plot_air) & 
        Which[ip] == "Temperature"){  # add water height/plot_water temp
      
      Air.temp  <- subset(x, which = "Airtemperature")
      BW.temp   <- subset(x, which = "Watertemperature")
      BW.height <- - pmax(0, 
                          subset(x, which = "Waterheight"))
      nva <- length(Air.temp)
      # NOT SURE WHAT TO USE HERE...
      #      Air.temp  <- 0.5*(Air.temp [-1] + Air.temp [-nva])
      #      BW.temp   <- 0.5*(BW.temp  [-1] + BW.temp  [-nva])
      Air.temp  <- Air.temp [-nva]
      BW.temp   <- BW.temp  [-nva]
      
      BWy       <- matrix(nrow = length(times-1), ncol = length(gridint),
                          data = gridint, byrow = TRUE)
      
      z         <- 0.5*(z[-1,] + z[-nrow(z),])
      
      if (is.null(yylim))
        dotmain$ylim <- rev(range(c(dotmain$ylim, min(BW.height))))
      
      if (plot_water){
        z         <- cbind(BW.temp, z)
      } else z    <- cbind(NA, z)
      
      BWy       <- cbind(BW.height, BWy)
      if (plot_air){
        z         <- cbind(Air.temp, z)
        BWy       <- cbind(min(BW.height), BWy)
      }  
      
      
      List <- alist(z = z, x = times, y = BWy, colkey = colkey)
      
    } else isPOSIXct <- FALSE # will use time labels by default
    
    if (isPOSIXct){
      dotmain$frame.plot <- TRUE
      dotmain$axes <- FALSE
    } 
    
    do.call("image2D", c(List, dotmain))
    
    if (isPOSIXct){
      axis.POSIXct (side = 1)
      axis (side = 2, at = pretty(dotmain$ylim))
    }
  }  
}

#==========================================================

image2D.TempSED2D <- function (z, which, colkey, time_origin, 
                               plot_water, BW = FALSE, ...)
{
  x  <- z
  if (!"deSolve" %in% class(x))
    stop("cannot make an image from output that is not dynamic")
  ldots  <- list(...)
  nmdots <- names(ldots)
  att    <- attributes(x)
  model  <- att$model
  
  if (model != "TempSED2D")
    stop("image2D.TempSED2D works with TempSED2D models only")
  
  dimens <- att$dimens
  Nx <- dimens[1]
  Nz <- dimens[2]
  Dec2D  <- c(att$ynames, att$var2D)
  Dec1D  <- att$var1D
  if ("grid" %in% nmdots) {
    grid <- ldots$grid
    if (is.list(grid)){
      grid.x <- grid$x.mid
      grid.y <- grid$y.mid
    }
    ldots$grid <- NULL
  } else {
    grid.x <- att$distance
    grid.y <- att$seddepth
  }
  Which <- which
  if (is.null(Which) | missing(which))
    Which <- 1:att$nspec
  if (is.numeric(Which))
    Which <- c(Dec2D, Dec1D)[Which]
  n2D <- length(which(Which%in%Dec2D))
  n1D <- length(which(Which%in%Dec1D))
  
  if (n1D > 0 & n2D > 0)
    stop("can only plot either a collection of 1D or of 2D variables, not both")
  nv  <- n2D*nrow(z) + n1D
  ask <- setMain(nv, ldots, nmdots)
  ldots$ask <- NULL
  
  if (ask) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }
  
  Dotmain <- cleanLdots(ldots, nmdots, nv, Which, att)
  
  if (n1D > 0){
    Dotmain$main <- expanddots(ldots$main, Which, nv)
    Dotmain$xlab <- expanddots(ldots$xlab, "times [s]", nv)
    Dotmain$ylab <- expanddots(ldots$ylab, "distance [m]", nv)
    
  } else { #2D variable
    Main <- ldots$main
    Dotmain$main <- NULL
    Dotmain$xlab <- expanddots(ldots$xlab, "distance [m]", nv)
    Dotmain$ylab <- expanddots(ldots$xlab, "depth [m]", nv)
  }
  xxlim <- expanddotslist(ldots$xlim, nv)
  yylim <- expanddotslist(ldots$ylim, nv)
  zzlim <- expanddotslist(ldots$zlim, nv)
  times <- x[  , 1]
  if (! is.null(time_origin))
    times <- as.POSIXct(times, origin = time_origin)
  
  if (n2D > 0) { # 2D variable
    
    for (i in 1:nrow(x)) {
      for (ip in 1:length(Which)) {
        z <- subset(x, which = Which[ip], arr = TRUE)[,,i]
        dotmain <- extractdots(Dotmain, ip)
        if (is.null(Main))
          dotmain$main <- paste(Which[ip], " (t=", times[i], "s)", sep = "")
        
        if (!is.null(xxlim))
          dotmain$xlim <- xxlim[[ip]]
        if (!is.null(yylim))
          dotmain$ylim <- yylim[[ip]]
        else dotmain$ylim <- rev(range(grid.y))
        if (!is.null(zzlim))
          dotmain$zlim <- zzlim[[ip]]
        else dotmain$zlim <- range(z, na.rm = TRUE)
        
        List <- alist(z = z, x = grid.x, y = grid.y, colkey = colkey)
        if (plot_water & Which[ip] == "Temperature"){
          depth <- att$waterdepth
          
          BW.temp   <- subset(x, which = "Watertemperature")[i,]
          BW.height <- -pmax(0,subset(x, which = "Waterheight")[i,])
          z         <- cbind(BW.temp, z)
          BWy       <- matrix(nrow = Nx, ncol = Nz, data = grid.y, byrow = TRUE)
          BWy       <- BWy + matrix(nrow = Nx, ncol = Nz, data = depth)
          BWy       <- cbind(BW.height + BWy[,1], BWy)
          
          List <- alist(z = z, x = grid.x, y = BWy, colkey = colkey)
          if (!is.null(yylim))
            dotmain$ylim <- yylim[[ip]]
          else dotmain$ylim <- rev(range(BWy))
          
          if (!is.null(zzlim))
            dotmain$zlim <- zzlim[[ip]]
          else {  # to avoid round off
            zr <- range(z, na.rm = TRUE)
            dotmain$zlim <- zr + diff(zr)*c(-1e-6,1e-6)
          }
        }
        
        do.call("image2D", c(List, dotmain))
        if (BW & Which[ip] == "Temperature")
          lines(grid.x, -depth, col = "black", lty = 1)
        
      } # ip in Which
    } # i in nrow
    
  } else {  # one-D
    for (ip in 1:length(Which)) {
      z <- subset(x, which = Which[ip])
      dotmain <- extractdots(Dotmain, ip)
      if (!is.null(xxlim))
        dotmain$xlim <- xxlim[[ip]]
      if (!is.null(yylim))
        dotmain$ylim <- yylim[[ip]]
      if (!is.null(zzlim))
        dotmain$zlim <- zzlim[[ip]]
      else dotmain$zlim <- range(z, na.rm = TRUE)
      List <- alist(z = z, x = times, y = grid.x, colkey = colkey)
      do.call("image2D", c(List, dotmain))
    }
  }  # end if
}

