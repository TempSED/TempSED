
#===============================================================================
# 1D sediment temperature model, forced by water and air properties
# fortran version
#===============================================================================

TempSED_run1D <- function(
    parms        = list(),
    times        = 1:3600, # output times
    T_ini        = 10,     # Initial temperature distribution
    z_max        = 10,     # total length of sediment (in the vertical), m
    dz_1         = 1e-3,   # size of first grid cell, m (1e-3m=1 mm)
    Grid         = setup.grid.1D(N  = 100, dx.1 = dz_1, L = z_max),  
    
    
    # Sediment parameters
    porosity     = 0.5,    # [-] volume of water/bulk volume
    irrigation   = 0,      # [/s] irrigation rate
    
    # forcing functions:
    f_Waterheight      = 0,       # [m] height of overlying water
    f_Watertemperature = 10,      # [degC] temperature of overlying water
    f_Airtemperature   = 10,      # [degC] temperature of the air
    f_Qrel             = 0.3,     # [-] relative air humidity, water vapor as a fraction of saturated water vapor
    f_Pressure         = 101325,  # [Pa]
    f_Solarradiation   = 100,     # [W/m2]
    f_Windspeed        = 1,       # [m/s]
    f_Cloudiness       = 0.5,     # [-] cloud cover
    f_Deeptemperature  = NA, 
    dependency_on_T = FALSE, 
    
    sedpos    = NULL,
    verbose   = FALSE, 
    dtmax     = 3600
){
  
  # ----------------------
  # Check the grid
  # ----------------------
  N    <- .TMPSED$N                # number of layers - fixed
  
  # layer sizes (dx..), and depths (x..)  
  if (! all(c("dx", "dx.aux", "x.mid", "x.int") %in% names(Grid)))
    stop("'Grid' should be a list containing 'x.mid', 'x.int', 'dx', 'dx.aux'")
  
  if (length(Grid$dx) != N | length(Grid$x.mid) != N)  
    stop("'Grid$dx' and 'Grid$x.mid' should be a vector of length 100")
  
  if (length(Grid$dx.aux) != N+1 | length(Grid$x.int) != N+1)  
    stop("'Grid$dx.aux' and 'Grid$x.int' should be a vector of length 101")
  
  # ----------------------
  # Check the parameters
  # ----------------------
  Parms <- .TMPSED$parms                    
  nms <- names(Parms)                                                                   
  Parms[(namc <- names(parms))] <- parms                                                
  if (length(noNms <- namc[!namc %in% nms]) > 0)                                        
    warning("unknown names in parms: ", paste(noNms, collapse = ", "))                
  PP <- unlist(Parms)                                                                   
  
  # ----------------------
  # Check the profiles
  # ----------------------
  # Functions to check dimensionalities of properties
  
  FunVec <- function(val, name){
    if (is.function(val)) return(as.double(val(Grid$x.mid)))
    if (length(val) == 1)   val <- rep(val, times=N)
    if (length(val) == N+1) val <- 0.5*(val[-1]+val[-(N+1)])
    if (length(val) != N)
      stop (name, "should be either one number or a vector of length 100")
    as.double(val)
  }
  
  FunVecp1 <- function(val, name){
    if (is.function(val)) return(as.double(val(Grid$x.int)))
    if (length(val) == 1) val <- rep(val, times=N+1)
    if (length(val) != N+1)
      stop (name, "should be either one number or a vector of length 101")
    as.double(val)
  }
  
  # porosity (in middle of cells and interface)
  intpor   <- FunVecp1(porosity, "porosity")
  porosity <- FunVec (porosity, "porosity")
  
  # irrigation, defined in middle of cells
  irr      <- FunVec(irrigation, "irrigation")
  
  # initial condition  
  
  # output of previous run: use last values
  if ("TempSEDdyn" %in% class(T_ini))  
    if (attributes(T_ini)$model == "TempSED1D")
      T_ini <- T_ini[nrow(T_ini), 2:(1+N)]
  
  T_ini <- FunVec(T_ini,   "T_ini")
  
  # ----------------------
  # Check the forcings
  # ----------------------
  
  # times at which to estimate forcings, at most 1000 values
  outtimes <- times
  
  if (diff(range(times)) < 1000)
    ftimes <- times[1]:times[length(times)]
  
  else
    ftimes <- seq(from       = times[1], 
                  to         = times[length(times)], 
                  length.out = 1000)
  
  # function to process forcings
  getV <- function (FF, name)  {       # checks if a timeseries-or one value
    ff <- FF
    if (is.function(FF))
      ff <- cbind(ftimes, FF(ftimes))
    
    else if (length(FF) == 1){
      if (is.na(FF)) FF <- 0
      ff <- cbind(range(times), rep(FF, times=2))
    }
    
    else if (ncol(FF) != 2)
      stop("Forcing", name,
           " should be a function, one value or a two-columned matrix")
    
    else
      if (min(na.omit(FF[,1])) > min(times) | max(na.omit(FF[,1])) < max(times))
        stop ("Forcing times ", name, " should embrace output times")
    
    return(ff)
  }
  
  if (is.na (f_Deeptemperature)) deepBC <-  3 else deepBC <- 2
  
  Forcings <- list(
    f_Waterheight      = getV(f_Waterheight     , "f_Waterheight"),     # [m]
    f_Watertemperature = getV(f_Watertemperature, "f_Watertemperature"),# [degC]
    f_Airtemperature   = getV(f_Airtemperature  , "f_Airtemperature"),  # [degC]
    f_Qrel             = getV(f_Qrel            , "f_Qrel"),            # [-]
    f_Pressure         = getV(f_Pressure        , "f_Pressure"),        # [Pa]
    f_Solarradiation   = getV(f_Solarradiation  , "f_Solarradiation"),  # [W/m2]
    f_Windspeed        = getV(f_Windspeed       , "f_Windspeed"),       # [m/s]
    f_Cloudiness       = getV(f_Cloudiness      , "f_Cloudiness"),      # [-]
    f_Deeptemperature  = getV(f_Deeptemperature , "f_Deeptemperature")  # [degC]
  )
  
  parms <- as.double(c(PP, 
                       
                       dependency_on_T, deepBC = deepBC,
                       
                       porosity, intpor, irr, Grid$dx, Grid$dx.aux))
  
  # ----------------------
  # Run the model
  # ----------------------
  
  svar     <- .TMPSED$y_names
  outnames <- .TMPSED$out0D_names
  
  nspec <- 1  # one set of state variables (Temperature)
  nout  <- length(outnames)
  
  # names of fortran routines and the dll
  func     <- "modtemp1d"
  initfunc <- "inittemp1d"
  initforc <- "forctemp1d"
  dllname  <- "TempSED"  # name of the package
  
  if (length(times) == 1){
    ff <- cbind(times, unlist(lapply(Forcings, FUN=function(x) x[1,2])))
    
    FF <- DLLfunc(y = as.double(T_ini), times = as.double(times), 
                  parms = parms, forcings = ff,
                  dllname = dllname, func = func, 
                  initfunc = initfunc, initforc = initforc, 
                  outnames = outnames, nout = nout)
    return(FF)
  }
  
  ZZ <- capture.output(suppressWarnings(
    out <- ode.1D(
      y = as.double(T_ini), times = as.double(times), 
      initpar = parms, forcings = Forcings,
      
      dllname = dllname, func = func,
      initfunc = initfunc, initforc = initforc, 
      
      names = svar, outnames = outnames, nout = nout, 
      nspec = nspec, dimens = N, 
      verbose = verbose, method = "vode", hmax = dtmax)
  ))
  
  colnames(out)[2:(nspec*N+1)] <- as.vector(sapply(svar,
                                                   FUN = function(x) rep(x, times = N)))
  
  if (! is.null(sedpos)){
    ATT <- attributes(out)[-(1:2)]
    
    # positions where temperature output is wanted - find layer closest to depth
    ised     <- 1+unlist(lapply(sedpos,
                                FUN=function(x) which.min(abs(Grid$x.mid-x))))
    Temp.sed <- as.matrix(out[,ised])
    
    colnames(Temp.sed) <- paste("Tsed", sedpos, sep = "_")
    out <- cbind(out, Temp.sed)
    
    attributes (out) <- c(attributes(out),ATT)
  }
  
  if (verbose) print(ZZ)
  
  # ----------------------
  # Attributes and settings
  # ----------------------
  
  lengthvar        <- c(N, rep(1, times = length(outnames)))
  names(lengthvar) <- c(svar, outnames)
  
  # parameter description
  attributes(out)$parms <- 
    data.frame(names       = names(.TMPSED$parms), 
               values      = PP, 
               default     = .TMPSED$parms,
               units       = .TMPSED$parms_units, 
               description = .TMPSED$parms_description, 
               row.names   = NULL)
  
  attributes(out)$grid.z   <- Grid
  attributes(out)$dx       <- Grid$dx
  attributes(out)$seddepth <- Grid$x.mid
  
  attributes(out)$porosity   <- porosity
  attributes(out)$irrigation <- irr
  
  attributes(out)$var1D      <- NULL  # No 1D output variables
  attributes(out)$lengthvar  <- lengthvar
  
  # variable description
  attributes(out)$variables <- data.frame(
    names       = c(.TMPSED$y_names, .TMPSED$out0D_names),
    units       = c(.TMPSED$y_units, .TMPSED$out0D_units),
    description = c(.TMPSED$y_description, .TMPSED$out0D_description),
    row.names   = NULL)
  attributes(out)$units <- c(rep("degC", times = 100), 
                             attributes(out)$variables[-1,]$units) 
  attributes(out)$model <- "TempSED1D"
  
  class(out) <- c("TempSEDdyn", class(out))
  return(out)
}

