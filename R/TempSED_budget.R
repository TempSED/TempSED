
#===============================================================================
# Heat budget 
#===============================================================================

TempSED_budget <- function(out, ..., 
                           how = c("mean", "integral"),
                           remove_ini = FALSE){
  ALL  <- list(...)
  
  Budget <- TempSED_budget_one(out = out, how = how, remove_ini = remove_ini)
  if (length(ALL) >= 1){
    ATT <- attributes(Budget)[-1]
    for (i in 1:length(ALL)) {
      budg <- TempSED_budget_one(ALL[[i]], how = how, remove_ini = remove_ini)
      Budget <- rbind(Budget, budg)
    }
    attributes(Budget) <- c(attributes(Budget), ATT)
    na <- names(ALL)
    if (is.null(names(ALL)))
      na <- paste("extra_budget_", 1:length(ALL), sep="")
    rownames(Budget) <- c("out", na)
  }
  return(Budget)
}

TempSED_budget_one <- function(out, 
                           how = c("mean", "integral"),
                           remove_ini = FALSE){
  
  if (remove_ini){
    ATT  <- attributes(out)[-(1:2)]
    out  <- out[-1, ]
    attributes(out) <- c(attributes(out), ATT)
  }
  cumulativeHeat <- function(heat){
    flux <- out[ ,heat]   # value at start of each time interval
    
    # integral, taking into account unequal dt
    totflux <- sum(0.5*(flux[-1] + flux[-nrow(out)]) * diff(times))
    totflux
  }
  
  times      <- out[,1]
  
  Temp       <- subset(out, which = "Temperature")
  
  dx         <- TempSED_getdx(out)
  por        <- TempSED_getpor(out)
  
  parms      <- TempSED_getparms(as.vector = TRUE)
  cp_water   <- parms["cp_water"]
  cp_solid   <- parms["cp_solid"]
  dens_water <- parms["density_water"]
  dens_solid <- parms["density_solid"]
  
  dens_bulk  <- (dens_water*por + dens_solid*(1.-por))
  cp_bulk    <- (cp_water*dens_water *por    + 
                   cp_solid*dens_solid *(1.-por) )/dens_bulk
  
  Heat_accumulation  <- sum(dx*(Temp[nrow(Temp), ] - Temp[1, ])*cp_bulk *dens_bulk)
  
  Heatflux_total      <- cumulativeHeat("Heatflux_total")
  Heatflux_deep       <- cumulativeHeat("Heatflux_deep")
  Heatflux_convection <- cumulativeHeat("Heatflux_convection")
  Heatflux_radiation  <- cumulativeHeat("Heatflux_radiation") 
  Heatflux_latent     <- cumulativeHeat("Heatflux_latent")     
  Heatflux_sensible   <- cumulativeHeat("Heatflux_sensible")   
  Heatflux_backrad    <- cumulativeHeat("Heatflux_backrad")
  precision <- 100*(Heat_accumulation - (Heatflux_total - Heatflux_deep))/Heat_accumulation
  
  # the MEAN over the time interval or the integrated value
  How <- match.arg(how,  c("mean", "integral"))
  if (How == "mean") dT <- diff(range(times)) else dT <- 1
  RES <- c(Heat_accumulation = Heat_accumulation/dT, 
           Heatflux_total  = Heatflux_total/dT,
           Heatflux_deep     = Heatflux_deep/dT,
           Heatflux_convection = Heatflux_convection/dT,
           Heatflux_radiation = Heatflux_radiation/dT,
           Heatflux_latent = Heatflux_latent/dT,  
           Heatflux_sensible = Heatflux_sensible/dT,
           Heatflux_backrad = Heatflux_backrad/dT,
           precision = precision)

  attributes(RES)$title <- ifelse (How == "mean", 
                                   "average heat fluxes per time step",
                                   "integrated heat fluxes over time interval")
  
  attributes(RES)$description <- subset(TempSED_get0Dvars(),
                                        subset = names %in% names(RES),
                                        select = c("names", "units", "description"))
  attributes(RES)$description <- rbind(
    c(names = "Heat_accumulation",
      units = "W/m2",
      description = "Heat added to sediment"),
    attributes(RES)$description,
    c(names = "precision", 
      units = "%",
      description = "100*(Heat_accumulation-(Heatflux_total-Heatflux_deep))/Heat_accumulation"))
  if (How == "integral") 
    attributes(RES)$description$units <- paste("J/m2, [", dT, " sec]", sep="")
  
  RES
  
} 
