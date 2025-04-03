
#===============================================================================
#===============================================================================
# Water properties
#===============================================================================
#===============================================================================

CheckWater <- function(val, name, n){
    if (is.null(val)) stop(name, "cannot be NULL")
    if (n == 0) stop("length cannot be 0")
    val <- unlist(val)
    if (length(val) == 1 & n >1) 
      val <- rep(val, times = n)
    else if (length(val) != n) 
      stop ("length of ", name, " should be equal to ", n)
    as.double(unlist(val))
}

Wdesc <- function(names){
 Bd <- data.frame(
  names       = c("density_water", "cp_water",  
                  "td_water", "tc_water", "lh_water"),
  description = c("density of water", "specific heat capacity of water", 
                  "thermal diffusivity of water", "thermal conductivity of water",
                  "latent heat of vaporization of water"),
  units       = c("kg/m3",  "J/kg/dg",  "m2/s", "W/m/dg", "J/kg" ))
  Bd[Bd$names %in% names,]
}

Wpars <- function(names){
  Bp <- data.frame(
    names = c("T_water", "S", "P", "density_water", "cp_water"), 
    mean.values =NA,
    description = c("water temperature", "S", "pressure", 
                    "density of water", "specific heat capacity of water"),
    units = c("dgC", "-", "Pa", "kg/m3", "J/kg/dg"))
  Bp[Bp$names %in% names,]
}

#===============================================================================
# Specific heat capacity, thermal diffusivity, density of water
#===============================================================================

water_properties <- function(T_water = 20, 
                             S = 30, 
                             P = 101325, 
                             type = 1){
  
  if (type > 3 | type < 1) 
    stop ("formulation `type` for conductivity should be inbetween 1 and 3")
  
  CP   <- water_cp(T_water = T_water, S = S, P = P)
  DENS <- water_density(T_water = T_water, S = S, P = P)
  TD   <- water_td(T_water = T_water, S = S, P = P, 
                   density_water = DENS, cp_water  = CP)
  TC   <- water_tc(T_water = T_water, S = S, P = P, type = type)
  LH   <- water_lh(T_water = T_water, S = S, P = P)
  RR <- data.frame(cp_water = CP, density_water = DENS, 
                   td_water = TD, tc_water = TC, lh_water = LH)
  
  # attributes
  BP <- Wpars(c("T_water", "S", "P"))
  BP$mean.values <- c(mean(T_water), S, P)
  
  attributes(RR)$description <- Wdesc(c("cp_water", "density_water", 
                                        "td_water", "tc_water", "lh_water"))
  attributes(RR)$parameters <- BP
  
  return(RR)
}

#===============================================================================
# Specific heat capacity of water
#===============================================================================

water_cp <- function(T_water = 20, S = 30, P = 101325){
  
  nx        <- length(T_water)
  T_water <- as.double(T_water)
  
  S  <- CheckWater(S, "S", 1)
  
  # note: pressure is not taken into account here
  P         <- CheckWater(P, "P", 1)
  
  BB <- .Fortran("calccpwater", as.integer(nx), T_water, S, # P, 
                 cp_water=as.double(rep(1., times = nx)))
  RR <- BB[["cp_water"]]


  # attributes
  BP <- Wpars(c("T_water", "S", "P"))
  BP$mean.values <- c(mean(T_water), S, P)

  attributes(RR)$description <- Wdesc("cp_water")
  attributes(RR)$parameters <- BP

  return(RR)
}

#===============================================================================
# thermal conductivity of water
#===============================================================================

water_tc <- function(T_water = 20, S = 30, P = 101325, type = 1){
  
  if (type > 3 | type < 1) 
    stop ("formulation `type` for conductivity should be inbetween 1 and 3")
  
  type      <- as.integer(type)
  nx        <- length(T_water)
  T_water <- as.double(T_water)

  S  <- CheckWater(S, "S", 1)
  P  <- CheckWater(P, "P", 1)
  
  BB <- .Fortran("calctcwater", as.integer(nx), type, 
                            as.double(T_water), S, P, 
                            tc_water = as.double(rep(1., times = nx)))
  RR <- BB[["tc_water"]]
  
  
  # attributes
  BP <- Wpars(c("T_water", "S", "P"))
  BP$mean.values <- c(mean(T_water), S, P)
  
  attributes(RR)$description <- Wdesc("tc_water")
  attributes(RR)$parameters <- BP
  
  return(RR)
}

#===============================================================================
# thermal diffusivity of water
#===============================================================================

water_td <- function(T_water = 20, S = 30, P = 101325, 
                    density_water = water_density(T_water, S, P), 
                    cp_water      = water_cp     (T_water, S, P)) {
  
  type      <- as.integer(1)
  prop      <- data.frame(tempWat = T_water, 
                          densWat = density_water, 
                          cpWat   = cp_water)

  nx        <- nrow(prop)
  if (is.null(nx)) nx <- 1

  S  <- CheckWater(S, "S", 1)
  P  <- CheckWater(P, "P", 1)
  
  BB <- with(prop, .Fortran("calctdwater", as.integer(nx), type, 
                            as.double(tempWat), S, P, as.double(cpWat), as.double(densWat), 
                            td_water = as.double(rep(1., times = nx))))
  RR <- BB[["td_water"]]
  
  
  # attributes
  BP <- Wpars(c("T_water", "S", "P", "density_water", "cp_water"))
  BP$mean.values <- c(mean(T_water), S, P, mean(density_water), mean(cp_water))
  
  attributes(RR)$description <- Wdesc("td_water")
  attributes(RR)$parameters <- BP
  
  return(RR)
}

#===============================================================================
# density of water
#===============================================================================

water_density <- function(T_water = 20, S = 30, P = 101325) {
  
  nx        <- length(T_water)
  T_water <- as.double(T_water)
  
  S  <- CheckWater(S, "S", 1)
  
  # note: pressure is not taken into account here
  P         <- CheckWater(P, "P", 1)
  
  BB <- .Fortran("calcrhowater", as.integer(nx), T_water, S, # P, 
                 density_water = as.double(rep(1., times = nx)))
  RR <- BB[["density_water"]]
  
  
  # attributes
  BP <- Wpars(c("T_water", "S", "P"))
  BP$mean.values <- c(mean(T_water), S, P)
  
  attributes(RR)$description <- Wdesc("density_water")
  attributes(RR)$parameters <- BP
  
  return(RR)
}

#===============================================================================
# Latent heat of vaporisation
#===============================================================================

water_lh <- function(T_water = 20, S = 30, P = 101325){
  
  nx        <- length(T_water)
  T_water <- as.double(T_water)
  
  S  <- CheckWater(S, "S", 1)
  
  # note: pressure is not taken into account here
  # P         <- CheckWater(P, "P", 1)
  
  BB <- .Fortran("calclhwater", as.integer(nx), T_water, S, # P, 
                 lh_water = as.double(rep(1., times = nx)))
  RR <- BB[["lh_water"]]
  
  
  # attributes
  BP <- Wpars(c("T_water", "S", "P"))
  BP$mean.values <- c(mean(T_water), S, P)
  
  attributes(RR)$description <- Wdesc("lh_water")
  attributes(RR)$parameters <- BP
  
  return(RR)
}

