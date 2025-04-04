
#===============================================================================
#===============================================================================
# BULK properties
#===============================================================================
#===============================================================================

CheckBulk <- function(val, name, n){
  if (is.null(val)) stop(name, "cannot be NULL")
  if (n == 0) stop("length cannot be 0")
  
  val <- unlist(val)
  if (length(val) == 1 & n >1) 
    val <- rep(val, times=n)
  else if (length(val) != n) 
    stop ("length of ", name, " should be equal to ", n)
  as.double(unlist(val))
}

Bdesc <- function(names){
  Bd <- data.frame(
    names       = c("density_bulk", "cp_bulk",     "td_bulk", "tc_bulk"),
    description = c("bulk density", "bulk specific heat capacity", 
                    "bulk thermal diffusivity", "bulk thermal conductivity"),
    units       = c("kg/m3",        "J/kg/K",      "m2/s", "W/m/K"))
  Bd[Bd$names %in% names,]
}

Bpars <- function(names){
  Bp <- data.frame(
    names = c("density_water", "density_solid", "cp_water", "cp_solid",                 
              "td_water", "td_solid", "tc_water", "tc_solid", "porosity"), 
    mean.values = NA,
    description = c("density of water", "density of solid",  
                    "specific heat capacity of water", "specific heat capacity of solid", 
                    "thermal diffusivity of water", "thermal diffusivity of solid", 
                    "thermal conductivity of water", "thermal conductivity of solid", 
                    "porosity"),
    units = c("kg/m3", "kg/m3", "J/kg/K", "J/kg/K", "m2/s", "m2/s", 
              "W/m/K", "W/m/K", "-"))
  Bp[Bp$names %in% names,]
}

#===============================================================================

bulk_density <- function(density_water = 1024, density_solid = 2500, porosity = 0.5){
  
  prop      <- data.frame(density_water = density_water, 
                          density_solid = density_solid)
  nprop     <- nrow(prop)
  nx        <- length(porosity)
  
  if (nx > 1 & nprop > 1) 
    stop("either porosity or one of the other parameters can be of length > 1")
  
  density_water <- CheckBulk(density_water, "density_water", nprop)
  density_solid <- CheckBulk(density_solid, "density_solid", nprop)
  
  porosity  <- CheckBulk(porosity,  "porosity", nx)
  
  if (nprop > 1){  # many values of properties, only one value of porosity
    RR <- sapply(1:nprop, FUN = function(i)
      .Fortran("calcbulkdensity", as.integer(nx), density_water[i], density_solid[i],                  
               porosity, 
               density_bulk = as.double(rep(1., times=nx)))[["density_bulk"]])
  } else  { 
    BB <- .Fortran("calcbulkdensity", as.integer(nx), density_water, density_solid, 
                   porosity, density_bulk = as.double(rep(1., times=nx)))
    RR <- BB[["density_bulk"]]
  } 
  # attributes
  BP <- Bpars(c("density_water", "density_solid", "porosity"))
  BP$mean.values <- c(mean(density_water), mean(density_solid), mean(porosity))
  
  attributes(RR)$description <- Bdesc("density_bulk")
  attributes(RR)$parameters <- BP
  
  return(RR)
}

#===============================================================================

bulk_tc <- function(tc_water = 0.6, tc_solid = 2.0, porosity = 0.5){
  
  prop      <- data.frame(tc_water = tc_water, tc_solid = tc_solid)
  nprop     <- nrow(prop)
  nx        <- length(porosity)
  
  if (nx > 1 & nprop > 1) 
    stop("either porosity or one of the other parameters can be of length > 1")
  
  tc_water <- CheckBulk(tc_water, "tc_water", nprop)
  tc_solid <- CheckBulk(tc_solid, "tc_solid", nprop)
  
  porosity <- CheckBulk(porosity,  "porosity", nx)
  
  if (nprop > 1){  # many values of properties, only one value of porosity
    RR <- sapply(1:nprop, FUN = function(i)
      .Fortran("calcbulktc", as.integer(nx), tc_water[i], tc_solid[i],                  
               porosity, 
               tc_bulk = as.double(rep(1., times = nx)))[["tc_bulk"]])
  } else  { 
    BB <- .Fortran("calcbulktc", as.integer(nx), tc_water, tc_solid, 
                   porosity, tc_bulk = as.double(rep(1., times = nx)))
    RR <- BB[["tc_bulk"]]
  } 
  # attributes
  BP <- Bpars(c("tc_water", "tc_solid", "porosity"))
  BP$mean.values <- c(mean(tc_water), mean(tc_solid), mean(porosity))
  
  attributes(RR)$description <- Bdesc("tc_bulk")
  attributes(RR)$parameters <- BP
  
  return(RR)
}

#===============================================================================

bulk_cp <- function(density_water = 1024, density_solid = 2500, 
                    cp_water = 3994, cp_solid = 1000, 
                    porosity = 0.5){
  
  prop      <- data.frame(density_water = density_water, 
                          density_solid = density_solid,
                          cp_water = cp_water, cp_solid = cp_solid)
  nprop     <- nrow(prop)
  nx        <- length(porosity)
  if (nx > 1 & nprop > 1) 
    stop("either porosity or one of the other parameters can be of length > 1")
  
  density_water <- CheckBulk(density_water, "density_water", nprop)
  density_solid <- CheckBulk(density_solid, "density_solid", nprop)
  cp_water   <- CheckBulk(cp_water,   "cp_water", nprop)
  cp_solid   <- CheckBulk(cp_solid,   "cp_solid", nprop)
  
  porosity  <- CheckBulk(porosity,  "porosity", nx)
  
  if (nprop > 1){  # many values of properties, only one value of porosity
    RR <- sapply(1:nprop, FUN = function(i)
      .Fortran("calcbulkcp", as.integer(nx), density_water[i], density_solid[i],                  
               cp_water[i], cp_solid[i], porosity, 
               cpBulk = as.double(rep(1., times = nx)))[["cpBulk"]])
  } else  { 
    BB <- .Fortran("calcbulkcp", as.integer(nx), density_water, density_solid, 
                   cp_water, cp_solid,                 
                   porosity, cpBulk = as.double(rep(1., times = nx)))
    RR <- BB[["cpBulk"]]
  }
  # attributes
  BP <- Bpars(c("density_water", "density_solid", 
                "cp_water", "cp_solid", "porosity"))
  
  BP$mean.values <- c(mean(density_water), mean(density_solid), 
                      mean(cp_water), mean(cp_solid),                 
                      mean(porosity))
  
  attributes(RR)$description <- Bdesc("cpBulk")
  attributes(RR)$parameters  <- BP
  
  return(RR)
}

#===============================================================================

bulk_td <- function(density_water = 1024, density_solid = 2500, 
                    cp_water = 3994, cp_solid = 1000, 
                    td_water = 1.4e-07, td_solid = 2e-06, porosity = 0.5) {
  
  prop      <- data.frame(density_water = density_water, 
                          density_solid = density_solid,
                          cp_water = cp_water, cp_solid = cp_solid,
                          td_water = td_water, td_solid = td_solid)
  prop <- apply(prop, MARGIN = 2, FUN = as.double)
  nprop     <- nrow(prop)
  nx        <- length(porosity)
  if (nx > 1 & nprop > 1) 
    stop("either porosity or one of the other parameters can be of length > 1")
  
  density_water <- CheckBulk(density_water, "density_water", nprop)
  density_solid <- CheckBulk(density_solid, "density_solid", nprop)
  cp_water      <- CheckBulk(cp_water,         "cp_water",   nprop)
  cp_solid      <- CheckBulk(cp_solid,         "cp_solid",   nprop)
  td_water      <- CheckBulk(td_water,         "td_water",   nprop)
  td_solid      <- CheckBulk(td_solid,         "td_solid",   nprop)
  
  porosity      <- CheckBulk(porosity,    "porosity",  nx)
  
  if (nprop > 1){  # many values of properties, only one value of porosity
    RR <- sapply(1:nprop, FUN=function(i)
      .Fortran("calcbulktd", as.integer(nx), density_water[i], density_solid[i],                  
               cp_water[i], cp_solid[i], td_water[i], td_solid[i], porosity, 
               td_bulk = as.double(rep(1., times = nx)))[["td_bulk"]])
  } else  { 
    BB <- .Fortran("calcbulktd", as.integer(nx), density_water, density_solid,                  
                   cp_water, cp_solid, td_water, td_solid, porosity, 
                   td_bulk = as.double(rep(1., times = nx)))
    RR <- BB[["td_bulk"]]
  }
  # attributes
  BP <- Bpars(c("density_water", "density_solid", "cp_water", "cp_solid",                 
                "td_water", "td_solid", "porosity"))
  BP$mean.values <- c(mean(density_water), mean(density_solid), 
                      mean(cp_water), mean(cp_solid),                 
                      mean(td_water), mean(td_solid), mean(porosity))
  
  attributes(RR)$description <- Bdesc("td_bulk")
  attributes(RR)$parameters <- BP
  
  return(RR)
}

#===============================================================================
# Heat flux between sediment (or water) and air
#===============================================================================

bulk_properties <- function(density_water = 1024, density_solid = 2500, 
                            cp_water = 3994, cp_solid = 1000, 
                            td_water = 1.4e-07, td_solid = 2e-06, 
                            tc_water = 0.6, tc_solid = 2.0, 
                            porosity = 0.5)
{
  
  prop      <- data.frame(density_water = density_water, 
                          density_solid = density_solid,
                          cp_water = cp_water, cp_solid = cp_solid,
                          td_water = td_water, td_solid = td_solid,
                          tc_water = tc_water, tc_solid = tc_solid)
  nprop     <- nrow(prop)
  nx        <- length(porosity)
  if (nx > 1 & nprop > 1) 
    stop("either porosity or one of the other parameters can be of length > 1")
  
  density_water <- CheckBulk(density_water, "density_water", nprop)
  density_solid <- CheckBulk(density_solid, "density_solid", nprop)
  cp_water      <- CheckBulk(cp_water,         "cp_water",   nprop)
  cp_solid      <- CheckBulk(cp_solid,         "cp_solid",   nprop)
  td_water      <- CheckBulk(td_water,         "td_water",   nprop)
  td_solid      <- CheckBulk(td_solid,         "td_solid",   nprop)
  tc_water      <- CheckBulk(tc_water,         "tc_water",   nprop)
  tc_solid      <- CheckBulk(tc_solid,         "tc_solid",   nprop)
  
  porosity      <- CheckBulk(porosity,         "porosity", nx)
  
  if (nprop > 1){  # many values of properties, only one value of porosity
    RR <- data.frame(sapply(1:nprop, FUN = function(i)
      .Fortran("calcbulkdensity", as.integer(nx), density_water[i], density_solid[i],                  
               porosity, 
               density_bulk = as.double(rep(1., times = nx)))[["density_bulk"]]))
    names(RR) <- "density_bulk"
    RR$cpBulk <- sapply(1:nprop, FUN = function(i)
      .Fortran("calcbulkcp", as.integer(nx), density_water[i], density_solid[i],                  
               cp_water[i], cp_solid[i], porosity, 
               cpBulk = as.double(rep(1., times = nx)))[["cpBulk"]])
    RR$td_bulk <- sapply(1:nprop, FUN = function(i)
      .Fortran("calcbulktd", as.integer(nx), density_water[i], density_solid[i],                  
               cp_water[i], cp_solid[i], td_water[i], td_solid[i], porosity, 
               td_bulk = as.double(rep(1., times = nx)))[["td_bulk"]])
    RR$tc_bulk <- sapply(1:nprop, FUN = function(i)
      .Fortran("calcbulktc", as.integer(nx), tc_water[i], tc_solid[i], porosity, 
               tc_bulk = as.double(rep(1., times = nx)))[["tc_bulk"]])
    
  } else  { 
    RR <- data.frame(
      density_bulk = .Fortran("calcbulkdensity", as.integer(nx), 
                              density_water, density_solid, porosity, 
                              density_bulk = as.double(rep(1., times = nx)))$density_bulk)
    
    RR$cp_bulk <- .Fortran("calcbulkcp", as.integer(nx), 
                           density_water, density_solid, cp_water, cp_solid,                 
                           porosity, cpBulk = as.double(rep(1., times = nx)))$cpBulk
    RR$td_bulk <- .Fortran("calcbulktd", as.integer(nx), 
                           density_water, density_solid,                  
                           cp_water, cp_solid, td_water, td_solid, porosity, 
                           td_bulk = as.double(rep(1., times = nx)))$td_bulk
    RR$tc_bulk <- .Fortran("calcbulktc", as.integer(nx), 
                           tc_water, tc_solid, porosity, 
                           tc_bulk = as.double(rep(1., times = nx)))$tc_bulk
  }
  # attributes
  BP <- Bpars(c("density_water", "density_solid", "cp_water", "cp_solid",                 
                "td_water", "td_solid", "tc_water", "tc_solid", "porosity"))
  BP$mean.values <- c(mean(density_water), mean(density_solid), 
                      mean(cp_water), mean(cp_solid),                 
                      mean(td_water), mean(td_solid), 
                      mean(tc_water), mean(tc_solid), 
                      mean(porosity))
  
  attributes(RR)$description <- Bdesc(c("density_bulk", "cp_bulk", "td_bulk", "tc_bulk"))
  attributes(RR)$parameters <- BP
  
  return(RR)
}
