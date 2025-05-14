
#===============================================================================
#===============================================================================
# HEAT FLUXES
#===============================================================================
#===============================================================================

#===============================================================================
# Heat flux between sediment (or water) and air
#===============================================================================

flux_heat <- function(T_air      = 10,     # [degC]    air temperature
                      T_sed      = 11,     # [degC]    temp sediment 
                      P          = 101325, # [Pa]      air pressure,
                      Wind       = 0,      # [m/s]     wind speed
                      Cloudiness = 0.5,    # [-]       relative fraction cloud cover
                      Qrel       = 0.01,   # [-]       relative air humidity (0-1)
                      em_air     = 0.8,    # [-]       emissivity of air
                      em_sed     = 0.95,   # [-]       emissivity of bulk sediment
                      stanton    = 0.001,  # [-]       transfer coeff for sensible heat
                      dalton     = 0.0014, # [-]       transfer coeff for latent heat
                      por0       = 1,      # [-]       volume fraction water at SWI interface
                      NLR = c("basic", "may86", "josey97", "bunker76", "bignami95"))                               
{
  NLRvals <- c("basic", "may86", "josey97", "bunker76", "bignami95") 
  if (! is.numeric(NLR))  NLR <- pmatch(match.arg(NLR),NLRvals)
  if (NLR > 5 | NLR < 1) stop("'NLR' should be a number between 1 and 5 or a name")
  
  ZZ <- data.frame(T_air, T_sed, P, Wind, Cloudiness, Qrel, 
                   em_air, em_sed, dalton, stanton, por0)
  
  if (nrow(ZZ) > 1){
    RR <- sapply(1:nrow(ZZ), FUN = function(i){
      BB <- with(ZZ, .Fortran("heatflux", 
                              as.double(T_air[i]), as.double(T_sed[i]), as.double(P[i]), 
                              as.double(Wind[i]), as.double(Cloudiness[i]), as.double(Qrel[i]), 
                              as.double(em_air[i]), as.double(em_sed[i]), 
                              as.double(dalton[i]), as.double(stanton[i]),
                              as.double(por0[i]), as.integer(NLR[i]), 
                              EvaporationRate = as.double(1.), Hlatent = as.double(1.), 
                              Hsensible = as.double(1.), BackRadiation = as.double(1.)))
      c(evaporation   = BB$EvaporationRate, 
        latent_heat   = BB$Hlatent, 
        sensible_heat = BB$Hsensible, 
        backradiation = BB$BackRadiation,
        total=BB$Hlatent + BB$Hsensible + BB$BackRadiation)
    })
    RR <- as.data.frame(t(RR))
  } else   { 
    RES <- .Fortran("heatflux", 
                    as.double(T_air), as.double(T_sed), as.double(P), 
                    as.double(Wind), as.double(Cloudiness), as.double(Qrel), 
                    as.double(em_air), as.double(em_sed), 
                    as.double(dalton), as.double(stanton),
                    as.double(por0), as.integer(NLR), 
                    EvaporationRate = as.double(1.), Hlatent = as.double(1.), 
                    Hsensible = as.double(1.), BackRadiation = as.double(1.))
    
    RR <- c(evaporation   = RES$EvaporationRate, 
            latent_heat   = RES$Hlatent, 
            sensible_heat = RES$Hsensible, 
            backradiation = RES$BackRadiation,
            total         = RES$Hlatent + RES$Hsensible + RES$BackRadiation)
  } 
  attributes(RR)$description = data.frame(
    names = c("evaporation", "latent_heat", "sensible_heat", 
              "backradiation", "total"),
    description = c("rate of evaporation", 
                    "LATENT heat flux (due to evaporation)",
                    "SENSIBLE heat flux (due to conduction)", 
                    "Net Longwave Radiation (backradiation)", 
                    "latentHeat+sensibleHeat+backradiation"),
    units = c("kg/m2/s", "W/m2", "W/m2", "W/m2", "W/m2"))
  
  attributes(RR)$parameters = data.frame(
    names = c("T_air", "T_sed", "P", "Wind", "Cloudiness", "Qrel", 
              "em_air", "em_sed", "dalton", "stanton", "por0"), 
    mean.values = c(mean(T_air), mean(T_sed), mean(P), 
                    mean(Wind), mean(Cloudiness), mean(Qrel), 
                    mean(em_air), mean(em_sed), 
                    mean(dalton), mean(stanton), mean(por0)), 
    description = c("air temperature", "sediment temperature", "air pressure", 
                    "wind speed", "relative fraction cloud cover",
                    "relative air humidity", 
                    "emissivity of air", "emissivity of bulk sediment",
                    "transfer coeff for latent heat", "transfer coeff for sensible heat", 
                    "surface sediment porosity"),
    units = c("degC", "degC", "Pa", "m/s", "-", "-", "-", "-", "-", "-", "-"))
  attributes(RR)$settings = data.frame(name = "NLR", 
                                       description = "backradiation formula",
                                       value = NLRvals[mean(NLR)])
  RR
}

#===============================================================================
# Heat flux due to evaporation
#===============================================================================

flux_latent <- function (
    T_air   = 10,       # [degC]  air temperature
    T_sed   = 11,       # [degC]  temp sediment 
    P       = 101325,   # [Pa]    air pressure,
    Wind    = 0,        # [m/s]   wind speed
    Qrel    = 0.01,     # [-]     relative air humidity (0-1)
    dalton  = 0.0014,     # [-]   transfer coeff for latent heat
    por0    = 1){
  
  AP <- air_properties (T_air = T_air, P = P, Qrel = Qrel)
  row.names(AP) <- NULL
  
  ZZ <- data.frame(T_air, T_sed, P, Wind, Qrel, dalton, por0, AP)
  if (nrow(ZZ) > 1){
    RR <- sapply(1:nrow(ZZ), FUN = function(i)
      BB <- with(ZZ,.Fortran("latentheat", as.double(T_sed[i]), as.double(P[i]), 
                             as.double(Wind[i]), as.double(por0[i]), 
                             Qspec=Qspec[i], density = density[i], dalton = as.double(dalton),
                             EvaporationRate = as.double(1.), Hlatent = as.double(1.))$Hlatent))
  } else   { 
    RR <- .Fortran("latentheat", as.double(T_sed), as.double(P), 
                   as.double(Wind), as.double(por0), 
                   Qspec = AP$Qspec, density = AP$density, dalton = as.double(dalton),
                   EvaporationRate = as.double(1.), Hlatent = as.double(1.))$Hlatent
  }
  attributes(RR)$description = data.frame(
    names = c("latent_heat"),
    description = c("LATENT heat flux (due to evaporation)"),
    units = c("W/m2"))
  
  attributes(RR)$parameters = data.frame(
    names       = c("T_air", "T_sed", "P", "Wind", "Qrel", " dalton", "por0"), 
    mean.values = c(mean(T_air), mean(T_sed), mean(P), mean(Wind), mean(Qrel), 
                    mean(dalton), mean(por0)), 
    description = c("air temperature", "sediment temperature", "air pressure", 
                    "wind speed", "relative air humidity", 
                    "transfer coeff for latent heat", "surface sediment porosity"),
    units = c("degC", "degC", "Pa", "m/s", "-", "-", "-"))
  RR
}

#===============================================================================
# Evaporation flux
#===============================================================================

flux_evaporation <- function (
    T_air   = 10,       # [degC]  air temperature
    T_sed   = 11,       # [degC]  temp sediment 
    P       = 101325,   # [Pa]    air pressure,
    Wind    = 0,        # [m/s]   wind speed
    Qrel    = 0.01,     # [-]     relative air humidity (0-1)
    dalton  = 0.0014,     # [-]   transfer coeff for latent heat
    por0    = 1){
  
  AP <- air_properties (T_air = T_air, P = P, Qrel = Qrel)
  row.names(AP) <- NULL
  
  ZZ <- data.frame(T_air, T_sed, P, Wind, Qrel, dalton, por0, AP)
  if (nrow(ZZ) > 1){
    RR <- sapply(1:nrow(ZZ), FUN = function(i)
      BB <- with(ZZ,.Fortran("latentheat", as.double(T_sed[i]), as.double(P[i]), 
                             as.double(Wind[i]), as.double(por0[i]), 
                             Qspec=Qspec[i], density = density[i], dalton = as.double(dalton),
                             EvaporationRate = as.double(1.), 
                             Hlatent = as.double(1.))$EvaporationRate))
  } else   { 
    RR <- .Fortran("latentheat", as.double(T_sed), as.double(P), 
                   as.double(Wind), as.double(por0), 
                   Qspec = AP$Qspec, density = AP$density, dalton = as.double(dalton),
                   EvaporationRate = as.double(1.), 
                   Hlatent = as.double(1.))$EvaporationRate
  }
  attributes(RR)$description = data.frame(
    names = c("evaporation"),
    description = c("rate of evaporation"),
    units = c("kg/m2/s"))
  
  attributes(RR)$parameters = data.frame(
    names       = c("T_air", "T_sed", "P", "Wind", "Qrel", " dalton", "por0"), 
    mean.values = c(mean(T_air), mean(T_sed), mean(P), mean(Wind), mean(Qrel), 
                    mean(dalton), mean(por0)), 
    description = c("air temperature", "sediment temperature", "air pressure", 
                    "wind speed", "relative air humidity", 
                    "transfer coeff for latent heat", "surface sediment porosity"),
    units = c("degC", "degC", "Pa", "m/s", "-", "-", "-"))
  RR
}

#===============================================================================
# Heat flux due to conduction
#===============================================================================

flux_sensible <- function (
    T_air   = 10,       # [degC]    air temperature
    T_sed   = 11,       # [degC]    temp sediment 
    P       = 101325,   # [Pa]      air pressure,
    Wind    = 0,        # [m/s]     wind speed
    Qrel    = 0.01,     # [-]       relative air humidity (0-1)
    stanton = 0.001     # [-]       transfer coeff for sensible heat
){    
  
  AP <- air_properties (T_air = T_air, P = P, Qrel = Qrel)
  row.names(AP) <- NULL
  
  ZZ <- data.frame(T_air, T_sed, P, Wind, Qrel, stanton, AP)
  if (nrow(ZZ) > 1){
    RR <- sapply(1:nrow(ZZ), FUN = function(i)
      BB <- with(ZZ, .Fortran("sensibleheat", as.double(T_air[i]), as.double(T_sed[i]), 
                              as.double(Wind[i]), 
                              Qspec = Qspec[i], density = density[i],
                              stanton = as.double(stanton[i]), 
                              Hsensible = as.double(1.))$Hsensible))
    
  } else   { 
    RR <- .Fortran("sensibleheat", as.double(T_air), as.double(T_sed), 
                   as.double(Wind), Qspec = AP$Qspec, density = AP$density, 
                   stanton = as.double(AP$stanton),
                   Hsensible = as.double(1.))$Hsensible
  }
  attributes(RR)$description = data.frame(
    names = c("sensible_heat"),
    description = c("SENSIBLE heat flux (due to conduction)"),
    units = c("W/m2"))
  
  attributes(RR)$parameters=data.frame(
    names = c("T_air", "T_sed", "P", "Wind", "Qrel", "stanton"), 
    mean.values = c(mean(T_air), mean(T_sed), mean(P), mean(Wind), 
                    mean(Qrel), mean(stanton)), 
    description = c("air temperature", "sediment temperature", "air pressure", 
                    "wind speed", "relative air humidity", 
                    "transfer coeff for sensible heat"),
    units = c("degC", "degC", "Pa", "m/s", "-", "-"))
  
  RR
  
}

#===============================================================================
# Heat flux due to net longwave radiation
#===============================================================================

flux_backradiation <- function (
    T_air   = 10,       # [degC]    air temperature
    T_sed   = 11,       # [degC]    temp sediment 
    P       = 101325,   # [Pa]      air pressure,
    Cloudiness = 0.5,   # [-]       relative fraction cloud cover
    Qrel    = 0.01,     # [-]       relative air humidity (0-1)
    em_air  = 0.8,      # [-]       emissivity of air
    em_sed  = 0.95,     # [-]       emissivity of bulk sediment
    NLR     = c("basic", "may86", "josey97", 
                "bunker76", "bignami95")){
  
  NLRvals <- c("basic", "may86", "josey97", "bunker76", "bignami95") 
  if (! is.numeric(NLR))  NLR <- pmatch(match.arg(NLR),NLRvals)
  if (NLR > 5 | NLR < 1) stop("'NLR' should be a number between 1 and 5 or a name")
  
  AP <- air_properties (T_air = T_air, P = P, Qrel = Qrel)
  row.names(AP) <- NULL
  
  nlr <- as.integer(NLR)
  
  ZZ <- data.frame(T_air, T_sed, P, Cloudiness, Qrel, nlr, em_air, em_sed, AP)
  if (nrow(ZZ) > 1){
    RR <- sapply(1:nrow(ZZ), FUN = function(i)
      BB <- with(ZZ, .Fortran("backrad", as.double(T_air[i]), as.double(T_sed[i]), 
                              as.double(Cloudiness[i]), Vapor = Pvapor[i], 
                              em_air = as.double(em_air[i]), em_sed = as.double(em_sed[i]),
                              nlr[i], Hback = as.double(1.))$Hback))
    
  } else   { 
    RR <- .Fortran("backrad", as.double(T_air), as.double(T_sed), 
                   as.double(Cloudiness), 
                   Pvapor = AP$Pvapor, as.double(em_air), as.double(em_sed), 
                   nlr, Hback = as.double(1.))$Hback
  }
  attributes(RR)$description = data.frame(
    names = c("backradiation"),
    description = c("Net Longwave Radiation (backradiation)"),
    units = c("W/m2"))
  
  attributes(RR)$parameters = data.frame(
    names = c("T_air", "T_sed", "P", "Cloudiness", "Qrel", "em_air", "em_sed"), 
    mean.values = c(mean(T_air), mean(T_sed), mean(P), mean(Cloudiness), 
                    mean(Qrel), mean(em_air), mean(em_sed)), 
    description = c("air temperature", "sediment temperature", "air pressure", 
                    "relative fraction cloud cover", "relative air humidity", 
                    "emissivity of a
              heair", "emissivity of bulk sediment"),
    units = c("dgC", "dgC", "Pa", "-", "-", "-", "-"))
  attributes(RR)$settings = data.frame(
    name = "NLR", description = "backradiation formula",
    value = NLRvals[nlr])
  RR
  
}
