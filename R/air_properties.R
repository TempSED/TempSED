
#===============================================================================
# Air humidity, density, vapor pressure
#===============================================================================

air_properties <- function(
    T_air   = 10,       # [degC]    air temperature
    P       = 101325,   # [Pa]      air pressure,
    Qrel    = 0.01)     # [-]       relative air humidity (0-1)
{
  
  # Create a data.frame to ensure that all inputs have the same length
  ZZ <- data.frame(T_air, P, Qrel)
  
  # Call fortran function 
  if (nrow(ZZ) > 1){
    
    RR <- sapply(1:nrow(ZZ), 
                 FUN = function(i){
                   RES <- with(ZZ, 
                               .Fortran("airproperties", as.double(T_air[i]), as.double(P[i]), 
                                        as.double(Qrel[i]), 
                                        Qair = as.double(1.), 
                                        RHO = as.double(1.), 
                                        Pvapor = as.double(1)))
                   c(Qspec = RES$Qair, density = RES$RHO, Pvapor = RES$Pvapor)
                 })
    RR <- as.data.frame(t(RR))
    
  } else {  
    
    RES <- .Fortran("airproperties", as.double(T_air), as.double(P), 
                    as.double(Qrel), 
                    Qair = as.double(1.), RHO = as.double(1.), Pvapor = as.double(1))
    RR <- list(Qspec = RES$Qair, density = RES$RHO, Pvapor = RES$Pvapor)
  } 
  
  R2 <- air_thermal(T_air = ZZ$T_air)
  R2$td_air = R2$tc / R2$cp/RR$density
  
  R2att <- attributes(R2)$description
  RR <- data.frame(RR, R2)
  
  attributes(RR)$description <- data.frame(
    names       = c("Qspec", 
                    "density", 
                    "Pvapor", 
                    R2att$names),
    description = c("specific humidity of the air", 
                    "air density", 
                    "partial vapor pressure of air", 
                    R2att$description),
    units = c("kg/kg", "kg/m3", "Pa", 
              R2att$units)
  )
  
  attributes(RR)$parameters <- data.frame(
    names       = c("T_air", "P", "Qrel"), 
    mean.values = c(mean(T_air), mean(P), mean(Qrel)), 
    description = c("air temperature",  "air pressure", 
                    "relative air humidity"),
    units = c("degC", "Pa", "-")
  )
  
  RR
  
}

air_Qspec <-  function( # [kg/kg]   air specific humidity
  T_air   = 10,       # [degC]    air temperature
  P       = 101325,   # [Pa]      air pressure,
  Qrel    = 0.01)     # [-]       relative air humidity (0-1)
{
  AA <- air_properties(T_air = T_air, P = P, Qrel = Qrel)
  AH <- AA$Qspec
  attributes(AH)$description <- subset(attributes(AA)$description, 
                                       subset = names == "Qspec")
  attributes(AH)$parameters <- attributes(AA)$parameters 
  AH
}

air_density <- function( # [kg/m3]   air density
  T_air   = 10,        # [degC]    air temperature
  P       = 101325,    # [Pa]      air pressure,
  Qrel    = 0.01)      # [-]       relative air humidity (0-1)
{
  AA <- air_properties(T_air = T_air, P = P, Qrel = Qrel)
  AD <- AA$density
  attributes(AD)$description <- subset(attributes(AA)$description, 
                                       subset = names == "density")
  attributes(AD)$parameters <- attributes(AA)$parameters 
  AD
}

air_Pvapor <-  function( # [Pa]      air partial vapor pressure
  T_air   = 10,        # [degC]    air temperature
  Qrel    = 0.01)      # [-]       relative air humidity (0-1)
{
  AA <- air_properties(T_air = T_air, P = 101325, Qrel = Qrel)
  AV <- AA$Pvapor
  attributes(AV)$description <- subset(attributes(AA)$description, 
                                       subset = names == "Pvapor")
  attributes(AV)$parameters <- subset(attributes(AA)$parameters, 
                                      subset = names != "P")
  AV
  
}

# Thermal properties of the air - from
# https://www.cambridge.org/core/books/abs/gas-turbines/equations-of-air-thermophysical-properties/9572106E068EFF1B7C0896124C17A196
#Zografos, Martin and Sunderland, 1987. Equations of properties as a function of temperature for seven fluids
# Comput. Methods Appl. Mech. Eng, 61: 177-187. (cp, td)

# Shitzer 2006. Wind-chill-equivalent temperatures: regarding the impact due to
# the variability of the environmental convective heat transfer coefficient. Int. J. Biometeorol 50, 224-232.


air_thermal <- function (T_air){
  TK <- T_air + 273.2 # T in Kelvin
  tc  = -3.06e-4 + 9.89089e-5*TK - 3.46571e-8*TK*TK
  cp  = 1061.332 - 0.432819*TK + 1.02344e-3*TK*TK - 6.47474e-7*TK^3 + 1.3846e-10*TK^4
  
  RES <- data.frame(cp_air = cp, tc_air = tc)
  attributes(RES)$description <- data.frame(
    names = c("cp_air", "td_air", "tc_air"),
    description = c("specific heat capacity of air", 
                    "thermal conductivity of air", 
                    "thermal diffusivity of air"),
    units = c("J/kg/K", "W/m/K", "m2/s"))
  attributes(RES)$parameters <- data.frame(names = "T_air", values = mean (T_air), description = "air temperature", units = "degC") 
  RES
}

air_tc <- function(     # conductivity, W/m/K
  T_air   = 10)       # [degC]     air temperature
{
  AA <- air_thermal(T_air = T_air)
  AH <- AA$tc_air
  attributes(AH)$description <- subset(attributes(AA)$description, 
                                       subset = names == "tc_air")
  attributes(AH)$parameters <- attributes(AA)$parameters 
  AH
}

air_td <- function(     # diffusivity, m2/s 
  T_air   = 10)       # [degC]     air temperature
{
  AA <- air_thermal(T_air = T_air)
  AH <- AA$td_air
  attributes(AH)$description <- subset(attributes(AA)$description, 
                                       subset = names == "td_air")
  attributes(AH)$parameters <- attributes(AA)$parameters 
  AH
}

air_cp <- function(     # heat capacity, J/kg/K
  T_air   = 10)       # [degC]     air temperature
{
  AA <- air_thermal(T_air = T_air)
  AH <- AA$cp_air
  attributes(AH)$description <- subset(attributes(AA)$description, 
                                       subset = names == "cp_air")
  attributes(AH)$parameters <- attributes(AA)$parameters 
  AH
}
