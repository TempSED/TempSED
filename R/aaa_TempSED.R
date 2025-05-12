## ====================================================================
## A local environment for non user-visible data,
## ====================================================================
.TMPSED <- new.env()

.TMPSED$N     <- 100

##------------------------------------
## Parameters
##------------------------------------

.TMPSED$parms <- c(
  S               = 35,    # [-]        salinity of water
  em_air          = 0.8,   # [-]        emissivity of air
  em_sediment     = 0.95,  # [-]        emissivity of bulk sediment
  albedo_water    = 0.05,  # [-] part   light refracted by water
  albedo_sediment = 0.15,  # [-] part   light refracted by sediment*
  kd_water        = 1.0,   # [/m]       light attenuation coeff water (1-25/m in RT13)
  kd_sediment     = 1000,  # [/m]       light attenuation coeff (bulk) sediment
  cp_water        = 3994,  # [J/kg/K]   specific heat capacity water
  cp_solid        = 1000,  # [J/kg/K]   specific heat capacity solid (dry sediment)
  tc_water        = 0.6,   # [W/m/K]    thermal conductivity of water 
  tc_solid        = 2.1,   # [W/m/K]    thermal conductivity of solid* 
  density_water   = 1024,  # [kg/m3]    seawater density
  density_solid   = 2500,  # [kg/m3]    sediment dry density
  stanton         = 0.001, # [-]        transfer coeff for sensible heat
  dalton          = 0.0014)# [-]        transfer coeff for latent heat


.TMPSED$parms_units <- c(
  S                 = "-", 
  em_air            = "-",
  em_sediment       = "-",
  albedo_water      = "-",
  albedo_sediment   = "-",
  kd_water          = "/m",
  kd_sediment       = "/m",
  cp_water          = "J/kg/K",
  cp_solid          = "J/kg/K",
  tc_water          = "W/m/K",
  tc_solid          = "W/m/K",
  density_water     = "kg/m3",
  density_solid     = "kg/m3",
  stanton           = "-",
  dalton            = "-"
)

.TMPSED$parms_description <- c(
  S               = "Salinity of water and bulk sediment",
  em_air          = "Emissivity of air",
  em_sediment     = "Emissivity of bulk sediment",
  albedo_water    = "Part light refracted by water",
  albedo_sediment = "Part light refracted by sediment",
  kd_water        = "Light attenuation coefficient water",
  kd_sediment     = "Light attenuation coefficient (bulk) sediment",
  cp_water        = "Specific heat capacity water",
  cp_solid        = "Specific heat capacity solid sediment",
  tc_water        = "Thermal conductivity of water",
  tc_solid        = "Thermal conductivity of solid",
  density_water   = "Reference seawater density",
  density_solid   = "Sediment dry density",
  stanton         = "Transfer coeff for sensible heat",
  dalton          = "Transfer coeff for latent heat"
)

##------------------------------------
## State variables
##------------------------------------

.TMPSED$y_names <- c("Temperature")
.TMPSED$y_units <- c("degC")
.TMPSED$y_description <- c("Temperature of bulk sediment")

.TMPSED$ynamesall <- as.vector(
  sapply(.TMPSED$y_names, 
         FUN = function(x) rep(x, times = .TMPSED$N)))

##------------------------------------
## 0-D Variables
##------------------------------------

.TMPSED$out0D_names <- c(
  "Watertemperature", "Waterheight",
  "Airtemperature",  "Airhumidity", "Pressure", "Solarradiation",
  "Windspeed", "RadiationSWI", "Evaporationrate", "Heatflux_air",
  "Heatflux_total", "Heatflux_convection", 
  "Heatflux_radiation", "Heatflux_latent",
  "Heatflux_sensible", "Heatflux_backrad", "Heatflux_deep", 
  "Total_irrigation", "Tsed_mean" )


.TMPSED$out0D_units <- c(
  Watertemperature    = "degC", 
  Waterheight         = "m",
  Airtemperature      = "degC", 
  Airhumidity         = "-",
  Pressure            = "Pa", 
  Solarradiation      = "W/m2",
  Windspeed           = "m/s", 
  RadiationSWI        = "W/m2", 
  EvaporationRate     = "kg/m2/s", 
  Heatflux_air        = "W/m2",
  Heatflux_total      = "W/m2",
  Heatflux_convection = "W/m2", 
  Heatflux_radiation  = "W/m2", 
  Heatflux_latent     = "W/m2",
  Heatflux_sensible   = "W/m2", 
  Heatflux_backrad    = "W/m2", 
  Heatflux_deep       = "W/m2",
  Total_irrigation    = "m3.dgC/s",
  Tsed_mean           = "degC")

.TMPSED$out0D_description <- c( 
  Watertemperature    = "Temperature of overlying water",
  Waterheight         = "Height of overlying water",
  Airtemperature      = "Temperature of air",
  Qrel                = "Relative humidity of air",
  Pressure            = "Air pressure",
  Solarradiation      = "Solar radiation (shortwave)",
  Windspeed           = "Wind speed",
  RadiationSWI        = "Solar radiation at sediment surface",
  Evaporationrate     = "Rate of evaporation",
  Heatflux_air        = "Total heat flux at interface with air",
  Heatflux_total      = "Total heat added (upper boundary+radiation)",
  Heatflux_convection = "Convective heat flux with overlying water ",
  Heatflux_radiation  = "Shortwave radiation heat input",
  Heatflux_latent     = "latent heat input (~evaporation)",
  Heatflux_sensible   = "sensible heat input (~air temperature)",
  Heatflux_backrad    = "net longwave radiation (net backradation)",
  Heatflux_deep       = "Deep heat flux out of the domain (lower boundary)",
  Total_irrigation    = "temperature input due to irrigation",
  Tsed_mean           = "Mean sediment temperature (depth-averaged")

##------------------------------------
## forcing functions 
##------------------------------------

.TMPSED$forc_names <- c("f_Waterheight",    "f_Watertemperature", 
                        "f_Airtemperature", "f_Qrel",
                        "f_Pressure",      "f_Solarradiation",    
                        "f_Windspeed" ,    "f_Cloudiness")

.TMPSED$forc_units <- c("m", "degC", "degC", "-",
                        "Pa", "W/m2", "m/s", "-")

.TMPSED$forc_description <- c( "Height of overlying water", 
                               "Temperature of overlying water", 
                               "Temperature of the air",
                               "Air humidity (fraction of saturation)", 
                               "Air pressure",
                               "Solar radiation",
                               "Wind speed (10 m.a.b.)", 
                               "Cloud cover"
)

##------------------------------------
## no 1D variables
##------------------------------------
.TMPSED$out1D_names <- .TMPSED$out1D_units <- .TMPSED$out1D_description <- NULL      

.TMPSED$outnames <- c(
  .TMPSED$out0D_names,                 # 0D first
  .TMPSED$forc_names                   # forcings
) 
.TMPSED$outunits <- c(
  as.vector(sapply(.TMPSED$y_units,    # state variables  
                   FUN = function(x) rep(x, times = .TMPSED$N))),
  .TMPSED$out0D_units,                 # 0D first
  .TMPSED$forc_units                   # forcings
) 

.TMPSED$nout     <- length(.TMPSED$outnames)
