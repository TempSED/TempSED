\name{heat fluxes}
\alias{flux_heat}
\alias{flux_latent}
\alias{flux_sensible}
\alias{flux_backradiation}
\alias{flux_evaporation}
\title{
  Heat fluxes between the air and sediment. Fluxes are positive when the heat transport is in the direction from air into sediment.
}
\description{
  \code{flux_heat} calculates the total heat flux across the air-sediment interface and its different contributions (see below).

  \code{flux_latent} calculates the heat flux due to evaporation.

  \code{flux_evaporation} calculates the evaporation flux.
  
  \code{flux_sensible} calculates the heat flux due to conduction.

  \code{flux_backradiation} calculates the net heat flux due to net long-wave back-radation.
}

 
\usage{
flux_heat (T_air = 10, T_sed = 11, P = 101325, Wind = 0,
             Cloudiness = 0.5, Qrel = 0.01, 
             em_air = 0.8, em_sed = 0.95, stanton = 0.001, dalton = 0.0014, 
             por0 = 1,
             NLR = c("basic", "may86", "josey97", "bunker76", "bignami95"))                               

flux_latent (T_air = 10, T_sed = 11, P = 101325,
             Wind = 0, Qrel = 0.01, dalton = 0.0014, por0 = 1)
                
flux_evaporation(T_air = 10, T_sed = 11, P = 101325,
             Wind = 0, Qrel = 0.01, dalton = 0.0014, por0 = 1)

flux_sensible (T_air = 10, T_sed = 11, P = 101325,
             Wind = 0, Qrel = 0.01, stanton = 0.001)

flux_backradiation (T_air = 10, T_sed = 11, P = 101325, 
             Cloudiness = 0.5, Qrel = 0.01, em_air = 0.8, em_sed   = 0.95,
             NLR = c("basic", "may86", "josey97", "bunker76", "bignami95"))
}
\arguments{
 \item{T_air}{ The temperature of the air, one number or a vector, units [degC].
  }
 \item{T_sed}{ The temperature of the sediment near the surface, one number or a vector, units [degC].
  }
 \item{P}{ The air pressure, one number or a vector, units [Pa].
  }
 \item{Qrel}{ The *relative* air humidity, ratio of the actual vapor pressure of water in the air to that in air saturated with water vapor, one number or a vector, units [-]; values should be inbetween 0 and 1 (not checked).
  }  
 \item{Wind}{  The velocity of the wind, estimated 10m above the surface, one number or a vector, units [m/s].
  }  
 \item{por0 }{ The volumetric water content (volume water/volume bulk) of sediment near the surface. One number or a vector, units [-].
  }
 \item{em_air, em_sed}{ Emissivity of Air (em_air) and  emissivity of bulk sediment; unitless, one value.
 }
 \item{stanton, dalton}{ Transfer coeff for sensible heat (stanton) and for latent heat (dalton); unitless, one value.
 }
 \item{Cloudiness}{ The fraction of the sky covered by clouds; one number or a vector, units [-], values should be inbetween 0  and 1 (not checked).
  }
 \item{NLR}{ How the backradiation should be estimated; a value between 1 and 5 or one of "basic" (= 1: Bolzmann), "may86" (=2), "josey97" (=3), "bunker76" (=4), "bignami95" (=5); see details.
  }
}

\value{
function \code{flux_heat} returns a list with:
 \itemize{
  \item \code{evaporation}, the rate of evaporation, [kg/m2/s]
  \item \code{latent_heat}, the LATENT heat flux (due to evaporation), [W/m2]
  \item \code{sensible_heat}, the SENSIBLE heat flux (due to conduction), [W/m2]
  \item \code{backradiation}, the Net Longwave Radiation, [W/m2] 
  \item \code{total}, latent_heat + sensible_heat + backradiation, [W/m2]
 } 

function \code{flux_evaporation} returns :
 \itemize{
  \item the rate of evaporation, [kg/m2/s]
 } 

function \code{flux_latent} returns :
 \itemize{
  \item the LATENT heat flux (due to evaporation), [W/m2]
 } 

function \code{flux_sensible} returns:
 \itemize{
  \item the SENSIBLE heat flux (due to conduction), [W/m2]
 } 

function \code{flux_backradiation} returns:
 \itemize{
  \item the Net Longwave Radiation, [W/m2] 
 } 

The object attributes contain:

\itemize{
 \item A \code{description} of these variables, including their units
 \item the input \code{parameter} settings 
}

use \code{attibutes(object)$description} and \code{attibutes(object)$parameters} to extract it. 
}

\author{
  Karline Soetaert
}

\details{
See vignette("TempSED") for a description of the formulations used.
}

\note{
Latent heat is the heat lost when the fluid evaporates.

Sensible heat is the heat required to change the temperature without changing the phase.

Backradiation is the amount of heat radiated from the atmosphere to the surface.
}


\examples{

# ---------------------------------------------------------
# The defaults
# ---------------------------------------------------------

flux_sensible()
attributes(flux_sensible())  # Metadata

flux_latent()
attributes(flux_latent())

flux_backradiation()
attributes(flux_backradiation())

# ---------------------------------------------------------
# One value
# ---------------------------------------------------------

flux_sensible      (T_air = 20)
flux_latent        (T_air = 20)
flux_backradiation (T_air = 20)

# ---------------------------------------------------------
# Sequences of values
# ---------------------------------------------------------

# temperature, wind and relative humidity sequences
t.seq   <- seq(from = 0, to = 20, length.out = 100)
W.seq   <- seq(from = 0, to = 20, length.out = 100)
Q.seq   <- seq(from = 0, to = 1,  length.out = 100)

# sensible heat flux

sH.t.seq <- flux_sensible(T_air = t.seq)
sH.Q.seq <- flux_sensible(Qrel  = Q.seq)
sH.W.seq <- flux_sensible(Wind  = W.seq)

# latent heat flux
lH.t.seq <- flux_latent(T_air = t.seq)
lH.Q.seq <- flux_latent(Qrel  = Q.seq)
lH.W.seq <- flux_latent(Wind  = W.seq)

# Show results

par(mfrow = c(2,3), las = 1)

plot(t.seq, sH.t.seq,     
     xlab = "Temp, degC", ylab = "W/m2", 
     main = "sensible heat flux")

plot(Q.seq, sH.Q.seq, 
     xlab = "rel humidity, -", ylab = "W/m2", 
     main = "sensible heat flux")

plot(W.seq, sH.W.seq, 
     xlab = "Wind speed, m/s", ylab = "W/m2", 
     main = "sensible heat flux")

plot(t.seq, lH.t.seq,     
     xlab = "Temp, degC", ylab = "W/m2", 
     main = "latent heat flux")

plot(Q.seq, lH.Q.seq, 
      xlab = "rel humidity, -", ylab = "W/m2", 
      main = "latent heat flux")

plot(W.seq, lH.W.seq, 
      xlab = "Wind speed, m/s", ylab = "W/m2", 
      main = "latent heat flux")

# Backradiation

BR.t.seq <- flux_backradiation(T_air = t.seq)

# show differences between the different formulations!

c(flux_backradiation(NLR = 1), flux_backradiation(NLR = 2), 
  flux_backradiation(NLR = 3), flux_backradiation(NLR = 4), 
  flux_backradiation(NLR = 5)) 

# ---------------------------------------------------------
# All air-sediment exchange fluxes
# ---------------------------------------------------------

# The default:
flux_heat()
attributes(flux_heat())

# One value
flux_heat(T_air = 20)

# a sequence of temperature values
SS <- sapply(X   = t.seq, 
             FUN = function(T) flux_heat(T_air = T))

SS <- rbind(temperature = t.seq, SS)
SS[,1:3]
}
\references{

Bunker, A. F., 1976. 
Computations of surface energy flux and annual air-sea interaction cycles of the North Atlantic Ocean, 
Mon. Weather Rev., 104, 1122--1140.

Bignami, F., S. Marullo, R. Santoleri, and M.E. Schiano, 1995. 
Longwave radiation budget in the Mediterranean Sea, 
J. Geophys. Res., 100(C2), 2501--2514.

Clark, N.E., L. Eber, R.M. Laurs, J.A. Renner, and J.F.T. Saur, 1974. 
Heat exchange between ocean and atmosphere in the eastern North Pacific for 1961-71, 
NOAA Tech. Rep. NMFS SSRF-682, U.S. Dep. of Commer., Washington, D. C., 1974.

Josey, S.A., D. Oakley, and R.W. Pascal, 1997. 
On estimating the atmospheric longwave flux at the ocean surface from ship meteorological reports. 
J. Geophys. Res., 102(C13),  27961--27972.

Onken, R., H. Garbe, S. Schroder, and M. Janik, 2010. 
A new instrument for sediment temperature measurements. 
J Mar Sci Technol 15:427--433, DOI 10.1007/s00773-010-0096-8

J.P. Rinehimer and J.T. Thomson, 2014. Observations and modeling of heat fluxes on tidal flats. 
J. Geophys. Res.: oceans, 119, 133--146, doi:10.1002/2013JC009225

T.-W. Kim,1 Yang-Ki Cho and E.P. Dever, 2007. 
An evaluation of the thermal properties and albedo of a macrotidal flat. 
J. Geophys. Res., 112 (C12009), doi:10.1029/2006JC004015.
}
\seealso{
  \code{\link{air_properties}}.
  \code{\link{water_properties}}.
  \code{\link{mineral_properties}}.
  \code{\link{bulk_properties}}.
}

\keyword{ utilities }
