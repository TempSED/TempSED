\name{air properties}
\alias{air_properties}
\alias{air_density}
\alias{air_Qspec}
\alias{air_Pvapor}
\alias{air_tc}
\alias{air_cp}
\alias{air_td}

\title{
  Properties of the moist air relevant for sediment temperature modelling.
}
\description{
  \code{air_properties} calculates the specific humidity, density and vapor pressure of the moist air.
}

 
\usage{
air_properties (T_air = 10,  P = 101325, Qrel = 0.01)
air_density    (T_air = 10,  P = 101325, Qrel = 0.01)
air_Qspec      (T_air = 10,  P = 101325, Qrel = 0.01)
air_Pvapor     (T_air = 10,              Qrel = 0.01)
air_tc         (T_air = 10)
air_cp         (T_air = 10)
air_td         (T_air = 10)
}
\arguments{
 \item{T_air}{ The temperature of the air, one number or a vector, units [degC].
  }
 \item{P}{ The air pressure, one number or a vector, units [Pa].
  }
 \item{Qrel}{ The *relative* air humidity, ratio of the actual vapor pressure of water in the air to that in air saturated with water vapor, one number or a vector, units [-]; values should be inbetween 0 and 1 (not checked).
  }  
}

\value{
function \code{air_properties} returns a list with:
 \itemize{
  \item \code{Qspec}, the *specific* humidity of the moist air, mass of water vapor in a unit mass of moist air [kg/kg]
  \item \code{density}, the air density, [kg/m3]
  \item \code{Pvapor}, the vapor pressure of air, [Pa]
  \item \code{cp_air}, the specific heat capacity of air, [J/kg/K]
  \item \code{td_air}, the thermal conductivity of air,  [W/m/K]
  \item \code{tc_air}, the thermal diffusivity of air,    [m2/s]
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

\examples{

# ---------------------------------------------------------
# The defaults
# ---------------------------------------------------------

air_properties()

# parameter settings and description
attributes(air_properties())

# ---------------------------------------------------------
# One value
# ---------------------------------------------------------

air_properties(T_air = 20)
air_Qspec     (T_air = 20)

# ---------------------------------------------------------
# temperature, relative humidity, pressure sequences
# ---------------------------------------------------------

P.seq     <- seq(from = 101320, to = 101330, length.out = 100)
air.P.seq <- air_properties(P = P.seq)

head(air.P.seq)
attributes(air.P.seq)

# -------
t.seq     <- seq(from =  0, to = 20, length.out = 100)
air.t.seq <- air_properties(T_air = t.seq)
               
par(mfrow = c(2,3), las = 1)

plot(t.seq, air.t.seq$Qspec, 
     xlab = "Temp", ylab = "kg/kg", 
     main = "specific air humidity")

plot(t.seq, air.t.seq$density,  
     xlab = "Temp", ylab = "kg/m3", 
     main = "air density")

plot(t.seq, air.t.seq$Pvapor,  
     xlab = "Temp", ylab = "Pa", 
     main = "air vapor pressure")


# -------
Q.seq     <- seq(from =  0, to =  1, length.out = 100)

air.Q.seq <- air_properties(Qrel = Q.seq)

plot(Q.seq, air.Q.seq$Qspec, 
     xlab = "rel humidity", ylab = "kg/kg", 
     main = "air specific humidity")


plot(Q.seq, air.Q.seq$density,  
     xlab = "rel humidity", ylab = "kg/m3", 
     main = "air density")

plot(Q.seq, air.Q.seq$Pvapor,  
     xlab = "rel humidity", ylab = "Pa", 
     main = "air vapor pressure")

}
\references{
Goff, J. A., and Gratch, S. (1946) 
Low-pressure properties of water from -160 to 212 dgF, 
in Transactions of the American Society of Heating and Ventilating Engineers, pp 95-122
}
\seealso{
  \code{\link{water_properties}}.
  \code{\link{mineral_properties}}.
  \code{\link{bulk_properties}}.
  \code{\link{flux_heat}}.
}
\keyword{ utilities }
