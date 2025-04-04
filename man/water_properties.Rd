\name{water properties}
\alias{water_properties}
\alias{water_density}
\alias{water_cp}
\alias{water_td}
\alias{water_tc}
\alias{water_lh}

\title{
  Properties of the water relevant for sediment temperature modelling.
}
\description{
  \code{water_density}, \code{water_cp}, \code{water_td} , \code{water_tc} 
  calculate the density, specific heat capacity, thermal diffusivity and
  thermal conductivity of water.
  
  \code{water_lh} estimates the latent heat of vaporization  
}

 
\usage{
water_properties(T_water = 20, S = 30, P = 101325, type = 1)

water_density (T_water = 20, S = 30, P = 101325)

water_cp      (T_water = 20, S = 30, P = 101325)

water_tc      (T_water = 20, S = 30, P = 101325, type = 1)

water_td (T_water = 20, S = 30, P = 101325,
          density_water = water_density(T_water, S, P), 
          cp_water      = water_cp(T_water, S, P)) 
               
water_lh (T_water = 20, S = 30, P = 101325)

}               
\arguments{
 \item{T_water}{ temperature of the water, one value or a vector [degC]. 
  }
 \item{S}{ salinity of the water, one value [-]. 
  }
 \item{P}{ pressure, one value [Pa]. 
  }
 \item{type}{ formulation for conductivity, 1 = Jamieson, 2 = Caldwell, 3 = Castelli. 
  }
 \item{density_water}{ Water density; [kg/m3], one value, or a vector of length = length of \code{T_water}.
 }
 \item{cp_water}{ Specific heat capacity of the water ; [J/kg/K], one value, or a vector of length = length of \code{T_water}.
 }
}

\value{
function \code{water_properties} returns a list with:
 \itemize{
  \item \code{density_water}, the density of the water, [kg/m3], as many values as length of \code{T_water}.
  \item \code{cp_water}, the specific heat capacity of the water [J/kg/K], length of \code{T_water}.
  \item \code{td_water}, the thermal diffusivity of the water, [m2/s], length of \code{T_water}.
  \item \code{tc_water}, the thermal conductivity of the water, [W/m/K], length of \code{T_water}.
  \item \code{lh_water}, the latent heat of vaporization of the water, [W/m/K], length of \code{T_water}.
 } 

function \code{water_density} returns a vector with the density of the water, 
[kg/m3], as many values as length of \code{T_water}.

function \code{water_cp} returns a vector with the specific heat capacity of the water, 
[J/kg/K], as many values as length of \code{T_water}.

function \code{water_td} returns a vector with the thermal diffusivity of the water, 
[m2/s], as many values as length of \code{T_water}.

function \code{water_tc} returns a vector with the thermal conductivity of the water, 
[W/m/K], as many values as length of \code{T_water}.

The object attributes contain:

\itemize{
 \item A \code{description} of these variables, including their units.
 \item the input \code{parameter} settings. 
}

use \code{attibutes(object)$description} and \code{attibutes(object)$parameters} to extract it. 
}

\author{
  Karline Soetaert
}

\details{
See vignette("TempSED") for a description of the formulations used.

\code{Latent heat of vaporization} is the amount of heat
required to transform a unit mass from the liquid to the
gaseous state. 

\code{Specific heat} is the amount of heat required to change the temperature of 
a mass unit of a substance by one degree; 
here we use Cp, the isobaric specific heat (at constant pressure)

\code{Thermal conductivity} describes the ability to conduct heat;  it is
the quantity of heat transmitted due to a unit temperature gradient, 
through a unit thickness of a material, in a direction normal to a surface of unit area.

\code{Thermal diffusivity} is the rate of transfer of heat of a material; 
a measure of how quickly a material can absorb heat from its surroundings. 
It can be calculated by taking the thermal conductivity divided by density and specific heat capacity at constant pressure.
}

\examples{

# ---------------------------------------------------------
# The defaults
# ---------------------------------------------------------

 wP <- water_properties()
 wP
 attributes(wP)
 
 water_density()
 as.vector(water_density())
 attributes(water_density())

# ---------------------------------------------------------
# range of parameters
# ---------------------------------------------------------

 t.seq <- 0:50
 w.t   <- water_properties(T_water = t.seq)
 attributes(w.t)[c("description", "parameters")]
 
 par (mfrow = c(2,2), las = 1)
 
 plot(t.seq, w.t$cp_water, 
      xlab = "temperature, degC", ylab = "J/kg/K",
      main = "specific heat capacity of water") 
 
 plot(t.seq, w.t$density_water, 
      xlab = "temperature, degC", ylab = "kg/m3",
      main = "density of water")
 
 plot(t.seq, w.t$td_water, 
      xlab = "temperature, degC", ylab = "m2/s", 
      main = "thermal diffusivity of water")
 
 plot(t.seq, w.t$lh_water, 
      xlab = "temperature, degC", ylab = "J/kg", 
      main= "latent heat of vaporization")

}
\references{
general:

 Sharkawy, M.H. Lienhard,J.H., Zubair, S.M., 2010. 
 Thermophysical properties of seawater: a review of existing correlations and data. 
 Desalination and Water Treatment 16: 1-3, 354-380.

 K.G. Nayar, M.H. Sharqawy, L.D. Banchik, and J.H. Lienhard V, 2016, 
 Thermophysical properties of seawater: A review and new correlations
 that include pressure dependence, 
 Desalination, Vol. 390, pp.1-24. doi:10.1016/j.desal.2016.02.024

for conductivity:

 D.T. Jamieson, and J.S. Tudhope (1970). 
 Physical properties of seawater solutions. 
 Thermal Conductivity, Desalination, 8, 393-401.

 D. Caldwell,  (1974). Thermal conductivity of sea water. 
 Deep Sea Research and Oceanographic Abstracts, 21(2) 131-137.

 V. Castelli, E. Stanley and E. Fischer (1974). 
 The thermal conductivity of seawater as a function of pressure and temperature. 
 Deep Sea Research and Oceanographic Abstracts, 21(4) 311-319.

for density: 

 F.J. Millero and A. Poisson, (1981) 
 International one-atmosphere equation of state of seawater, 
 Deep-Sea Research, 28A (6) 625-629.

for specific heat capacity:

 D.T. Jamieson, J.S. Tudhope, R. Morris and G. Cartwright, (1969) 
 Physical properties of sea water solutions: heat capacity,
 Desalination, 7(1) 23-30. 

}
\seealso{
  \code{\link{air_properties}}.
  \code{\link{mineral_properties}}.
  \code{\link{bulk_properties}}.
  \code{\link{flux_heat}}.
}

\keyword{ utilities }
