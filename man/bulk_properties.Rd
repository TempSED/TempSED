\name{bulk properties}
\alias{bulk_properties}
\alias{bulk_density}
\alias{bulk_cp}
\alias{bulk_td}
\alias{bulk_tc}
\title{
  Properties of the bulk sediment.
}
\description{
  Calculates the density (\code{bulk_density}), 
  specific heat capacity (\code{bulk_cp}), 
  thermal conductivity ( \code{bulk_tc}), and 
  thermal diffusivity (\code{bulk_td}) of the bulk sediment. 
  
  \code{bulk_properties} calculates all these properties.
}

 
\usage{
bulk_properties(density_water = 1024, density_solid = 2500,  cp_water = 3994, 
                cp_solid = 1000, td_water = 1.4e-07, td_solid = 2e-06, 
                tc_water = 0.6, tc_solid = 2.0, porosity = 0.5)

bulk_density (density_water = 1024, density_solid = 2500, porosity = 0.5)

bulk_tc (tc_water = 0.6, tc_solid = 2.0, porosity = 0.5)

bulk_cp (density_water = 1024, density_solid = 2500, cp_water = 3994, 
         cp_solid = 1000, porosity = 0.5)

bulk_td (density_water = 1024, density_solid = 2500, cp_water = 3994, 
         cp_solid = 1000, td_water = 1.4e-07, td_solid = 2e-06, 
         porosity = 0.5) 
               
}               
\arguments{
 \item{porosity}{ volumetric water content (volume water/volume bulk), [-]. 
  }
 \item{density_water, density_solid}{ Water density (density_water) and sediment dry density (density_solid); [kg/m3], one value.
 }
 \item{cp_water, cp_solid}{ Specific heat capacity of the water (cp_water) and of solid sediment (cp_solid) [J/kg/dgC], one value.
 }
 \item{td_water, td_solid}{ The thermal diffusivity of the water (td_water) and of the solid sediment (td_solid) [m2/s] (the "diffusion coefficient" for temperature), one value.
  }
 \item{tc_water, tc_solid}{ The thermal conductivity of the water (tc_water) and of the solid sediment (tc_solid) [W/m/dg], one value.
  }
}

\value{

function \code{bulkProperties} returns a list with:
 \itemize{
  \item \code{density_bulk}, the density of the bulk sediment, [kg/m3], as many values as length of porosity.
  \item \code{cp_bulk}, the specific heat capacity of the bulk sediment [J/kg/dgC], length of porosity.
  \item \code{td_bulk}, the thermal diffusivity of the bulk sediment, [m2/s], length of porosity.
  \item \code{tc_bulk}, the thermal conductivity of the bulk sediment, [W/m/s], length of porosity.
 } 

function \code{bulk_density} returns a vector with the density of the bulk sediment, 
[kg/m3], as many values as length of porosity.

function \code{bulk_cp} returns a vector with the specific heat capacity of the bulk sediment 
[J/kg/dgC], as many values as length of porosity.

function \code{bulk_td} returns a vector with the thermal diffusivity of the bulk sediment, 
[m2/s], as many values as length of porosity.

function \code{bulk_tc} returns a vector with the thermal conductivity of the bulk sediment, 
[W/m/s], as many values as length of porosity.

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
}

\examples{

# ---------------------------------------------------------
# The defaults
# ---------------------------------------------------------

 bulk_properties()
 bulk_density()
 as.vector(bulk_density())
 
 attributes(bulk_properties())
 
# ---------------------------------------------------------
# range of parameters
# ---------------------------------------------------------

 por.seq  <- seq(from = 1, to = 0.5, length.out = 10)
 
 BP.por   <- bulk_properties(por = por.seq)
 BP.por
 attributes(BP.por)

 CP.por  <- bulk_cp(por = por.seq)
 CP.por
 attributes(CP.por)

 cps.seq <- seq(from = 400, to = 2000, length.out = 100)
 BP.cps  <- bulk_properties(cp_solid = cps.seq)
 head(BP.cps)
 
 attributes(BP.cps)$description

# ---------------------------------------------------------
# realistic porosity profile
# ---------------------------------------------------------

x <- seq(0, 0.3, length.out=100)

# a function to estimate porosity

p.exp <- function (x,                      # depth
                   y.0 = 0.9, y.inf = 0.5, # porosity at upper and lower boundary
                   x.att = 0.01)           # attenuation coefficient
                   
    return(y.inf + (y.0 - y.inf) * exp(-x/x.att))

por.seq <- p.exp(x)
BP      <- bulk_properties(por = por.seq)

par(mfrow = c(1,4), las = 1)
plot(por.seq, x*100, 
     type = "l", ylim = c(30, 0), 
     xlab = "-", ylab = "cm", main = "sediment porosity")
     
plot(BP$density_bulk, x*100, 
     type = "l", ylim = c(30, 0), 
     xlab = "kg/m3", ylab = "cm", main = "bulk density")
     
plot(BP$cp_bulk, x*100, 
     type = "l", ylim = c(30, 0), 
     xlab = "J/kg/dg", ylab = "cm", main = "bulk heat capacity")
     
plot(BP$td_bulk, x*100, 
     type = "l", ylim = c(30, 0), 
     xlab = "m2/s", ylab = "cm", main="bulk thermal diffusivity")
}
\references{
Shusaku Goto, Makoto Yamano, Sumito Morita, Toshiya Kanamatsu, Akihiro Hachikubo, Satsuki Kataoka, Manabu Tanahashi, Ryo Matsumoto, 2017. 
Physical and thermal properties of mud-dominant sediment from the Joetsu Basin in the eastern margin of the Japan Sea. 
Mar Geophys Res 38:393-407, DOI 10.1007/s11001-017-9302-y
}

\seealso{
  \code{\link{air_properties}}.
  \code{\link{water_properties}}.
  \code{\link{mineral_properties}}.
  \code{\link{flux_heat}}.
}
\keyword{ utilities }
