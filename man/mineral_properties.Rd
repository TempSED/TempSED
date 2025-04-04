\name{Properties of sediment minerals}
\alias{mineral_properties}
\alias{mineral_density}
\alias{mineral_tc}
\alias{mineral_cp}
\alias{mineral_td}

\title{
  Properties of the major sediment-constituent minerals.
}
\description{
  \code{mineral_properties} calculates the density, specific heat capacity, and thermal conductivity and diffusivity of the major mineral consituents of sediments relevant for sediment temperature modelling.
}

 
\usage{
mineral_properties (
    mineral = c("Quartz", "Orthoclase", "Albite", "Anorthite",  "Calcite" ,                  
                "Muscovite", "Illite", "Smectite (montmorillonite)", 
                "Chlorite", "Pyrite"))
mineral_density    (
    mineral = c("Quartz", "Orthoclase", "Albite", "Anorthite",  "Calcite" ,                  
                "Muscovite", "Illite", "Smectite (montmorillonite)", 
                "Chlorite", "Pyrite"))

mineral_tc   (
    mineral = c("Quartz", "Orthoclase", "Albite", "Anorthite",  "Calcite" ,                  
                "Muscovite", "Illite", "Smectite (montmorillonite)", 
                "Chlorite", "Pyrite"))

mineral_cp         (    
    mineral = c("Quartz", "Orthoclase", "Albite", "Anorthite",  "Calcite" ,                  
                "Muscovite", "Illite", "Smectite (montmorillonite)", 
                "Chlorite", "Pyrite"))

mineral_td         (
    mineral = c("Quartz", "Orthoclase", "Albite", "Anorthite",  "Calcite" ,                  
                "Muscovite", "Illite", "Smectite (montmorillonite)", 
                "Chlorite", "Pyrite"))

}
\arguments{
 \item{mineral}{ Sediment-constituent mineral, one element or a vector. 
  }}

\value{
function \code{mineral_properties} returns a list with:
 \itemize{
  \item \code{density}, the mineral density, [kg/m3]
  \item \code{cp}, the specific heat capacity of the mineral(s), [J/kg/K]
  \item \code{td}, the thermal conductivity of the mineral(s),  [W/m/K]
  \item \code{tc}, the thermal diffusivity of the mineral(s),    [m2/s]
 } 

The object attributes contain:

\itemize{
 \item A \code{description} of these variables, including their units
}

use \code{attibutes(object)$description} to extract it. 
}

\author{
  Karline Soetaert
}

\examples{

mineral_properties()
mineral_properties("Quartz")

mineral_tc()
mineral_tc("Quartz")

# description
attributes(mineral_properties())$description


}
\references{
Shusaku Goto, Makoto Yamano, Sumito Morita, Toshiya Kanamatsu, Akihiro Hachikubo, Satsuki Kataoka, Manabu Tanahashi, Ryo Matsumoto, 2017. 
Physical and thermal properties of mud-dominant sediment from the Joetsu Basin in the eastern margin of the Japan Sea. 
Mar Geophys Res 38:393-407, DOI 10.1007/s11001-017-9302-y
}
\seealso{
  \code{\link{air_properties}}.
  \code{\link{water_properties}}.
  \code{\link{bulk_properties}}.
  \code{\link{flux_heat}}.
}

\keyword{ utilities }
