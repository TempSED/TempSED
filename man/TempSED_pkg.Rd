\name{TempSED-package}
\alias{TempSED-package}
\alias{TempSED}
\docType{package}
\title{
  A package to model temperature in aquatic sediments.
}
\description{
  TempSED contains functions and datasets for modelling temperature in aquatic sediments that are exposed to the air or overlying water.
  
  Its main functions are:
  \itemize{
    \item \code{TempSED_run1D} calculates temperature in a vertical sediment column that contains \bold{100} layers.

    \item \code{flux_heat} estimates the various contributions of heat exchange across the sediment-water or sediment-air interface.
      
      \item \code{air_properties}, \code{water_properties}, \code{bulk_properties} estimates physical properties of the air, water and bulk sediment.
  }

  The models are implemented in fortran and linked to R.

  Its main datasets are:
  \itemize{
    \item \code{ForcingOS, DataOS} the forcing functions and observed data to model temperature in sediments of the Oosterschelde (the Netherlands).
  }
}


\author{
  Karline Soetaert
}

\references{
 to be added
}

\keyword{ package }
