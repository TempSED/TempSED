\name{get parameters and properties}
\alias{TempSED_getparms}
\alias{TempSED_getpor}
\alias{TempSED_getirr}
\alias{TempSED_getdx}
\alias{TempSED_getdepth}
\alias{TempSED_getgrid}
\title{
  Functions to retrieve parameters, porosity, depth and sediment grid, irrigation and bioturbation, for the TempSED model.
}
\description{

  \code{TempSED_getparms, TempSED_getdepth, TempSED_getdx} retrieve the parameters, sediment depths and layer thicknesses of TempSED model solutions.
  
  \code{TempSED_getpor, TempSED_getirr} retrieve the porosity, and irrigation profiles of TempSED model solutions.

}
\usage{
  TempSED_getparms(out = NULL, as.vector = FALSE, which = NULL)
  TempSED_getdepth(out)
  TempSED_getgrid(out)
  TempSED_getdx(out)
  TempSED_getpor(out)
  TempSED_getirr(out)
}
\arguments{

  \item{out }{an output object returned by \link{TempSED_run1D}.
   If \code{NULL}, \code{TempSED_getparms} will return the default (parameter) values.
  }
  \item{as.vector }{if \code{TRUE} will return the parameter \code{vector}, else a \code{data.frame} that also contains the units.
  }
  \item{which }{if not \code{NULL}, a vector with names of the variables/parameters to return.
  }
}

\author{
  Karline Soetaert
}

\examples{

# defaults
  defparms  <- TempSED_getparms(as.vector = TRUE)
  defparms

# a run to work with  
  defdyn    <- TempSED_run1D()

  out <- TempSED_run1D(parms     = list(cp_solid = 500))

# grid used for outputs

  plot(TempSED_getpor(out), TempSED_getdepth(out), 
       ylim = c(0.10, 0), 
       type = "l", 
       ylab = "m", xlab = "-", main = "porosity")
  
}

\references{
  Soetaert K, PMJ Herman and JJ Middelburg, 1996.
  A model of early diagenetic processes from the shelf to abyssal depths.
  Geochimica Cosmochimica Acta, 60(6):1019-1040.
}

\seealso{
  \code{\link{TempSED_get0Dvars}}.
  \code{\link{TempSED_getparms}}.
  \code{\link{plot.TempSEDdyn}}.
}

\keyword{ utilities }
