\name{TempSED output and state variables}
\alias{TempSED_get0Dvars}
\alias{TempSED_get1Dvars}
\alias{TempSED_getstates}
\title{
  Functions to retrieve variables for the TempSED model.
}
\description{

  \code{TempSED_get0Dvars, TempSED_get1Dvars, TempSED_getstates} retrieve the (0-dimensional or 1-dimensional) output variables or the state variables of TempSED model solutions.  When called with \code{out} not specified, will return the names of these variables and their units.
}
\usage{
  TempSED_get0Dvars(out, subset, as.vector = FALSE, which = NULL) 
  TempSED_get1Dvars(out, subset, which = NULL) 
  TempSED_getstates(out, subset, which = NULL) 
}
\arguments{

  \item{out }{an output object returned by \link{TempSED_run1D}. 
  }
  \item{as.vector }{if \code{TRUE} will return the parameter vector, else a data.frame that also contains the units.
  }
  \item{subset }{logical expression indicating elements or rows to keep: missing values are taken as false.
  }
  \item{which }{if not \code{NULL}, a vector with names of the variables/parameters to return.
  }
}
\value{
 \code{TempSED_get0Dvars} and \code{TempSED_get1Dvars} return the output variables of the solution as a vector or data.frame. 
 For dynamic runs, the output is averaged over the mean of the run.
 
 \code{TempSED_get1Dvars} always returns the sediment depth and the porosity as the first two columns.
}

\author{
  Karline Soetaert
}


\examples{

# defaults
  TempSED_getstates()
  head(TempSED_get0Dvars())
  TempSED_get1Dvars()
  
# some runs to work with  
  defdyn    <- TempSED_run1D()   # times = 1:3600 in the default

# altered run
  out <- TempSED_run1D(parms = list(em_air = 0.95))

# 0-D outputs
  cbind(dyna   = TempSED_get0Dvars(defdyn, as.vector = TRUE), 
        ref    = TempSED_get0Dvars(out))

# 1-D outputs
  head(TempSED_get1Dvars(out))
  head(TempSED_get1Dvars(defdyn))

# State variables
  head(TempSED_getstates(out))   # average over all times
  head(TempSED_getstates(out, 
                         subset = time > 1800))   # average over selection
  head(TempSED_getstates(out, 
                         subset = nrow(out)))   # last value
}

\references{
  Soetaert K, PMJ Herman and JJ Middelburg, 1996a.
  A model of early diagenetic processes from the shelf to abyssal depths.
  Geochimica Cosmochimica Acta, 60(6):1019-1040.

  Soetaert K, PMJ Herman and JJ Middelburg, 1996b.
  Dynamic response of deep-sea sediments to seasonal variation: a model.
  Limnol. Oceanogr. 41(8): 1651-1668.
}

\seealso{
  \code{\link{TempSED_run1D}}.
  \code{\link{TempSED_getparms}}.
  \code{\link{plot.TempSEDdyn}}.
}

\keyword{ utilities }
