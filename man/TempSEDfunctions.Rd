\name{Periodic functions}
\alias{trunc_function}

\title{
  Utility functions for use with the TempSED models. Creates a truncated sinewave type of function.
}
\description{

\code{trunc_function} returns a function that estimates variations within a certain time period ( \code{t_period}) and that has a specific time length within this period (t_trunc) where the value is larger than a preset value ( \code{t_trunc}). The returned function can be used to estimate the value at any time (in seconds).
For instance, it can be used to estimate:

* light intensity variations within a day (t_period = 24 hours - t_trunc = daylength, v_trunc = 0)

* water height over a tidal cycle (t_period = 12,4 hours - t_trunc = inundation period, v_trunc = 0)
}
\usage{
trunc_function (t_period = 86400, t_offset = 0, t_trunc = t_period/2, 
                v_trunc = 0, v_average = 1, v_maximum = NA)
}

\arguments{

  \item{t_period }{periodicity of the function, in [s].
  }
  \item{t_offset }{time offset, in [s].
  }
  \item{t_trunc }{seconds during which function value is larger than \code{v_trunc}; the default is half of the periodicity.
  }
  \item{v_trunc }{minimum value below the function should be truncated.
  }
  \item{v_average, v_maximum }{mean and maximum function-value, only one of those should be specified.
  }
}

\details{

 \code{trunc_function} creates a periodic sinusoidal function with a given periodicity, and where the fraction of time that the function is larger than a certain value (usually 0) can be specified. Useful, for instance to generate light within a day or within a year.
}

\author{
  Karline Soetaert
}


\examples{

par(mfrow = c(2, 1))

# Light variations within a day
DL5  <- trunc_function(t_period   = 86400, 
                       t_trunc    = 5*3600, 
                       v_average  = 150)
                       
DL10 <- trunc_function(t_period   = 86400, 
                       t_trunc    = 10*3600, 
                       v_average  = 150)

DL15 <- trunc_function(t_period   = 86400, 
                       t_trunc    = 15*3600, 
                       v_average  = 150)
                       
DL20 <- trunc_function(t_period   = 86400, 
                       t_trunc    = 20*3600, 
                       v_average  = 150)

time <- seq(from = 1, to = 2*24*3600, by = 180)

matplot(x = time/3600, 
        y = cbind(DL5(time), DL10(time), DL15(time), DL20(time)),   
        xlab = "hr", ylab = "W/m2",
        main = "light intensity - mean=150 W/m2", 
        type = "l", lty = 1)

legend("top", 
        col = 1:4, 
        title = "Day Length (hr)", 
        legend = c(5, 10, 15, 20), 
        lty = 1)

# Tidal variations
T1    <- trunc_function(t_period   = 12.4*3600, 
                        t_trunc    = 1*3600,   
                        v_maximum  = 1)

T6    <- trunc_function(t_period   = 12.4*3600, 
                        t_trunc    = 6*3600,   
                        v_maximum  = 1)

T10   <- trunc_function(t_period   = 12.4*3600, 
                        t_trunc    = 10*3600,   
                        v_maximum  = 1)

T12.3 <- trunc_function(t_period   = 12.4*3600, 
                        t_trunc    = 12.3*3600, 
                        v_maximum  = 1)

matplot(x = time/3600, 
        y = cbind(T1(time), T6(time), T10(time), T12.3(time)), 
        xlab = "hr", ylab = "m",
        main = "Water Height - max = 1 m, period=12.4H", 
        col = 1:4, type = "l", lty = 1)

legend("top", 
       col = 1:4,
       title = "Tidal length (hr)", 
       legend = c(1, 6, 10, 12.3),
       lty = 1)

}
\value{
 A function that needs to be called with a time argument
}

\keyword{ utilities }
