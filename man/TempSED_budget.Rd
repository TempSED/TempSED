\name{TempSED_budget}
\alias{TempSED_budget}
\title{
  Heat budget of sediment.
}
\description{
  \code{TempSED_budget} estimates mean or integrated values of the heat budget.
}
\usage{
TempSED_budget (out, ..., how = c("mean", "integral"), remove_ini = FALSE)
}
\arguments{

  \item{out, ... }{an output object returned by \link{TempSED_run1D}.
  }
  \item{how }{if \code{"mean"} will return the heat fluxes, averaged over the interval, if \code{"integral"}, will return integrated fluxes.
  }
  \item{remove_ini }{whether to remove the initial value or not. This is especially handy if model initialisation was not in equilibrium with the imposed dynamics.
  }
}

\value{
 A data.frame with the different terms of the heat budget:
 
 \itemize{
   \item  \code{Heat_accumulation}, Heat added to sediment (estimated from temperature differences between start and end)
   \item  \code{Heatflux_total}, Total heat added via upper boundary and radiation
   \item  \code{Heatflux_convection},  Convective heat flux with overlying water 
   \item  \code{Heatflux_radiation},  Shortwave radiation heat input
   \item  \code{Heatflux_latent}, latent heat input (~evaporation)
   \item  \code{Heatflux_sensible}, sensible heat input (~air temperature)
   \item  \code{Heatflux_backrad},  net longwave radiation (net backradation)
   \item  \code{Heatflux_deep}, Deep heat flux out of the domain (lower boundary
   \item  \code{precision}, \% precision estimated as:
   100*(Heat_accumulation-(Heatflux_total-Heatflux_deep))/Heat_accumulation
 }
 
 
 The objects attributes contain a description of these output variables, including their units.
}

\author{
  Karline Soetaert
}

\details{
For applications with very variable forcings, or that switch between inundated (water) and exposed (to air) conditions, the budget will be approximate, depending on the amount of output time points. The coarser the output times, the less accurate it will be. 

Comparing \code{Heat_accumulation} with \code{Heatflux_total - Heatflux_deep} allows to estimate the accuracy of the budget. In theory they should be the same, with "Heat_accumulation" giving the exact values (as it is based on the temperture difference at the start and end of the simulation). Sometimes the run is initialised with a profile that is not consistent with the imposed forcing functions. It then helps to remove the initial condition from the budget calculation (code{remove_ini = TRUE}).

}


\examples{

# =============================
# yearly forcing
# =============================

# Forcing functions for air temperature (degC) 
# and solar radiation (W/m2), yearly period

fAirTempYr  <- function(t)                            
                    return(15 + 15*sin(2*pi*t/86400/365))

fSolarRadYr <- function(t)                           
                    return(150 + 120*sin(2*pi*t/86400/365))   

# spinup - not in equilibrium with the forcings
#------------------------------------------------

times   <- seq(from = 0, to = 365*86400, by = 86400)

ToutIni <- TempSED_run1D(
                   T_ini            = 10, 
                   times            = times, 
                   f_Airtemperature = fAirTempYr,
                   f_Solarradiation = fSolarRadYr)

# final run - more in equilibrium
#------------------------------------------------

ToutYr  <- TempSED_run1D(
                  T_ini            = ToutIni, 
                  times            = times, 
                  f_Airtemperature = fAirTempYr,
                  f_Solarradiation = fSolarRadYr)

# Heatflux is changing rapidly in first few steps 
plot (ToutIni, ToutYr, 
      which = "Heatflux_total", lwd = 2)
legend("topright", col = 1:2, lwd = 2, legend = c("ToutIni", "ToutYr"))


# The budget 
# budget for initial run: rather inaccurate, 
#        for final run  : accurate
#------------------------------------------------

TempSED_budget(ToutIni, ToutYr)


# more accurate budget if the initial condition is removed.
#------------------------------------------------

TempSED_budget(ToutIni, remove_ini = TRUE)


# also more accurate if smaller output steps in the first part
#------------------------------------------------

times_0 <- seq(from = 0,     to =  86400-60, by = 60)
times_1 <- seq(from = 86400, to = 365*86400, by = 86400)

ToutIni2 <- TempSED_run1D(
                   T_ini            = 10, 
                   times            = c(times_0, times_1), 
                   f_Airtemperature = fAirTempYr,
                   f_Solarradiation = fSolarRadYr)

TempSED_budget(ToutIni2)

# Very accurate for small output steps
Tout3 <- TempSED_run1D(
                   T_ini            = 10, 
                   times            = seq(0, 86400, by = 10), 
                   f_Airtemperature = fAirTempYr,
                   f_Solarradiation = fSolarRadYr)

TempSED_budget(Tout3)
                   
}
\references{
to be added
}

\seealso{
  \code{\link{TempSED_get0Dvars}}.
  \code{\link{TempSED_getparms}}.
  \code{\link{plot.TempSEDdyn}}.
  \code{\link{flux_heat}}.
}

\keyword{ utilities }
