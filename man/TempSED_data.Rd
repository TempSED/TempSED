\name{TempSED Data}
\docType{data}
\alias{ForcingOS}
\alias{DataOS}
\alias{InitTempOS}
\title{
  Forcings, initial conditions, and sediment data from the Oosterschelde in 2020
}
\description{
    Data set \code{ForcingOS} contains hourly data for meteorological conditions and for water column properties for 2020; \code{HRforcingOS} contains water height and water temperature data with 10-minute resolution for 2020.

    The data were obtained from:

  \itemize{
    \item Royal Netherlands Meteorological Institute KNMI (WindSpeed, AirTemperature,
    Cloudiness, SolarRadiation, Pressure, AirHumidity), their station 315, Vlissingen.

    \item from Rijkswaterstaat (waterHeight), from the mooring at Stavenisse

    \item Royal Netherlands Institute for Sea Research NIOZ (WaterTemperature):
    the Yersekse paal, in the center of the Oosterschelde.
   }
   The dataset contains:
  \itemize{
    \item \code{Time, Second}: the time in long format and in seconds.
    The KNMI data were obtained at hourly intervals; the other data (WaterTemperature, WaterHeight) were at 10 minute intervals. These data are in data set \code{HRforcingOS}. The values at the start of the day are also in \code{ForcingOS}
    \item \code{Windspeed}, speed of the wind at 10 m above the surface [m/s] 
    \item \code{Airtemperature, Watertemperature}, [dgC]
    \item \code{Dewpoint}, dewpoint air Temperature, [dgC]
    \item \code{Cloudiness}, fractional cover of the sky by clouds [-]).
    \item \code{Solarradiation}, solar radiation [W/m2].
    \item \code{Airhumidity}, water vapor as a fraction of saturated water vapor  [-].
    \item \code{Pressure} [Pa].
    \item \code{Rain} [cm/hour]
    \item \code{Waterheight}, water height above NAP (nieuw Amsterdams Peil) [m].
  }

    Data set \code{DataOS} contains sediment temperature data from Zandkreek
    in the Oosterschelde, at 3 cm deep. Data from NIOZ.   It contains:
  \itemize{
    \item \code{Time}: the time in long format and in seconds, [Second].
    \item \code{SedimentTemperature}, [dgC]
    \item \code{Depth_cm}, the depth at which the temperature was recorded [cm]
   }
   
   \code{InitTempOS} contains initial conditions to run the Oosterschelde model.
}

\usage{
ForcingOS
DataOS
}

\format{
  ForcingOS is a data.frame with 8784 rows and 10 columns;

  DataOS is a data.frame with 9898 rows and 4 columns.
}

\author{
  Karline Soetaert <karline.soetaert@nioz.nl>
}

\examples{
summary(ForcingOS)
plot(ForcingOS[, 1:2], type = "l")
attributes(ForcingOS)$description

head(DataOS)

# use forcing function in model
 times <- seq(from = as.POSIXct("2020-01-01"),
              to   = as.POSIXct("2020-04-01"),
              by   = 3600)
 Tout <- TempSED_run1D(
      T_ini            = 10, 
      times            = as.numeric(times), 
      f_Airtemperature = ForcingOS[, c("Time", "Airtemperature")],
      f_Solarradiation = ForcingOS[, c("Time", "Solarradiation")])

# initial condition
plot(InitTempOS[, 2:1], ylim = c(10,0), 
     main = "InitTempOS", ylab = "depth, m", xlab = "Temperature")

}

\keyword{datasets}

