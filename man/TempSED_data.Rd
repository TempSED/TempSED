\name{TempSED Data}
\docType{data}
\alias{ForcingOS}
\alias{DataOS}
\alias{InitTempOS}
\title{
  Forcings, initial conditions, and sediment data from the Oosterschelde in 2020
}
\description{
    Data set \code{ForcingOS} contains hourly data for meteorological conditions and for water column properties for 2020; 

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
    The KNMI data were obtained at hourly intervals; the other data (WaterTemperature, WaterHeight) were at 10 minute intervals; they were hourly averaged. 
    \item \code{Windspeed}, speed of the wind at 10 m above the surface [m/s] 
    \item \code{Airtemperature, Watertemperature}, [degC]
    \item \code{Dewpoint}, dewpoint air Temperature, [degC]
    \item \code{Cloudiness}, fractional cover of the sky by clouds [-]).
    \item \code{Solarradiation}, solar radiation [W/m2].
    \item \code{Qrel}, relative air humidity; water vapor as a fraction of saturated water vapor  [-].
    \item \code{Pressure}, total air pressure [Pa].
    \item \code{Rain} [cm/hour]
    \item \code{Waterheight}, water height above NAP (nieuw Amsterdams Peil) [m].
  }

    Data set \code{DataOS} contains sediment temperature data at 15 minute resolution, from Zandkreek
    in the Oosterschelde, at 3, 6 and 9 cm deep. Data from NIOZ.   It contains:
  \itemize{
    \item \code{Time}: the time in long format and in seconds, [Second].
    \item \code{SedimentTemperature}, [degC]
    \item \code{Depth_cm}, the depth at which the temperature was recorded [cm]
   }
   
   \code{InitTempOS} contains initial conditions to run the Oosterschelde model, over a grid with 200 boxes.
}

\usage{
ForcingOS
DataOS
}

\format{
  ForcingOS is a data.frame with 8784 rows and 10 columns;

  DataOS is a data.frame with 29694 rows and 4 columns.
}

\author{
  Karline Soetaert <karline.soetaert@nioz.nl>
}

\examples{

# ---------------------------------------------------------
# Forcings
# ---------------------------------------------------------

summary(ForcingOS)
plot(ForcingOS[, c(1,3)], type = "l")
attributes(ForcingOS)$description

# ---------------------------------------------------------
# Dataset
# ---------------------------------------------------------

head(DataOS)

table(DataOS$Depth_cm)  # number of data points at 3, 6, 9 cm

# ---------------------------------------------------------
# initial condition
# ---------------------------------------------------------

plot(InitTempOS[, 2:1], ylim = c(10,0), 
     main = "InitTempOS", ylab = "depth, m", xlab = "Temperature")

# ---------------------------------------------------------
# use data in a model
# ---------------------------------------------------------
#------------
# spinup run
#------------

# spinup times (till the data start)

 times <- seq(from = as.POSIXct("2020-01-01"),
              to   = as.POSIXct("2020-06-01"),
              by   = 12*3600)
 
# initial condition: interpolate data from InitTempOS to the (default) grid:
 x     <- TempSED_getdepth()
 T_ini <- approx(InitTempOS, xout = x)$y
 
# run the model with forcing data for air temperature and solar radiation
 Tout.ini <- TempSED_run1D(
      T_ini            = T_ini, 
      times            = as.numeric(times), 
      
      # forcing functions passed as dataset:
      
      f_Airtemperature = ForcingOS[, c("Time", "Airtemperature")],
      f_Solarradiation = ForcingOS[, c("Time", "Solarradiation")]
)
      
#------------
# final run
#------------
# initial condition = last value of initial run

 times <- seq(from = as.POSIXct("2020-06-01"),
              to   = as.POSIXct("2020-08-01"),
              by   = 3600)

 Tout <- TempSED_run1D(
      T_ini            = Tout.ini,     # pass previous run
      times            = as.numeric(times), 
      
      # forcing functions passed as dataset:
      
      f_Airtemperature = ForcingOS[, c("Time", "Airtemperature")],
      f_Solarradiation = ForcingOS[, c("Time", "Solarradiation")],
      
      # output at the depth of the observations
      sedpos           = c(0.03, 0.06, 0.09)  
      )

# show last part

tail (Tout, n = c(3,3))

#------------
# prepare data
#------------
# create observed data from the dataset

DD <- data.frame(obs   = paste("Tsed_0.0", DataOS$Depth_cm, sep = ""),
                 time  = as.numeric(DataOS$Time),   
                 value = DataOS$SedimentTemperature) 

head(DD)

# show results, compared to data
# note: model is for sediment always exposed to the air, 
#       data  is for tidal sediment

plot(Tout, 
     which = c("Tsed_0.03", "Tsed_0.06", "Tsed_0.09"),
     obs   = DD, obspar = list(col = "red", pch = "."))
  
}

\keyword{datasets}

