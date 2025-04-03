## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo=TRUE, comment = NA)

## ----message=FALSE, echo=FALSE------------------------------------------------
require(plot3D)
require(TempSED)

## ----echo=FALSE, comment=NA---------------------------------------------------
Def_fun <- function(name){
  Z <- paste(as.character(deparse(name)), collapse="\n")
  Z <- substr(Z, 1, regexpr("{",Z,fixed =TRUE)-1)
  for (i in 1:10)
    Z <- gsub("  ", " ", Z)
  cat(paste(substitute(name),substr(Z, 9, nchar(Z))),"\n")
}

## ----echo=FALSE---------------------------------------------------------------
(TR <- ls("package:TempSED", pattern = "TempSED_run"))

## ----echo=FALSE,prompt=FALSE--------------------------------------------------
(TG <- ls("package:TempSED", pattern = "TempSED_get"))

## ----echo=FALSE, prompt=FALSE-------------------------------------------------
(TP <- ls("package:TempSED", pattern = "air_"))

## ----echo=FALSE---------------------------------------------------------------
(TB <- ls("package:TempSED", pattern = "bulk_"))

## ----echo=FALSE---------------------------------------------------------------
(TW <- ls("package:TempSED", pattern = "water_"))

## ----echo=FALSE---------------------------------------------------------------
(TF <- ls("package:TempSED", pattern = "flux_"))

## ----echo=FALSE, comment=NA---------------------------------------------------
Def_fun(air_properties)
attributes(air_properties())$parameters

## -----------------------------------------------------------------------------
knitr::kable(air_properties())
attributes(air_properties())$description

## -----------------------------------------------------------------------------
AP <- air_properties(T_air=10:12)
knitr::kable(AP)

## -----------------------------------------------------------------------------
T.seq   <- seq(0, 30,            length.out = 100)
Q.seq   <- seq(0,  1,            length.out = 100)
P.seq   <- seq(101325, 101325*2, length.out = 100)

Dtq <- outer(X   = T.seq, 
             Y   = Q.seq, 
             FUN = function(Ta, Q) 
               air_properties(T_air = Ta, Qrel = Q)$density)
Dtp <- outer(X   = T.seq, 
             Y   = P.seq, 
             FUN = function(Ta, P) 
               air_properties(T_air = Ta, P = P)$density)
Vtq <- outer(X   = T.seq, 
             Y   = Q.seq, 
             FUN = function(Ta, Q) 
               air_properties(T_air = Ta, Qrel = Q)$vapor)
Vtp <- outer(X   = T.seq, 
             Y   = P.seq, 
             FUN = function(Ta, P) 
               air_properties(T_air = Ta, P = P)$vapor)
Htq <- outer(X   = T.seq, 
             Y   = Q.seq, 
             FUN = function(Ta, Q) 
               air_properties(T_air = Ta, Qrel = Q)$humidity)
Htp <- outer(X   = T.seq, 
             Y   = P.seq, 
             FUN = function(Ta, P) 
               air_properties(T_air = Ta, P = P)$humidity)

## ----fig.width=8, fig.height=6------------------------------------------------
par(mfrow=c(2,3), las=1, oma=c(0,0,2,0))
image2D(z = Dtq, x = T.seq, y = Q.seq, 
        contour = TRUE, 
        main = "density", xlab = "T_air, dgC", 
        ylab = "Qrel, -", clab = "kg/m3")
image2D(z = Vtq, x = T.seq, y = Q.seq, 
        contour=TRUE, 
        main = "vapor pressure", xlab = "T_air, dgC",  
        ylab = "Qrel, -", clab = "Pa")
image2D(z = Htq, x = T.seq, y = Q.seq, 
        contour=TRUE, 
        main = "humidity", xlab = "T_air, dgC", 
        ylab = "Qrel, -", clab = "kg/kg")
image2D(z = Dtp, x = T.seq, y = P.seq/1000, 
        contour=TRUE, 
        main = "density", xlab = "T_air, dgC", 
        ylab = "P, KPa", clab = "kg/m3")
image2D(z = Vtp, x = T.seq, y = P.seq/1000, 
        contour=TRUE, 
        main = "vapor pressure", xlab = "T_air, dgC", 
        ylab = "P, KPa", clab = "Pa")
image2D(z = Htp, x = T.seq, y = P.seq/1000, 
        contour = TRUE, 
        main = "humidity", xlab = "T_air, dgC", 
        ylab = "P, KPa", clab = "kg/kg")
mtext(outer = TRUE, side = 3, "air properties", cex = 1.5)

## ----echo=FALSE---------------------------------------------------------------
airDry.ref   <- data.frame(
 temp    = seq(from = 35, to = -25, by = -5),
 density =c(1.1455, 1.1644, 1.1839, 1.2041, 1.225, 1.2466, 1.269, 1.2922,
            1.3163, 1.3413, 1.3673, 1.3943, 1.4224))

## ----fig.width=8, fig.height=4------------------------------------------------
T.seq   <- 0:30

Air.sat <- data.frame(temp = T.seq, 
                      air_properties(T_air = T.seq, Qrel = 1))

Air.dry <- data.frame(temp = T.seq, 
                      air_properties(T_air = T.seq, Qrel = 0))

## ----fig.width=8, fig.height=4------------------------------------------------
par(mfrow=c(1,3), las=1)

plot(T.seq, Air.sat$vapor, 
     type = "l", main = "Saturated vapor pressure", 
     xlab = "Air temperature", ylab = "Pa")
points(airSat.ref$temp, airSat.ref$P)

plot(T.seq, Air.sat$humidity, 
     type = "l", main = "Saturated humidity", 
     xlab = "Air temperature", ylab = "kg/kg")
points(airSat.ref$temp, airSat.ref$vapor)

plot(T.seq, Air.dry$density, 
     type = "l", main = "Dry air density", 
     xlab = "Air temperature", ylab = "kg/m3")
points(airDry.ref$temp, airDry.ref$density)

## ----echo=FALSE, comment=NA---------------------------------------------------
Def_fun(water_properties)
attributes(water_properties())$parameters

## -----------------------------------------------------------------------------
water_properties()
attributes(water_properties())[c("description", "parameters")]

## -----------------------------------------------------------------------------
WP <- water_properties(T_water = 10:12)
WP
attributes(WP)$description
attributes(WP)$parameters

## -----------------------------------------------------------------------------
T.seq   <- seq(from = 0,      to = 30,       length.out = 100)
S.seq   <- seq(from = 0,      to = 40,       length.out = 101)
P.seq   <- seq(from = 101325, to = 101325*2, length.out = 100)

Dtq <- sapply(X   = S.seq, 
              FUN = function(S) 
                    water_properties(T_water = T.seq, S = S)$density)

Dtp  <- sapply(X   = P.seq, 
               FUN = function(P) 
                     water_properties(T_water = T.seq, P = P)$density)

CPtq <- sapply(X   = S.seq, 
               FUN = function(S) 
                     water_properties(T_water = T.seq, S = S)$cp_water)

CPtp <- sapply(X   = P.seq, 
               FUN = function(P) 
                     water_properties(T_water = T.seq, P = P)$cp_water)

TDtq <- sapply(X   = S.seq, 
               FUN = function(S) 
                     water_properties(T_water = T.seq, S = S)$td_water)

TDtp <- sapply(X   = P.seq, 
               FUN = function(P) 
                     water_properties(T_water = T.seq, P = P)$td_water)

## ----fig.width=8, fig.height=6------------------------------------------------
par(mfrow=c(2,3), las=1, oma=c(0,0,2,0))
image2D(z = Dtq,  x = T.seq, y = S.seq, 
        contour = TRUE, 
        main = "density", xlab = "T_water, dgC", 
        ylab = "S, -", clab = "kg/m3")
image2D(z = CPtq, x = T.seq, y = S.seq, 
        contour = TRUE, 
        main = "specific heat capacity", xlab = "T_water, dgC", 
        ylab = "S -", clab = "J/kg/dg")
image2D(z = TDtq, x = T.seq, y = S.seq, 
        contour = TRUE, 
        main = "thermal diffusivity", xlab = "T_water, dgC", 
        ylab = "S, -", clab = "m2/s")
image2D(z = Dtp,  x = T.seq, y = P.seq/1000, 
        contour = TRUE, 
        main = "density", xlab = "T_water, dgC", 
        ylab = "P, KPa", clab = "kg/m3")
image2D(z = CPtp, x = T.seq, y = P.seq/1000, 
        contour = TRUE, 
        main = "specific heat capacity", xlab = "T_water, dgC", 
        ylab = "P, KPa", clab = "J/kg/dg")
image2D(z = TDtp, x = T.seq, y = P.seq/1000, 
        contour = TRUE, 
        main = "thermal diffusivity", xlab = "T_water, dgC", 
        ylab = "P, KPa", clab = "m2/s")
mtext(outer = TRUE, side = 3, "water properties", cex = 1.5)

## ----echo=FALSE---------------------------------------------------------------
cp.ref   <- data.frame(  # specific heat capacity, J/kg/dgC 
 temp    = seq(0, 40, by=10),
 sal_10 = c(4141.8, 4136.7, 4132.9, 4130.6, 4129.8),
 sal_30 = c(4019.2, 4022.2, 4024.9, 4027.5, 4030.3))

tc.ref   <- data.frame(  # thermal conductivity, W/kg/dgC
 temp    = seq(0, 40, by=10),
 sal_10 = c(0.571, 0.588, 0.603, 0.617, 0.629),
 sal_30 = c(0.570, 0.587, 0.602, 0.616, 0.628))

td.ref   <- data.frame(  # thermal diffusivity, m2/s
 temp    = seq(0, 40, by=10),
 sal_10 = c(1.37e-07, 1.41e-07, 1.45e-07, 1.49e-07, 1.52e-07),
 sal_30 = c(1.38e-07, 1.43e-07, 1.46e-07, 1.50e-07, 1.54e-07))

lh.ref   <- data.frame(  # latent heat of vaporisation, 
 temp    = seq(0, 40, by=10),
 sal_10 = c(2475.9, 2452.5, 2429.0, 2405.5, 2381.9)*1000,
 sal_30 = c(2425.9, 2402.9, 2379.9, 2356.9, 2333.8)*1000)

dens.ref   <- data.frame(
 temp    = seq(0, 40, by=10),
 sal_10 = c(1007.9, 1007.4, 1005.7, 1003.1,  999.7),
 sal_30 = c(1024.0, 1023.0, 1021.1, 1018.2, 1014.6))

## ----fig.width=9, fig.height=7------------------------------------------------
T.seq <- 0:40
S.seq <- seq(from = 0, to = 40, by = 10)

par(mfrow=c(2,3), las=1)
cpVals <- sapply(S.seq, FUN = function(s) 
                              water_cp(S = s, T_water = T.seq))
matplot(x = T.seq, y = cpVals, 
        main = "specific heat capacity", 
        xlab = "T, dgC", ylab = "J/kg/dgC", 
        type = "l", lwd = 2, lty = 1)
matpoints(x = cp.ref[,1], y = cp.ref[,-1], 
          pch = 18, col = c(2, 4), cex = 2)

tcVals <- sapply(S.seq, FUN = function(s) 
                              water_tc(S = s, T_water = T.seq))
matplot(x = T.seq, y = tcVals,
        main = "thermal conductivity",  
        xlab = "T, dgC", ylab = "W/m/dgC", 
        type = "l", lwd = 2, lty = 1)
matpoints(x = tc.ref[,1], y = tc.ref[,-1], 
          pch = 18, col = c(2, 4), cex = 2)

tdVals <- sapply(S.seq, FUN = function(s) 
                              water_td(S = s, T_water = T.seq))
matplot(x = T.seq, y = tdVals, 
        main = "thermal diffusivity", 
        xlab = "T, dgC", ylab = "m2/s", 
        type = "l", lwd = 2, lty = 1)
matpoints(x = td.ref[,1], y = td.ref[,-1], 
          pch = 18, col = c(2, 4), cex = 2)

lhVals <- sapply(S.seq, FUN = function(s) 
                              water_lh(S = s, T_water = T.seq))
matplot(x = T.seq, y = lhVals, 
        main = "latent heat varporization", 
        xlab = "T, dgC", ylab = "J/kg", 
        type = "l", lwd = 2, lty = 1)
matpoints(x=lh.ref[,1], y=lh.ref[,-1], 
          pch=18, col=c(2,4), cex=2)

ddVals <- sapply(S.seq, FUN = function(s) 
                              water_density(S = s, T_water = T.seq))
matplot(x = T.seq, y = ddVals, main = "density", 
        xlab = "T, dgC", ylab = "kg/m3", 
        type = "l", lwd = 2, lty = 1)
matpoints(x = dens.ref[,1], y = dens.ref[,-1], 
          pch = 18, col = c(2, 4), cex = 2)

plot.new()
legend("center", legend=S.seq, title="Salinity", col=1:5, lty=1, lwd=2)

## ----echo=FALSE, comment=NA---------------------------------------------------
Def_fun(bulk_density)
Def_fun(bulk_cp)
Def_fun(bulk_td)
Def_fun(bulk_properties)

## -----------------------------------------------------------------------------
attributes(bulk_properties())$description

## ----fig.width=9, fig.height=4------------------------------------------------
por.seq       <- seq(from = 0.,   to =    1, by = 0.2)
densSolid.seq <- seq(from = 2500, to = 2900, length.out = 50)

BD <- sapply(por.seq, FUN = function(p) 
                     bulk_density(porosity  = p, 
                                 density_solid = densSolid.seq) )
CP <- sapply(por.seq, FUN = function(p) 
                     bulk_cp     (porosity  = p, 
                                 density_solid = densSolid.seq) )
TD <- sapply(por.seq, FUN = function(p) 
                     bulk_td     (porosity  = p, 
                                 density_solid = densSolid.seq) )

par(mfrow=c(1, 3))
matplot(densSolid.seq, y = BD, 
        main = "bulk density",
        xlab = "density solid fraction, kg/m3", 
        ylab = "kg/m3", 
        type = "l", lty = 1, las = 1, lwd = 2)
matplot(densSolid.seq, y = CP, 
        main = "bulk specific heat capacity",
        xlab = "density solid fraction, kg/m3", 
        ylab = "J/kg/dg", 
        type = "l", lty = 1, las = 1, lwd = 2)
matplot(densSolid.seq, y = TD, 
        main = "bulk thermal diffusivity",
        xlab = "density solid fraction, kg/m3", 
        ylab = "m2/s", 
        type = "l", lty = 1, las = 1, lwd = 2)
legend("topleft", legend = por.seq, 
       title = "porosity", col = 1:6, lty = 1, lwd = 2)

## ----fig.width=6, fig.height=6------------------------------------------------
por.seq <- seq(from = 0, to = 1, length.out = 100)
cp.seq  <- seq(from = 500, to = 800, length.out = 200)

td.por  <- sapply(por.seq, FUN = function(x)
                   bulk_td(porosity = x ,
                          cp_solid  = cp.seq))
par(mar = c(4, 4, 4, 4))
image2D(x = cp.seq, y = por.seq, z = td.por, contour=TRUE,
        las = 1, xlab = "cp, J/kg/dg", ylab = "porosity", 
        main = "Thermal diffusivity", clab = "m2/s")

## -----------------------------------------------------------------------------
Def_fun(mineral_properties)
Def_fun(mineral_cp)
Def_fun(mineral_tc)
Def_fun(mineral_td)
Def_fun(mineral_density)

attributes(mineral_properties())$description

## -----------------------------------------------------------------------------
mineral_properties()

## ----echo=FALSE, comment=NA---------------------------------------------------
Def_fun(flux_heat)
Def_fun(flux_latent)
Def_fun(flux_sensible)
Def_fun(flux_backradiation)

attributes(flux_heat())$parameters

## -----------------------------------------------------------------------------
attributes(flux_heat())$description

## ----fig.width=8, fig.height=6------------------------------------------------
par(mfrow=c(2, 3), las=1)
T.seq     <- 0:30
Hsensible <- flux_sensible(T_air = T.seq)
plot(T.seq, Hsensible, 
     main = "Sensible heat exchange", 
     xlab = "Air temperature", ylab = "W/m2", 
     type = "l")

Hlatent   <- flux_latent(T_air = T.seq)
plot(T.seq, Hlatent, 
     main = "Latent heat exchange", 
     xlab = "Air temperature", ylab = "W/m2", 
     type = "l")

Q.seq   <- seq(0, 1, len=100)
Hlatent <- flux_latent(Qrel = Q.seq)
plot(Q.seq, Hlatent, 
     main = "Latent heat exchange", 
     xlab = "Relative Humidity", ylab = "W/m2", 
     type="l")

NLR1 <- flux_backradiation(T_air = T.seq, NLR = 1)
NLR2 <- flux_backradiation(T_air = T.seq, NLR = 2)
NLR3 <- flux_backradiation(T_air = T.seq, NLR = 3)
NLR4 <- flux_backradiation(T_air = T.seq, NLR = 4)
NLR5 <- flux_backradiation(T_air = T.seq, NLR = 5)
matplot(T.seq, cbind(NLR1, NLR2, NLR3, NLR4, NLR5), 
        type = "l", lty = 1, 
        main = "NLR heat exchange", 
        xlab = "Air temperature", ylab = "W/m2")

NLR1 <- flux_backradiation(T_sed = T.seq, NLR = 1)
NLR2 <- flux_backradiation(T_sed = T.seq, NLR = 2)
NLR3 <- flux_backradiation(T_sed = T.seq, NLR = 3)
NLR4 <- flux_backradiation(T_sed = T.seq, NLR = 4)
NLR5 <- flux_backradiation(T_sed = T.seq, NLR = 5)
matplot(T.seq, cbind(NLR1, NLR2, NLR3, NLR4, NLR5), 
        main = "NLR heat exchange", 
        xlab = "Sediment temperature", ylab = "W/m2",
        type = "l", lty = 1)

NLR1 <- flux_backradiation(Qrel = Q.seq, NLR = 1)
NLR2 <- flux_backradiation(Qrel = Q.seq, NLR = 2)
NLR3 <- flux_backradiation(Qrel = Q.seq, NLR = 3)
NLR4 <- flux_backradiation(Qrel = Q.seq, NLR = 4)
NLR5 <- flux_backradiation(Qrel = Q.seq, NLR = 5)

matplot(Q.seq, cbind(NLR1, NLR2, NLR3, NLR4, NLR5), 
        main = "NLR heat exchange", 
        xlab = "Relative Humidity", ylab = "W/m2", 
        type = "l", lty = 1)

legend("bottomright", col = 1:5, lty = 1, 
       legend= c("basic", "may86", "josey97", "bunker76", "bignami95"))

## ----fig.width=8, fig.height=6------------------------------------------------
T_sed <- seq(from = 0, to=30, length.out=50)
T_air <- seq(from = 0, to=30, length.out=50)

NLR1 <- outer(X = T_sed, Y = T_air, 
              FUN = function(X,Y) 
                    flux_backradiation(T_air = X, T_sed = Y, NLR = 1))
NLR2 <- outer(X = T_sed, Y = T_air, 
              FUN = function(X,Y) 
                    flux_backradiation(T_air = X, T_sed = Y, NLR = 2))
NLR3 <- outer(X = T_sed, Y = T_air, 
              FUN = function(X,Y) 
                    flux_backradiation(T_air = X, T_sed = Y, NLR = 3))
NLR4 <- outer(X=T_sed, Y=T_air, 
              FUN = function(X,Y) 
                    flux_backradiation(T_air = X, T_sed = Y, NLR = 4))
NLR5 <- outer(X=T_sed, Y=T_air, 
              FUN = function(X,Y) 
                    flux_backradiation(T_air = X, T_sed = Y, NLR = 5))

## ----fig.width=8, fig.height=6------------------------------------------------
par(mfrow=c(2,3))
clim <- c(-320, 110)
contour <- list(levels= c(0, -50,-100))

image2D(x = T_sed, y = T_air, z = NLR1, 
        xlab = "T_air", ylab = "T_sed", clim = clim, 
        contour = contour, main = "Basic")
image2D(x = T_sed, y = T_air, z = NLR2, 
        xlab = "T_air", ylab = "T_sed", clim = clim, 
        contour = contour, main = "May, 1986")
image2D(x = T_sed, y = T_air, z = NLR3, 
        xlab = "T_air", ylab = "T_sed", clim = clim, 
        contour = contour, main = "Clark et al., 1974")
image2D(x = T_sed, y = T_air, z = NLR4, 
        xlab = "T_air", ylab = "T_sed", clim = clim, 
        contour = contour, main = "Bunker, 1976")
image2D(x = T_sed, y = T_air, z = NLR5, 
        xlab = "T_air", ylab = "T_sed", clim = clim, 
        contour = contour, main = "Bignami et al., 1995")

NL <- as.data.frame(rbind(
            data.frame(NLR = "Basic",   val = as.vector(NLR1)), 
            data.frame(NLR = "May",     val = as.vector(NLR2)), 
            data.frame(NLR = "Clark",   val = as.vector(NLR3)),
            data.frame(NLR = "Bunker",  val = as.vector(NLR4)), 
            data.frame(NLR = "Bignami", val = as.vector(NLR5))))

par(las = 1, mar = c(4, 6, 2, 2))
boxplot(NL$val~NL$NLR, horizontal = TRUE, las = 1, 
        xlab = "value", ylab = "", main = "Backradiation")

## ----fig.width=8, fig.height=6------------------------------------------------
Qrel  <- seq(from=0, to=1,length.out=50)
Cloud <- seq(from=0, to=1,length.out=50)

NLR1 <- outer(X = Cloud, Y = Qrel, 
              FUN = function(X, Y) 
                    flux_backradiation(Cloud = X, Qrel = Y, NLR = 1))
NLR2 <- outer(X = Cloud, Y = Qrel, 
              FUN = function(X, Y) 
                    flux_backradiation(Cloud = X, Qrel = Y, NLR = 2))
NLR3 <- outer(X = Cloud, Y = Qrel, 
              FUN = function(X, Y) 
                    flux_backradiation(Cloud = X, Qrel = Y, NLR = 3))
NLR4 <- outer(X = Cloud, Y = Qrel, 
              FUN = function(X, Y) 
                    flux_backradiation(Cloud = X, Qrel = Y, NLR = 4))
NLR5 <- outer(X = Cloud, Y = Qrel, 
              FUN = function(X,Y) 
                    flux_backradiation(Cloud = X, Qrel = Y, NLR = 5))

## ----fig.width=8, fig.height=6------------------------------------------------
par(mfrow = c(2,3), las = 1)
zlim <- c(-150, -20)
contour=list(levels = c(0, -50,-100))

image2D(x = Cloud, y = Qrel, z = NLR1, 
        xlab = "Cloudiness", ylab = "Humidity", 
        contour = contour, main = "Basic", zlim = zlim)
image2D(x = Cloud, y = Qrel, z = NLR2, 
        xlab = "Cloudiness", ylab = "Humidity", 
        contour = contour, main = "May, 1986", zlim = zlim)
image2D(x = Cloud, y = Qrel, z = NLR3, 
        xlab = "Cloudiness", ylab = "Humidity", 
        contour = contour, main = "Clark et al., 1974", zlim = zlim)
image2D(x = Cloud, y = Qrel, z = NLR4, 
        xlab = "Cloudiness", ylab = "Humidity", 
        contour = contour, main = "Bunker, 1976", zlim = zlim)
image2D(x = Cloud, y = Qrel, z = NLR5, 
        xlab = "Cloudiness", ylab = "Humidity", 
        contour = contour, main = "Bignami et al. 1995", zlim = zlim)

NL <- as.data.frame(rbind (
            data.frame(NLR = "Basic",   val = as.vector(NLR1)), 
            data.frame(NLR = "May",     val = as.vector(NLR2)), 
            data.frame(NLR = "Clark",   val = as.vector(NLR3)),
            data.frame(NLR = "Bunker",  val = as.vector(NLR4)), 
            data.frame(NLR = "Bignami", val = as.vector(NLR5))))

par(las = 1, mar = c(4, 6, 2, 2))
boxplot(NL$val~NL$NLR, 
        horizontal = TRUE, las = 1, 
        xlab = "value", ylab = "", main = "Backradiation")

## -----------------------------------------------------------------------------
# Forcing function data for the Oosterschelde
attributes(ForcingOS)$description 
head(ForcingOS, n = 3)

# Temperature data for a station in the Oosterschelde
head(DataOS, n = 3)

# initial temperature profile
head(InitTempOS, n = 3)  

## ----echo=FALSE, comment=NA---------------------------------------------------
Def_fun(TempSED_run1D)

## -----------------------------------------------------------------------------
z_max    <- 10
dz_1     <- 1e-4

# function describing the variation of porosity (volume fraction of LIQUID) with depth
porosity  <- function(x, por.SWI = 0.9, por.deep = 0.7, porcoef = 100)
    return( por.deep + (por.SWI-por.deep)*exp(-x*porcoef) )

## -----------------------------------------------------------------------------
fAirTempYr  <- function(t) 
               return(15   +  15*sin(2*pi*t/86400/365))  # dgC
fSolarRadYr <- function(t) 
               return(150  + 120*sin(2*pi*t/86400/365))  # W/m2

## -----------------------------------------------------------------------------
times <- seq(from = 0, to = 365*86400, by = 86400)

ToutYr.ini <- TempSED_run1D(
                        z_max            = z_max, 
                        dz_1             = dz_1, 
                        porosity         = porosity, 
                        times            = times, 
                        T_ini            = 10, 
                        f_Airtemperature = fAirTempYr, 
                        f_Solarradiation = fSolarRadYr)

ToutYr     <- TempSED_run1D(
                        z_max            = z_max, 
                        dz_1             = dz_1, 
                        porosity         = porosity, 
                        times            = times, 
                        T_ini            = ToutYr.ini, 
                        f_Airtemperature = fAirTempYr, 
                        f_Solarradiation = fSolarRadYr)

## ----fig.width=6, fig.height=6, warning=FALSE---------------------------------
par(mfrow = c(2, 2), las = 1, oma = c(0,0,2,0))

T_air <- subset(ToutYr, which="Airtemperature")
lines2D(x = times/86400, y = T_air,
        main = "air temperature", 
        colvar = T_air, type = "l", lwd = 2)

Rad <- subset(ToutYr, which = "Solarradiation")
lines2D(x = times/86400, y = Rad,
        main = "Radiation", 
        colvar = Rad, type = "l", lwd = 2)

image2D(ToutYr, main = "Temperature", xlab = "days",
      clab = "dgC", ylab = "m", time_unit = "day",
      contour = TRUE, mfrow = NULL)

matplot1D(ToutYr, 
          subset = seq(1, nrow(ToutYr), by = 15),
          mfrow = NULL, 
          main = "Temperature profile", 
          xlab = "temperature", ylab = "m",
          type = "l", lwd = 1, lty = 1, col = "grey")
mtext(outer = TRUE, side = 3, "Seasonal variation")

## ----fig.width=6,fig.height=8-------------------------------------------------
par(mfrow = c(2, 1))
matplot0D(ToutYr, time_unit = "day",
          which = c("Heatflux_latent", "Heatflux_sensible",
              "Heatflux_backrad", "RadiationSWI", "Heatflux_total"),
          main = "Fluxes", xlab = "day", ylab = "W/m2", 
          lty = 1, lwd = 2)

matplot0D(ToutYr, time_unit = "day",
           which = c("Tsed_mean","Airtemperature"), 
           main = "Temperature", xlab = "day", ylab = "dg", 
           lty = 1, lwd = 2)
mtext(outer = TRUE, side = 3, "Seasonal variation-exposed to air")

## -----------------------------------------------------------------------------
times <- seq(from=0, to = 365*86400, by = 86400)

ToutYr2.ini <- TempSED_run1D(
                         z_max              = z_max, 
                         dz_1               = dz_1, 
                         porosity           = porosity, 
                         times              = times, 
                         T_ini              = 10, 
                         f_Watertemperature = fWaterTempYr, 
                         f_Waterheight      = 1,
                         f_Solarradiation   = fSolarRadYr)

ToutYr2     <- TempSED_run1D(
                         z_max              = z_max, 
                         dz_1               = dz_1, 
                         porosity           = porosity, 
                         times              = times, 
                         T_ini              = ToutYr2.ini, 
                         f_Watertemperature = fWaterTempYr, 
                         f_Waterheight      = 1,
                         f_Solarradiation   = fSolarRadYr)


## ----fig.width=6,fig.height=6, warnings=FALSE---------------------------------
par(mfrow = c(2, 2), las = 1, oma = c(0,0,2,0))

Twat <- subset(ToutYr2, which = "Watertemperature")
lines2D(x = times/86400, y = Twat, colvar = Twat,
        main = "Water temperature", 
        type = "l", lwd = 2)

Rad <- subset(ToutYr2, which = "Solarradiation")
lines2D(x = times/86400, y = Rad, colvar = Rad,
        main = "Radiation", 
        type = "l", lwd = 2)

image2D(ToutYr2, time_unit = "day",
        main = "Temperature", xlab = "days",
        clab = "dgC", ylab = "m", 
        contour = TRUE, mfrow = NULL)

matplot1D(ToutYr2,  
           subset = seq(1, nrow(ToutYr2), by = 15), 
           mfrow = NULL,  
           main = "Temperature profile", 
           xlab = "temperature", ylab = "m",
           type = "l", lwd = 1, lty = 1, col="grey")
mtext(outer = TRUE, side = 3, "Seasonal variation-submerged")

## ----fig.width=6,fig.height=10------------------------------------------------
par(mfrow = c(2,1))
matplot0D(ToutYr2, time_unit = "day",
           which = c("Heatflux_convection", "Heatflux_backrad",
                     "RadiationSWI", "Heatflux_total"), 
           main = "Fluxes", ylab = "W/m2", xlab = "day",
           lty = 1, lwd = 2)

matplot0D(ToutYr2, time_unit = "day",
           which = c("Tsed_mean", "Watertemperature"), 
           main = "Temperature", ylab = "dg", xlab = "day",
           lty = 1, lwd = 2)
mtext(outer = TRUE, side = 3, "Seasonal variation-submerged")

## -----------------------------------------------------------------------------
head(ForcingOS)
head(DataOS)

## -----------------------------------------------------------------------------
nap   <- 0.5   # [m] position of the sampling above NAP

WaterHeight <- data.frame(Time   = ForcingOS$Time, 
                          height = pmax(0, ForcingOS$Waterheight + nap)) 

# porosity function (varies from 0.49 to 0.54)
por_fun <- function(x)  # x = depth in m
  0.49 + (0.54 - 0.49)*exp(-100*x)

# initial temperature: a good profile was created, and used here with approx
Tini_fun <- function(x) approx(InitTempOS, xout = x, rule = 2)$y

times <- as.double(seq(from = min(ForcingOS$Time), 
                       to   = max(ForcingOS$Time), 
                       by   = 3600))

OSout <- TempSED_run1D(
           times              = times, 
           T_ini              = Tini_fun, 
           porosity           = por_fun, 
           f_Waterheight      = WaterHeight, 
           f_Watertemperature = ForcingOS[, c("Time", "Watertemperature")], 
           f_Solarradiation   = ForcingOS[, c("Time", "Solarradiation")], 
           f_Windspeed        = ForcingOS[, c("Time", "Windspeed")], 
           f_Airtemperature   = ForcingOS[, c("Time", "Airtemperature")], 
           f_Cloudiness       = ForcingOS[, c("Time", "Cloudiness")],
           parms              = list(kd_water = 0.1, albedo_sediment = 0.3),
           sedpos             = c(0.03, 0.06, 0.09))

## ----fig.width=10, fig.height=12----------------------------------------------
Tdata <- data.frame(var   = paste("Tsed", DataOS$Depth_cm, sep="_0.0"),
                    time  = as.numeric(DataOS$Time),
                    value = DataOS$SedimentTemperature)
plot(OSout, mfrow = c(3, 1), which = unique(Tdata$var),
     obs = Tdata, obspar = list(pch = ".", col = "red", cex=2))

## ----fig.width=10, fig.height=12,warning = FALSE, message = FALSE-------------
image2D(OSout, time.origin = "1970-01-01", mfrow = NULL, las = 1)

## ----fig.width=10, fig.height=12, warning = FALSE, message = FALSE------------
par(mfrow = c(4, 2), mar = c(4, 4, 3, 2), las = 1)
OSday <- OSout
OSday[,1] <- OSout[,1]/86400 - 18261

xlim <- c("2020-02-01", "2020-03-01")
image2D(OSout, xlim = as.POSIXct(xlim), 
        ylim = c(1, 0), time.origin = "1970-01-01", 
        xlab = "day", mfrow = NULL)
plot(OSday, which = "Tsed_0.03", 
     xlim = as.POSIXlt(xlim)$yday, 
     xlab = "day", mfrow = NULL)

xlim <- c("2020-05-01", "2020-06-01")
image2D(OSout, xlim = as.POSIXct(xlim), 
        ylim = c(1, 0), time.origin = "1970-01-01", 
        xlab = "day", mfrow = NULL)
plot(OSday, which = "Tsed_0.03", 
     xlim = as.POSIXlt(xlim)$yday, 
     xlab = "day", mfrow = NULL)

xlim <- c("2020-08-01", "2020-09-01")
image2D(OSout, xlim = as.POSIXct(xlim), 
        ylim = c(1, 0), time.origin = "1970-01-01", 
        xlab = "day", mfrow = NULL)
plot(OSday, which = "Tsed_0.03", 
     xlim = as.POSIXlt(xlim)$yday, 
     xlab = "day", mfrow = NULL)

xlim <- c("2020-11-01", "2020-12-01")
image2D(OSout, xlim = as.POSIXct(xlim), 
        ylim = c(1, 0), time.origin = "1970-01-01", 
        xlab = "day", mfrow = NULL)
plot(OSday, which = "Tsed_0.03", 
     xlim = as.POSIXlt(xlim)$yday, 
     xlab = "day", mfrow = NULL)

