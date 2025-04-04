---
editor_options: 
  markdown: 
    wrap: 72
title: TempSED, An R package to model temperature variations in aquatic sediments.
description: >
   TempSED is an R-package that includes a one-dimensional mechanistic
   model to describe space-time variations in temperature in aquatic
   sediments that are exposed to the air or overlying water. 
   It is designed to model temperature in sediments under variable
   atmospheric and water-column forcing, i.e. for intertidal 
   (estuarine / marine) sediments. Its vertical resolution is very 
   fine near the sediment-water interface and coarsening with depth. 
---

# <img src="man/figures/TempSEDlogo.png" align="center" alt="TempSED"/>

<center>

<br>

<h4>Karline Soetaert, Lubos Polerecky and Qi Liu</h4>

<br>

<h5>Netherlands Institute for Sea Research</h5>
<h5>Utrecht University</h5>

<br>


[click here to go to the website](https://github.com/TempSED/TempSED.github.io)


## Installation

Before installing the *TempSED* package, you need to first install the following R-packages and their dependencies:

* *deSolve*, *rootSolve*, *ReacTran* (required for solving the reaction-transport model in R);
* *plot3D* (required for image plotting);

Once the above packages have been installed, enter the following command in the R-console to install the *TempSED* package:

```
devtools::install_github("dynamic-R/TempSED", depend=TRUE)
```
Then, type ``require(TempSED)`` in the R-console to load the package in R. 

That's it! After this step, you should be able to use the package as described below.

## What can you do with the TempSED package?

### Calculate sediment temperature

Given (variable or constant) atmospheric conditions (air temperature, relative humidity, pressure, solar radiation, wind speed and cloudiness), and water height and temperature, function *TempSED_run1D* estimates the heat gain or losses between the air-sediment or water-sediment interface, and the propagation of heat across the sediment. Important parameters are emissivity of air and sediment, sediment porosity and specific heat capacity and thermal conductivity of water and solid.  

To run a specific tutorial, for example the tutorial called "introduction", enter one of the following commands in the R-console:

```
?TempSED_run1D
example("TempSED_run1D")
```

### Calculate properties of air, water, sediment

Properties that relate to sediment temperature modelling can be calculated, i.e. density, specific heat capacity, thermal conductivity, and thermal diffusivity. 
Other formulae estimate the vapor pressure and specific humidity of moist air. 

```
example("air_properties")
example("water_properties")
example("mineral_properties")
example("bulk_properties")
```

### Estimate heat fluxes between air and sediment

Latent heat is the heat lost when the fluid evaporates.

Sensible heat is the heat required to change the temperature without changing the phase.

Backradiation is the amount of heat radiated from the atmosphere to the surface.

```
?flux_heat
example("flux_heat")
```

### Data set from the Oosterschelde

A data set with actual measurements from an intertidal area in the Oosterschelde, together with the atmospheric conditions and water heights and temperature is provided

```
?DataOS
example("DataOS")
```

## Uninstall

To uninstall the *TempSED* package, locate it in the "Packages" tab in *Rstudio*, and click on the encircled "x" button ("Remove package") on the right margin.

To uninstall the package manually, type the following command in the R-console (the version of the library may differ, here we assume 4.1):

* Windows users: 
  ```
  remove.packages("TempSED", lib="~/R/win-library/4.1")
  ```
* Linux users: 
  ```
  remove.packages("TempSED", lib="~/R/x86_64-pc-linux-gnu-library/4.1")
  ```

---
Last updates: 2025-03-21
