!**************************************************************************
!**************************************************************************
!********************    Sediment temperature in 1D     *******************
!**************************************************************************
!**************************************************************************

!**************************************************************************
! module with dimensions
!**************************************************************************

MODULE dimTemp1D

 INTEGER, PARAMETER :: nx = 100                   ! number of vertical boxes
 INTEGER, PARAMETER :: npars  = 18 + 4*nx + nx+1  ! number of parameters
 INTEGER, PARAMETER :: nforcs = 9                 ! number of forcing functions
 INTEGER, PARAMETER :: nout   = 18                ! number of output variables

END MODULE dimTemp1D

!**************************************************************************
! Module with parameters, forcing functions, state variables
!**************************************************************************

MODULE parTemp1D
 USE dimTemp1D
 USE Heatconstants

 IMPLICIT NONE

!=====================
! Forcing functions
!=====================

 DOUBLE PRECISION ::                                                            &
   fWaterHeight,      & ! [m]       height of overlying water
   fWaterTemperature, & ! [degC]    temperature of overlying water
   fAirTemperature,   & ! [degC]    temperature of the air
   fAirHumidity,      & ! [-]       air humidity (fraction saturation)
   fPressure,         & ! [Pa]      air pressure
   fSolarRadiation,   & ! [W/m2]    solar radiation (short wavelength)
   fWindSpeed,        & ! [m/s]     Wind speed
   fCloudiness,       & ! [-]       cloud cover
   fDeepTemperature     ! [degC]    deep water temperature

 COMMON /forcsTemp1d/ fWaterHeight, fWaterTemperature, fAirTemperature,         &
&    fAirHumidity, fPressure, fSolarRadiation, fWindSpeed, fCloudiness,         &
&    fDeepTemperature

!=====================
! parameters
!=====================

 DOUBLE PRECISION ::                                                            &
   S,                 & ! [-]        salinity   
   emAir,             & ! [-]        emissivity of Air
   emSed,             & ! [-]        emissivity of Water/sediment
   albedoWat,         & ! [-]        part light refracted by water
   albedoSed,         & ! [-]        part light refracted by sediment
   kdWater,           & ! [/m]       light attenuation coeff water
   kdSed,             & ! [/m]       light attenuation coeff (bulk) sediment
   cpWat,             & ! [J/kg/K]   specific heat capacity water (parameter)
   cpSolid,           & ! [J/kg/K]   specific heat capacity solid sediment
   tcWat,             & ! [W/m/K]    thermal conductivity of water (parameter)
   tcSolid,           & ! [W/m/K]    thermal conductivity of dry sed
   densWat,           & ! [kg/m3]    reference seawater density (parameter)
   densSolid,         & ! [kg/m3]    sediment dry density
   stanton,           & ! [-]        transfer coeff for sensible heat
   dalton,            & ! [-]        transfer coeff for latent heat
   
   dependencyT,       & ! [-]        switch for temperature dependency of all pars
   deepBC,            & ! [-]        deep boundary condition (1=flux, 2=conc, 3=zero-grad)

   dx(nx),            & ! [m]        box thickness
   dxInt(nx+1),       & ! [m]        distance from mid-mid
   porosity(nx),      & ! [-]        volume water/volume bulk
   porint(nx+1),      & ! [-]        porosity at box interface
   Irrig(nx)            ! [/d]       irrigation rate

 COMMON /parsTemp1d/S, emAir, emSed, albedoWat, albedoSed,                      &
&                  kdWater, kdSed, cpWat, cpSolid,  tcWat, tcSolid,             &
&                  densWat, densSolid,  stanton, dalton,                        &
&                  dependencyT, deepBC,                                         &
&                  porosity, porint, Irrig, dx, dxInt

 INTEGER :: BCup, BCdown   ! boundary conditions (1=flux, 2=conc, 3=0-grad)
 INTEGER :: NLRused    ! 

!=====================
! output variables
!=====================

 DOUBLE PRECISION ::                                                            &
   Heightwater,        & ! [m]       Height of overlying water
   Twater,             & ! [degC]    Temperature of overlying water
   Tair,               & ! [degC]    Temperature of the air
   Humidity,           & ! [-]       Humidity of the air (part saturation)
   P,                  & ! [Pa]      Air pressure
   Radiation,          & ! [W/m2]    Shortwave radiation
   RadiationSWI,       & ! [W/m2]    Radiation at sediment-water interface
   Wind,               & ! [m/s]     Wind speed
   AirHeatFlux,        & ! [W/m2]    total heat flux with air
   EvaporationRate,    & ! [kg/m2/s] rate of evaporation
   Htotal,             & ! [W/m2]    total heat flux (upper boundary+radiation)
   HconvectionWater,   & ! [W/m2]    convective heat flux with overlying water
   Hradiation,         & ! [W/m2]    shortwave radiation heat input
   Hlatent,            & ! [W/m2]    latent heat input (~evaporation)
   Hsensible,          & ! [W/m2]    sensible heat input (~air temperature)
   BackRadiation,      & ! [W/m2]    net longwave radiation (net backradation)
   totalIrr,           & ! [W/m2]    heat input due to irrigation
   Tsed                  ! [degC]    Mean bulk sediment Temperature

  COMMON /outTemp1d/Twater, Heightwater, Tair, Humidity, P,                     &
&    Radiation,  Wind, RadiationSWI, EvaporationRate, AirHeatFlux, Htotal,      &
&    HconvectionWater, Hradiation, Hlatent, Hsensible, BackRadiation,           &
&    TotalIrr, Tsed 

  DOUBLE PRECISION :: densWater(nx), cpWater(nx)  ! box center
  DOUBLE PRECISION :: tcWater(nx+1)   ! thermal conductivity, at interface

  DOUBLE PRECISION :: densbulk(nx), cpBulk(nx)  ! box center
  DOUBLE PRECISION :: tcBulk(nx+1)   ! thermal conductivity, at interface
  DOUBLE PRECISION :: HFlux(nx+1)    ! enthalpy flux [W/m2]
  DOUBLE PRECISION :: Aint(nx+1)     ! surface area at the box interface [m2]
END MODULE parTemp1D

!==========================================================================
!==========================================================================
! subroutine calculating the rate of change of temperature in 1D
! Bottom water temperature is imposed.
!==========================================================================
!==========================================================================

SUBROUTINE modtemp1d (neq, t, Temperature, dTemperature, yout, ip)

 USE partemp1D
 IMPLICIT NONE

 INTEGER           :: neq       ! number of equations
 INTEGER           :: ip(*), i
 DOUBLE PRECISION  :: t, Temperature(neq), dTemperature(nx), yout(*)

 DOUBLE PRECISION :: Cloud, Fluxup, Tup, Tdown, RadTop, RadBot 

 INTEGER :: irrigate(neq)

!..........................................................................

! output variables for forcing functions
  Twater      = fWaterTemperature   ! [degC]
  Heightwater = fWaterHeight        ! [m]
  Tair        = fAirTemperature     ! [degC]
  Humidity    = fAirHumidity        ! [-]
  P           = fPressure           ! [Pa]
  Radiation   = fSolarRadiation     ! [W/m2]
  Wind        = fWindSpeed          ! [m/s]
  Cloud       = fCloudiness         ! [-]
  Tdown       = fDeepTemperature    ! only used when BCdown = 2

! ==================================
! bulk properties
! if switch dependencyT: 
! water properties are updated each time step
! ==================================

  IF (dependencyT > 0.5d0) THEN
    CALL calccpwater (nx,    Temperature, S, cpWater)
    CALL calcrhowater(nx,    Temperature, S, densWater)
    CALL calctcwater (nx, 1, Temperature, S, P, tcWater)

    CALL calcBulk(nx, densWater, densSolid, cpWater, cpSolid,                   &
&                 tcWater, tcSolid, porosity, porint,                           &
&                 densBulk, cpBulk, tcBulk)
  END IF
   
! ==================================
! output variable
! ==================================
  Tsed      = sum(Temperature*dx)/sum(dx)               ! mean temperature

! ==================================
! transport and surface exchange
! ==================================


  IF (Heightwater > 0) THEN           

! ***  submerged => exchange with overlying water    ***

    BcUp   = 2      ! upper boundary condition: imposed value
  	Tup    = Twater ! value of upper boundary temperature

    ! Transport of temperature amongst layers and with overlying water
    ! Note: tcBulk is thermal conductivity; Hflux is enthalpy flux
    CALL TranTmp1d(nx, Temperature, Tup, Tdown, BcUp, BcDown, tcBulk,           &
&                  irrig, dx, dxInt, porosity, Aint, cpBulk, densbulk,          &
&                  densWater, HFlux, dTemperature, totalIrr)

    HconvectionWater = HFlux(1)      ! W/m2

    ! No exchange with air
    AirHeatFlux       = 0.d0
    EvaporationRate   = 0.d0
    Hlatent           = 0.d0        ! W/m2, latent heat input
    Hsensible         = 0.d0        ! W/m2, sensible heat input
    BackRadiation     = 0.d0        ! W/m2, net longwave radiation

    ! Light at the sediment-water interface
    RadiationSWI = Radiation*(1.d0-albedoWat)*exp(-kdWater*Heightwater)
    RadiationSWI = RadiationSWI*(1.d0-albedoSed)
  ELSE

! ***   dry =>  exchange with overlying air - moist sediment   ***

    ! atmospheric heat exchange
    
    CALL heatflux (Tair, Temperature(1), P, Wind, Cloud, Humidity,              &
          emAir, emSed, dalton, stanton, porosity(1),                           &
          3, EvaporationRate, Hlatent, Hsensible, BackRadiation)

    ! No exchange with water
    HconvectionWater = 0.d0

    ! Light at sediment-air interface
    RadiationSWI = Radiation*(1.d0-albedoSed)

    ! Total heat flux and temperature flux (FluxUp)
    AirHeatFlux = Hlatent + Hsensible + BackRadiation

    ! set boundary conditions
    BcUp        = 1             ! upper boundary condition: imposed flux
    FluxUp      = AirHeatFlux   ! enthalpy flux

    ! Transport of temperature amongst layers, flux upper boundary
    CALL TranTmp1d(nx, Temperature, Fluxup, Tdown, BcUp, BcDown, tcBulk,        &
&                  irrig, dx, dxInt, porosity, Aint, cpBulk, densbulk,          &
&                  densWater, HFlux, dTemperature, totalIrr)

  END IF

  Hradiation = RadiationSWI  ! [W/m2], shortwave radiation heat input

  Htotal     = AirHeatFlux + HconvectionWater + HRadiation

! ==================================
! Light absorption
! ==================================

  RadTop = RadiationSWI  ! Light at the top of sediment layers
  
  DO i = 1, nx
   RadBot = RadTop*dexp(-kdSed*dx(i))  ! dx(i) = thickness layer i
   dTemperature(i) = dTemperature(i) +                                          &
                    (RadTop-RadBot)/(cpBulk(i)*densbulk(i))/dx(i)
 	 RadTop = RadBot
  END DO

  CALL getOutput1d (yout)

END SUBROUTINE modtemp1d

! =============================================================================
! Save the output from common block to vector accessible by R
! =============================================================================

SUBROUTINE getOutput1d(yout)
 USE dimTemp1D
 DOUBLE PRECISION, INTENT (INOUT) :: yout(*)
 DOUBLE PRECISION :: output(nout)
 INTEGER :: i
 COMMON /outTemp1d/output

 DO i = 1, nout
  yout(i) = output(i)
 END DO

END SUBROUTINE getOutput1d

! =============================================================================
! =============================================================================
!                                Interface to R
! =============================================================================
! =============================================================================

!-------------------------------------------------------------------------------
! Set parameters from R to Fortran
!-------------------------------------------------------------------------------

SUBROUTINE inittemp1d (setparms)
USE dimTemp1D

IMPLICIT NONE
EXTERNAL setparms

DOUBLE PRECISION parms(npars)
COMMON /parsTemp1d/parms

  CALL setparms(npars, parms)
  CALL InitialiseBulk
  
RETURN
END SUBROUTINE inittemp1d

! =============================================================================

SUBROUTINE InitialiseBulk

!-------------------------------------------------------------------------------
! initialise bulk properties if no dependency on temperature at each time step
!-------------------------------------------------------------------------------

USE parTemp1D

IMPLICIT NONE

! For now: assume that surface area is a constant
  Aint(:) = 1.D0
  
  IF (dependencyT < 0.5d0) THEN
    cpWater(:) = cpWat
    tcWater(:) = tcWat
    densWater(:) = densWat
    CALL calcBulk(nx, densWater, densSolid, cpWater, cpSolid,                   &
&                 tcWater, tcSolid, porosity, porint,                           &
&                 densBulk, cpBulk, tcBulk)
  END IF

  ! set boundary conditions
  BcDown    = INT(deepBC + 0.1) ! lower boundary condition: zero gradient/imposed 
  NLRused   = 3

END SUBROUTINE InitialiseBulk

! =============================================================================

!-------------------------------------------------------------------------------
! Set forcings from R to Fortran
!-------------------------------------------------------------------------------

SUBROUTINE forctemp1d (setforcs)
USE dimTemp1D

IMPLICIT NONE
EXTERNAL setforcs

DOUBLE PRECISION forcs(nforcs)
COMMON /forcsTemp1d/forcs

  CALL setforcs(nforcs, forcs)

RETURN
END SUBROUTINE forctemp1d

! =============================================================================
!==============================================================================
! Diffusion in a 1-dimensional finite difference grid
!==============================================================================
! =============================================================================

SUBROUTINE tranTmp1d (nx, TC, Tup, Tdown, BcUp, BcDown, cond, Dirr,             &
&            dx, dxint, porosity, Aint, cpBulk, densbulk, densWater,            &
&            HFlux, dT, irrf)

IMPLICIT NONE
INTEGER, INTENT(IN)          :: nx       ! length of TC
DOUBLE PRECISION, INTENT(IN) :: TC(nx)   ! temperature in degC

! boundary conditions (1= flux, 2=conc, 3 = 0-grad)
INTEGER, INTENT(IN) ::  BcUp, BcDown

! Boundary values (used if Bc.. = 2,4), fluxes (used if Bc= 1)
DOUBLE PRECISION, INTENT(IN) ::                                                 &
     Tup, Tdown,     &   ! either boundary temperature or enthalpy flux (W/m2)            
     densWater(nx),  &   ! density of seawater    
     cond(nx+1),     &   ! Thermal conductivity, [W/m/K]
     Dirr(nx),       &   ! irrigation rate, [\s]
     porosity(nx),   &   ! volumetric porosity [-]
     Aint(nx+1),     &   ! surface area at the box interface [m2]
     cpBulk(nx),     &   ! specific heat capacity [J/kg/K] of bulk material
     densBulk(nx),   &   ! density [kg/m3] of bulk material
     dx(nx), dxint(nx+1) ! grid size, distance from mid to mid [m]

! output: fluxes and rate of change
DOUBLE PRECISION, INTENT(OUT) ::                                                &
     HFlux(nx+1),    &   ! enthalpy flux (W/m2)   
     dT(nx),         &   ! rate of change of temperature [degC/s]
     irrf                ! summed irrigation

! locals
INTEGER :: i
DOUBLE PRECISION :: irrigation, Amid

! -------------------------------------------------------------------------------

! ----------------------------------------------
! enthalpy Fluxes [W/m2] 
! Hflux = cond     * dC  /dx 
! units: [W/m/K] * [degC] /[m] = [W/m2]
! ----------------------------------------------
    
! internal cells
    DO i = 2,nx  
     HFlux(i) = -cond(i) * (TC(i)-TC(i-1)) /dxint(i)
    END DO

! upstream boundary
    IF (BcUp .EQ. 1) THEN        ! flux
      HFlux(1) = Tup
      
    ELSE IF (BcUp .EQ. 2) THEN   ! conc
      HFlux(1) = -cond(1) * (TC(1)-Tup) /dxint(1)
      
    ELSE
      HFlux(1) = 0.D0
    END IF

! downstream boundary
    IF (BcDown .EQ. 1) THEN
      HFlux(nx+1) = Tdown
      
    ELSE IF (BcDown .EQ. 2) THEN
      HFlux(nx+1) = -cond(nx+1) * (Tdown-TC(nx)) /dxint(nx+1)
      
    ELSE
      HFlux(nx+1) = 0.D0
    END IF

! ----------------------------------------------
! Rate of change of temperature
! negative flux gradient /cpBulk/densBulk => [degC/s]
! ----------------------------------------------

    DO i = 1,nx
     Amid  = 0.5 * (Aint(i)+Aint(i+1))
     dT(i) = -(Aint(i+1)*HFlux(i+1) - Aint(i)*HFlux(i))                         &
               /Amid/dx(i)/cpBulk(i)/densBulk(i)
    END DO

! ----------------------------------------------
! bioirrigation - assumes bulk and liquid the same temperature
! (TC is defined in bulk sediment). 
!                           !! NEEDS ADAPTATION!!!
! ----------------------------------------------
    irrf = 0.d0
    DO i = 1, nx
     irrigation = Dirr(i)*(Tup - TC(i))  ! 
     irrf = irrf + Irrigation*cpBulk(i)*densWater(i)*dx(i)*porosity(i)
     dT(i) = dT(i) + irrigation*porosity(i)  ! per bulk
    END DO

RETURN
END SUBROUTINE tranTmp1d

