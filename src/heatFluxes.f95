
!**************************************************************************
! Estimates all heat fluxes
!**************************************************************************
! -------------------------------------------------------------------------
! Guarini et al, Marine Ecology Progress Series 153, 25-36.
! Modelling the mud surface temperature on intertidal
! flats to investigate the spatio-temporal dynamics of
! the benthic microalgal photosynthetic capacity
! -------------------------------------------------------------------------

SUBROUTINE heatflux(Tair, Tsed, P, Wind, Cloud, Qrel,                           &
                    emAir, emSed, dalton, stanton, por, NLR,                    &
                    EvaporationRate, Hlatent, Hsensible, BackRadiation)

 USE Heatconstants
 IMPLICIT NONE

 DOUBLE PRECISION, INTENT (IN) ::                                               &
      Tair ,          &    ! [dgC]     air temperature
      Tsed ,          &    ! [dgC]     temp sediment surface water
      P    ,          &    ! [Pa]      air pressure,
      Wind ,          &    ! [m/s]     wind speed
      Cloud,          &    ! [-]       relative fraction cloud cover
      Qrel ,          &    ! [-]       relative air humidity (0-1)
      emAir,          &    ! [-]       emissivity of Air
      emSed,          &    ! [-]       emissivity of Water/sediment
      dalton,         &    ! [-]       transfer coeff for latent heat
      stanton,        &    ! [-]       transfer coeff for sensible heat
      por                  ! [-]       volume fraction water at interface

 INTEGER, INTENT (IN)          :: NLR  ! how to estimate backradiation

 DOUBLE PRECISION, INTENT(OUT) ::                                               &
      Hlatent,        &    ! [W/m2]    LATENT heat flux (due to evaporation)
      Hsensible,      &    ! [W/m2]    SENSIBLE heat flux (due to conduction)
      BackRadiation,  &    ! [W/m2]    Backradiation, net longwave radiation
      EvaporationRate      ! [kg/m2/s] rate of evaporation

! local variables

 DOUBLE PRECISION :: Qair, RHOair, Vapor

!..........................................................................
  CALL airProperties(Tair,  P, Qrel,                                            &
                     Qair, RHOair, Vapor)

  CALL latentHeat(Tsed, P, Wind, por, Qair, RHOair, dalton,                     &
                  EvaporationRate, Hlatent)

  CALL sensibleHeat(Tair, Tsed, Wind, Qair, RHOair, stanton,                    &
                    Hsensible)

  CALL Backrad (Tair, Tsed, Cloud, Vapor, emAir, emSed, NLR,                    &
                backradiation)

END SUBROUTINE heatflux

!**************************************************************************
! The latent heat flux
!**************************************************************************

SUBROUTINE latentheat(Tsed, P, Wind, por, Qair, RHOair, dalton,                 &
                      EvaporationRate, Hlatent)

 USE Heatconstants
 IMPLICIT NONE

 DOUBLE PRECISION, INTENT (IN) ::                                               &
      Tsed ,          &   ! [dgC]     temp surface water sediment
      P    ,          &   ! [Pa]      air pressure,
      Wind ,          &   ! [m/s]     wind speed
      Qair,           &   ! [kg/kg]   specific humidity of the air
      RhoAir,         &   ! [kg/m3]   air density
      dalton,         &   ! [-]       transfer coeff for latent heat
      por                 ! [-]       volume fraction water at interface

 DOUBLE PRECISION, INTENT(OUT) ::                                               &
      Hlatent,        &   ! [W/m2]    LATENT heat flux (due to evaporation)
      EvaporationRate     ! [kg/m2/s] rate of evaporation

 DOUBLE PRECISION :: PsaTsed, QsaTsed
 DOUBLE PRECISION :: TsedK
 DOUBLE PRECISION :: LatentHeatEvap
 DOUBLE PRECISION :: airSatVapor

! Temperature in Kelvin
  TsedK = Tsed + 273.15d0

! Specific humidity of saturated air [kg/kg] at temperature of sediment
  PsatSed = airSatVapor(TsedK)                      ! Saturated vapor pressure
  QsatSed = mwr*PsatSed / (P -(1.d0-mwr) * PsatSed) ! at water/sediment temp

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! LATENT HEAT FLUX (due to evaporation) [W/m2]

! The latent heat of evaporation        [J/kg] KS: CHANGE WITH SHARQUAY FORMULA?
  LatentHeatEvap = (2500.84d0-2.35d0*Tsed)*1000d0

! The rate of evaporation               [kg/m2/s]
! take into account water content of upper layer sediment (por)
  EvaporationRate = dalton*RHOair*(Wind+1.d0)*(QsatSed-Qair)*por

! The latent heat gain of the water     [J/m2/s = W/m2]
  Hlatent  = -EvaporationRate*LatentHeatEvap
RETURN
END SUBROUTINE latentheat

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! SENSIBLE HEAT FLUX (due to conduction) [W/m2]
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE sensibleheat(Tair, Tsed, Wind, Qair, RHOair, stanton,                &
                        Hsensible)

 IMPLICIT NONE

 DOUBLE PRECISION, INTENT (IN) ::                                               &
      Tair ,          &    ! [dgC]     air temperature
      Tsed ,          &    ! [dgC]     temp surface sediment water
      Wind ,          &    ! [m/s]     wind speed
      Qair,           &    ! [kg/kg]   Specific humidity of the air
      stanton,        &    ! [-]       transfer coeff for sensible heat
      RhoAir               ! [kg/m3]   Air density

 DOUBLE PRECISION, INTENT(OUT) ::                                               &
      Hsensible            ! [W/m2]    SENSIBLE heat flux (due to conduction)

 DOUBLE PRECISION :: cpMoistAir

! The specific heat of moist air         [J/kg/K]  KS: CHANGE DEPENDENCY FOR CPMOISTAIR???
  cpMoistAir = 1005d0 + 1.82d0*Qair

! The sensible heat gain of the water    [J/m2/s = W/m2]
  Hsensible  = -stanton*cpMoistAir*RHOair*(Wind+1.d0)*(Tsed-Tair)

END SUBROUTINE sensibleheat

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! BACKRADIATION or Net Longwave Radiation (NLR): several formula
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE backrad (Tair, Tsed, Cloud, Vapor, emAir, emSed, NLR, Hback)

 USE Heatconstants

 IMPLICIT NONE

 DOUBLE PRECISION, INTENT (IN) ::                                               &
      Tair ,          &    ! [dgC]     air temperature
      Tsed ,          &    ! [dgC]     temp surface sediment water
      Cloud,          &    ! [-]       relative fraction cloud cover
      emAir,          &    ! [-]       emissivity of Air
      emSed,          &    ! [-]       emissivity of Water/sediment
      Vapor                ! [Pa]      vapor pressure of air
 INTEGER, INTENT (IN)  :: NLR  ! how to estimate backradiation

 DOUBLE PRECISION, INTENT(OUT) ::                                               &
      Hback              ! [W/m2]    backradiation heat flux

 DOUBLE PRECISION :: TairK, TsedK, ea

! Temperature in Kelvin
  TsedK = Tsed + 273.15d0
  TairK = Tair + 273.15d0

  ea    = Vapor/100d0  !  ea is vapor pressure in mbar  (= hPa)

  IF (NLR == 1) THEN       ! short formula

    Hback = - boltzmann* (emSed*TsedK**4. - emAir*TairK**4.)

  ELSE IF (NLR == 2) THEN  ! May 1986, in RT13

    Hback = -(boltzmann*emSed* (TairK**4.* (0.4d0-0.05d0*sqrt(ea)) +            &
           4.d0*TairK**3.d0*(TsedK-TairK)))*(1.d0-0.75*Cloud**3.4)

  ELSE IF (NLR == 3) THEN  ! Josey et al. 1997, Clark et al 1974

    Hback = -(boltzmann*emSed*                                                  &
          (TsedK**4.d0*(0.39d0-0.05d0*sqrt(ea))*(1.d0-0.8*Cloud**2) +           &
           4.d0*TsedK**3.d0*(TsedK-TairK)))

  ELSE IF (NLR == 4) THEN  ! Bunker 1976

    Hback  = -(boltzmann*emSed*                                                 &
              (TairK**4.d0*(0.254d0-0.00495d0*ea)*(1.d0-0.75d0*Cloud) +         &
                 4.d0*TairK**3.d0*(TsedK-TairK)))

  ELSE !IF (NLR == 5) THEN  ! Bignami et al 1995

    Hback = -(boltzmann*emSed*TsedK**4.d0                                       &
        - boltzmann*TairK**4.d0*(0.653d0+0.00535d0*ea)*                         &
	              	(1.d0+0.1762d0*Cloud**2.d0))
  END IF

END SUBROUTINE backrad

