
!**************************************************************************
!**************************************************************************
!********************   Heat constants     *******************
!**************************************************************************
!**************************************************************************

MODULE Heatconstants

 DOUBLE PRECISION, PARAMETER ::                                                 &
   boltzmann = 5.67d-08,  & ! [W/m2/K^4] Stefan-Boltzmann ct
   Rd        = 287.05,    & ! [J/kg/K]   specific gas ct for dry air
   Rv        = 461.495,   & ! [J/kg/K]   specific gas ct for water vapour
   mwr       = 0.622d0      ! [-]        mol weight water/mol weight dry air

END MODULE Heatconstants

!**************************************************************************
! Air density, humidity, vapor pressure
!**************************************************************************

SUBROUTINE airproperties(Tair, P, Qrel, Qair, RHOair, Vapor)

! -------------------------------------------------------------------------
! REF??
! -------------------------------------------------------------------------

 USE Heatconstants
 IMPLICIT NONE

 DOUBLE PRECISION, INTENT (IN) ::                                               &
      Tair ,          &    ! [dgC]     air temperature
      P    ,          &    ! [Pa]      air pressure,
      Qrel                 ! [-]       relative air humidity (0-1)

 DOUBLE PRECISION, INTENT(OUT) ::                                               &
      Qair,           &   ! [kg/kg]   specific humidity of the air
      RhoAir,         &   ! [kg/m3]   air density
      Vapor               ! [Pa]      vapor pressure of air

 DOUBLE PRECISION :: PsatAir, QsatAir
 DOUBLE PRECISION :: Pdry, TairK
 DOUBLE PRECISION :: LatentHeatEvap
 DOUBLE PRECISION :: airSatVapor

! Temperature in Kelvin
  TairK = Tair + 273.15d0

! Specific humidity of saturated air [kg/kg] at air temperature
  PsatAir = airSatVapor(TairK)                      ! Pa
  QsatAir = mwr*PsatAir / (P -(1.d0-mwr) * PsatAir) ! at air temperature

  Qair    = QsatAir*Qrel   ! [kg/kg]  Specific humidity of the air
  Vapor   = PsatAir*Qrel   ! [Pa]     vapor pressure of air
  Pdry    = P-Vapor        ! [Pa]     pressure of dry air

  RhoAir = (Pdry/Rd + Vapor/Rv)/TairK  ! [kg/m3]

RETURN

END SUBROUTINE airproperties

!**************************************************************************
! The saturated vapor pressure [Pa] of pure water
!**************************************************************************

DOUBLE PRECISION FUNCTION airSatVapor(TK)
! ------------------------------------------------------------------------------
! Goff and Gratch 1946
! ------------------------------------------------------------------------------

 IMPLICIT NONE
 
 DOUBLE PRECISION, INTENT(IN) :: TK  ! temperature, [dg K]
 DOUBLE PRECISION :: a1, a2, a3, a4, a5, a6

!..........................................................................
  a1 = 373.16d0/TK
  a2 = a1-1.d0
  a3 = 1.d0 - 1.d0/a1
  a4 = 10.d0**(-a2*3.49149)-1.d0
  a5 = 10.d0**( a3*11.344) -1.d0
  a6 = -a2*7.90298d0 +a4*8.1328d-3 -a5*1.3816d-7
  
  airSatVapor = 101324.6*a1**(5.02808d0)*10**a6  ! pressure, [Pa]

RETURN

END FUNCTION airSatVapor
