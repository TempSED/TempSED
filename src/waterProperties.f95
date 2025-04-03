!**************************************************************************
!**************************************************************************
!********************      Properties of water       *********************
!**************************************************************************
!**************************************************************************

!**************************************************************************
! Specific heat capacity of water
!**************************************************************************

SUBROUTINE calccpwater(nx, TC, S, cp)

! ------------------------------------------------------------------------------
! D.T. Jamieson, J.S. Tudhope, R. Morris and G. Cartwright,
! Physical properties of sea water solutions: heat capacity,
! Desalination, 7(1) (1969) 23–30.
! ------------------------------------------------------------------------------

 IMPLICIT NONE

 INTEGER, INTENT (IN) :: nx

 DOUBLE PRECISION, INTENT (IN) ::                                               &
      TC(nx),      &    ! [dgC]     air temperature
!     P,           &    ! [Pa]      air pressure,
      S                 ! [-]       salinity

 DOUBLE PRECISION, INTENT(OUT) ::                                               &
      cp(nx)            ! [J/kg/dg]    specific heat capacity of water

 DOUBLE PRECISION :: T68(nx), A, B, C, D, S2

! temperature as in the 1968 scale
  t68 = 1.00025d0*(TC+273.15 - 0.0682875d0)
  S2  = S*S

  A = 5.328d0   -9.76d-2   *S + 4.04d-4  *S2
  B = -6.913d-3 + 7.351d-4 *S - 3.15d-6  *S2
  C = 9.6d-6    - 1.927d-6 *S + 8.23d-9  *S2
  D = 2.5d-9    + 1.666d-9 *S - 7.125d-12*S2
  
  cp = A + B*t68 + C*t68**2 + D*t68**3  
  cp = cp*1000.d0
  
RETURN
END SUBROUTINE calccpwater

!**************************************************************************
! Density of water at one atm
!**************************************************************************

SUBROUTINE calcrhowater(nx, TC, S, rho)

! ------------------------------------------------------------------------------
! F.J. Millero and A. Poisson, International one-atmosphere
! equation of state of seawater, Deep-Sea Research, 28A (6)
! (1981) 625–629.
! ------------------------------------------------------------------------------

 IMPLICIT NONE

 INTEGER, INTENT (IN) :: nx

 DOUBLE PRECISION, INTENT (IN) ::                                               &
      TC(nx),      &    ! [dgC]     air temperature
!     P,           &    ! [Pa]      air pressure,
      S                 ! [-]       salinity

 DOUBLE PRECISION, INTENT(OUT) ::                                               &
      rho(nx)           ! [kg/m3]   density of water

 DOUBLE PRECISION, dimension (nx) :: T68, T_2, T_3, T_4, T_5, A, B, rhow
 DOUBLE PRECISION :: C, S2, S32

! temperature as in the 1968 scale, in dgK
  T68 = 1.00025d0*(TC - 2e-4) !0.0682875d0
  T_2 = T68 * T68
  T_3 = T_2 * T68
  T_4 = T_3 * T68
  T_5 = T_4 * T68
  
  S2  = S * S
  S32 = S*dsqrt(S)
  
  rhow = 999.842594d0 + 6.793952d-02 * T68 - 9.09529d-03 * T_2 +                &
         1.001685d-4 * T_3 - 1.120083d-06 * T_4 + 6.536332d-09 * T_5
  A    = 0.824493d0 - 0.0040899d0 * T68 + 7.6438d-05 * T_2 -                    &
         8.2467d-07 *T_3 + 5.3875d-09 * T_4
  B    = -0.00572466d0 + 0.00010227d0 * T68 - 1.6546d-06 * T_2
  C    = 0.00048314d0
  rho  = rhow + A *S + B * S32 + C * S2

END SUBROUTINE calcrhowater

!**************************************************************************
! thermal diffusivity of water
!**************************************************************************

SUBROUTINE calctdwater(nx, type, TC, S, P, cp, rho, td)

! ------------------------------------------------------------------------------
! Jamieson and Tudhope
! ------------------------------------------------------------------------------

 IMPLICIT NONE

 INTEGER, INTENT (IN) :: nx

 DOUBLE PRECISION, INTENT (IN) ::                                               &
      TC(nx),      &    ! [dgC]       air temperature
      P,           &    ! [Pa]        air pressure, in mPa?????
      S,           &    ! [-]         salinity
      cp(nx),      &    ! [J/kg/dg]   specific heat capacity of water
      rho(nx)           ! [kg/m3]     density of water

 INTEGER, INTENT (IN) :: type
 
 DOUBLE PRECISION, INTENT(OUT) ::                                               &
      td(nx)            ! [m2/s]      thermal diffusivity

 DOUBLE PRECISION :: cond(nx)  
 
   CALL calctcwater(nx, type, TC, S, P, cond)
   td = cond/(rho*cp)

END SUBROUTINE calctdwater

!**************************************************************************
! Thermal conductivity 
!**************************************************************************

SUBROUTINE calctcwater(nx, type, TC, S, P, cond)

! ------------------------------------------------------------------------------
! Jamieson and Tudhope
! ------------------------------------------------------------------------------

 IMPLICIT NONE

 INTEGER, INTENT (IN) :: nx
 INTEGER, INTENT (IN) :: type
 
 DOUBLE PRECISION, INTENT (IN) ::                                               &
      TC(nx),      &    ! [dgC]     air temperature
      P,           &    ! [Pa]      air pressure,
      S                 ! [-]       salinity

 DOUBLE PRECISION, INTENT(OUT) ::                                               &
      cond(nx)          ! [W/m/dg]   conductivity of water

 DOUBLE PRECISION :: T68(nx), TK68(nx), A(nx), F(nx)

! temperature as in the 1968 scale
  T68   = 1.00025d0*(TC - 2e-4) !0.0682875d0)

  IF (type .EQ. 1) THEN  ! Jamieson
    TK68 = T68+273.15
    A    = (2.3 - (343.5+0.037*S)/TK68 ) * (1.d0-TK68/(647+0.03*S))**0.333
    F    = log10(240.d0 +0.0002d0*S) + 0.434*A
    cond = 10.**F  ! mW/(m.K)
    
  ELSE IF (type .EQ. 2) THEN ! Caldwell - includes the typo correction (6.53e-4)
    cond = 0.5715d0*(1.d0 + 0.003*T68- 1.025d-5*T68**2 + 6.53d-4*P - 0.00029*S)  
  
  ELSE IF (type .EQ. 3) THEN ! Castelli
    cond = 0.55286d0 + 3.4025d-4*P + 1.8364d-3*T68 - 3.3058d-7*T68**2  
  END IF
  
  cond = cond*1d-3
  
END SUBROUTINE calctcwater

!**************************************************************************
! Latent heat of vaporisation
!**************************************************************************

SUBROUTINE calclhwater(nx, TC, S, lh)

! ------------------------------------------------------------------------------
! Sharkawy, M.H. Lienhard,J.H., Zubair, S.M., 2010. 
! Thermophysical properties of seawater: a review of existing correlations and data. 
! Desalination and Water Treatment 16:1-3, 354-380.
! ------------------------------------------------------------------------------

 IMPLICIT NONE

 INTEGER, INTENT (IN) :: nx

 DOUBLE PRECISION, INTENT (IN) ::                                               &
      TC(nx),      &    ! [dgC]     air temperature
!     P,           &    ! [Pa]      air pressure,
      S                 ! [-]       salinity

 DOUBLE PRECISION, INTENT(OUT) ::                                               &
      lh(nx)            ! [J/kg]    latent heat of seawater

 DOUBLE PRECISION :: T_2(nx), T_3(nx)

  T_2 = TC*TC
  T_3 = T_2*TC
  
  ! Latent heat for pure water
  LH = 2.501d6 -2.369d3*TC + 2.678d-1*T_2 -8.103d-3*T_3 -2.079d-5*T_3*TC
  
  IF (S .GT. 0.d0) THEN   ! add salinity factor
    LH = LH*(1.d0-S/1000.d0)
  END IF
  
END SUBROUTINE calclhwater

!**************************************************************************
! The seawater vapor pressure [Pa] 
!**************************************************************************

SUBROUTINE calcVaporwater(TK, S, Vapor)

! ------------------------------------------------------------------------------
! Nayar, Kishor G., Mostafa H. Sharqawy, Leonardo D. Banchik, John H. Lienhard V.
! Thermophysical Properties of Seawater: 
! A Review and New Correlations That Include Pressure Dependence.  
! Desalination 390 (July 2016): 1-24.
! ------------------------------------------------------------------------------

 IMPLICIT NONE
 
 DOUBLE PRECISION, INTENT(IN) :: TK  ! temperature, [dg K]
 DOUBLE PRECISION, INTENT(IN) :: S   ! salinity [-]

 DOUBLE PRECISION, INTENT(OUT) ::                                               &
      Vapor                 ! [Pa]      vapor pressure of water
      
 DOUBLE PRECISION :: pure, Ratio

!..........................................................................

! vapor pressure of pure water
  pure = -5800d0/TK + 1.3915d0 -4.864d-2*TK +4.1765d-5*TK*TK                    &
       - 1.4452d-8*TK*TK*TK + 6.546d0*log(TK)
  pure = dexp(pure)
  
  IF (S .GT. 0.d0) THEN
    Ratio = dexp(-4.5818d-4*S - 2.0443e-6*S*S)
    Vapor = pure*Ratio
  ELSE
    Vapor = pure
  END IF
  
END SUBROUTINE calcVaporwater
