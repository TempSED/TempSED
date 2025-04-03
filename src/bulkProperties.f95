!==========================================================================
! subroutine calculating the properties of the bulk sediment in 1D
! S. Goto, M. Yamano, S. Morita, T. Kanamatsu, A. Hachikubo, S. Kataoka,
! M. Tanahashi, R. Matsumoto, 2017. 
! Physical and thermal properties of mud-dominant sediment from the Joetsu 
! Basin in the eastern margin of the Japan Sea. 
! Mar Geophys Res 38:393–407, DOI 10.1007/s11001-017-9302-y
!==========================================================================
  
SUBROUTINE calcbulk(nx, densWater, densSolid, cpWater, cpSolid,                 &
&                   tcWater, tcSolid, por, porint,                              &
&                   densBulk, cpBulk, tcBulk)

 IMPLICIT NONE

 INTEGER, INTENT (IN)          ::  nx  ! number of  boxes

! chacteristics of water and solid substances

  DOUBLE PRECISION, INTENT (IN) ::                                              &
         densWater(nx), densSolid,  &  ! [kg/m3]   density           
         cpWater(nx),   cpSolid  ,  &  ! [J/kg/dg] specfic heat capacity     
         tcWater(nx+1), tcSolid        ! [W/m/dg]  thermal conductivity 

! porosity in center and interfaces of boxes

  DOUBLE PRECISION, INTENT (IN)  ::  por(nx), porint(nx+1)
  
  DOUBLE PRECISION, INTENT (OUT)::                                              &
         densBulk(nx),          &  ! [kg/m3]   density of bulk sediment          
         cpBulk(nx)  ,          &  ! [J/kg/dg] specific heat capacity of bulk    
         tcBulk(nx+1)              ! [W/m/dg]  thermal conductivity of bulk sed.

  DOUBLE PRECISION :: Beta
!..........................................................................


! sediment density - center of boxes (Goto et al, 2017, eq5)
  densbulk(:) = densWater*por(:) + densSolid*(1.d0-por(:))

! specific heat capacity profile (Goto et al, 2017, eq11)
  cpBulk(:)   = cpWater*densWater*por + cpSolid*densSolid*(1.d0-por)
  cpBulk(:)   = cpBulk/densbulk

! thermal conductivity - at box interfaces (Goto et al, 2017, eq7)
  tcBulk(:)  = tcWater**porint *  tcSolid**(1.d0-porint)

END SUBROUTINE calcbulk

!==========================================================================
! the density of the bulk sediment in 1D
!==========================================================================
  
SUBROUTINE calcbulkdensity(nx, densWater, densSolid, por,                       &
&                      densBulk)

 IMPLICIT NONE

 INTEGER, INTENT (IN)          ::  nx  ! number of  boxes

! chacteristics of water and solid substances

  DOUBLE PRECISION, INTENT (IN) ::                                              &
         densWater, densSolid      ! [kg/m3]   density           
  
  DOUBLE PRECISION, INTENT (IN) ::                                              &
         por(nx)                   ! [-] volumetric porosity 
  
  DOUBLE PRECISION, INTENT (OUT) ::                                             &
         densBulk(nx)              ! [kg/m3] density of bulk sediment          
!..........................................................................

! sediment density Goto et al, 2017, eq5
  densbulk(:) = densWater*por(:) + densSolid*(1.d0-por(:))

END SUBROUTINE calcbulkdensity

!==========================================================================
! the specific heat capacity of the bulk sediment in 1D
!==========================================================================
  
SUBROUTINE calcbulkcp(nx, densWater, densSolid, cpWater, cpSolid, por,          &
&                   cpBulk)

 IMPLICIT NONE

 INTEGER, INTENT (IN)          ::  nx  ! number of  boxes

! chacteristics of water and solid substances

  DOUBLE PRECISION, INTENT (IN) ::                                              &
         densWater, densSolid,  &  ! [kg/m3]   density           
         cpWater,   cpSolid        ! [J/kg/dg] specific heat capacity     

  DOUBLE PRECISION, INTENT (IN) ::                                              &
         por(nx)               ! [-] volumetric porosity 
  
  DOUBLE PRECISION, INTENT (OUT) ::                                             &
         cpBulk(nx)            ! [J/kg/dg] specific heat capacity of bulk          

!..........................................................................
! Goto et al., 2017 eq 11
  cpBulk(:)   = cpWater*densWater*por + cpSolid*densSolid*(1.d0-por)
  
  cpBulk(:)   = cpBulk/(densWater*por + densSolid*(1.d0-por))

END SUBROUTINE calcbulkcp

!==========================================================================
! the thermal conductivity of the bulk sediment in 1D 
!==========================================================================

SUBROUTINE calcbulktc(nx, tcWater, tcSolid, porosity,                           &
&                 tcBulk)

 IMPLICIT NONE

 INTEGER, INTENT (IN)          ::  nx  ! number of  boxes

! chacteristics of water and solid substances

  DOUBLE PRECISION, INTENT (IN) ::                                              &
         tcWater,   tcSolid        ! [W/m/dg]  thermal conductivity 

! porosity in interfaces of boxes

  DOUBLE PRECISION, INTENT (IN)  ::  porosity(nx)
  DOUBLE PRECISION, INTENT (OUT)::                                              &
         tcBulk(nx)                ! [W/m/dg]  thermal conductivity of bulk 

!..........................................................................

! thermal conductivity - defined at box interfaces (Goto et al, 2017, eq7)
  tcBulk(:)  = tcWater**porosity *  tcSolid**(1.d0-porosity)

END SUBROUTINE calcbulktc

!==========================================================================
! the thermal diffusivity of the bulk sediment in 1D
!==========================================================================

SUBROUTINE calcbulktd(nx, densWater, densSolid, cpWater, cpSolid,               &
&                 tdWater, tdSolid, porosity,                                   &
&                 tdBulk)

 IMPLICIT NONE

 INTEGER, INTENT (IN)          ::  nx  ! number of  boxes

! chacteristics of water and solid substances

  DOUBLE PRECISION, INTENT (IN) ::                                              &
         densWater, densSolid,  &  ! [kg/m3]   density           
         cpWater,   cpSolid  ,  &  ! [J/kg/dg] specific heat capacity     
         tdWater,   tdSolid        ! [m2/s]    thermal diffusion 

! porosity in interfaces of boxes

  DOUBLE PRECISION, INTENT (IN)  ::  porosity(nx)
  DOUBLE PRECISION, INTENT (OUT)::                                              &
         tdBulk(nx)                ! [m2/s]    thermal diffusion of bulk sediment

  DOUBLE PRECISION :: Beta
!..........................................................................


! thermal diffusivity - defined at box interfaces (Goto et al, 2017, eq14)
  tdBulk(:)  = tdWater**porosity *  tdSolid**(1.d0-porosity)
  
  beta       = cpWater*densWater/(cpSolid*densSolid)
  tdBulk     = beta**porosity/(1.d0+(beta-1.d0)*porosity)*tdBulk

END SUBROUTINE calcbulktd
