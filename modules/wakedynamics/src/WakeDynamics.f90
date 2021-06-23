!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2015-2016  National Renewable Energy Laboratory
!
!    This file is part of WakeDynamics.
!
! Licensed under the Apache License, Version 2.0 (the "License");
! you may not use this file except in compliance with the License.
! You may obtain a copy of the License at
!
!     http://www.apache.org/licenses/LICENSE-2.0
!
! Unless required by applicable law or agreed to in writing, software
! distributed under the License is distributed on an "AS IS" BASIS,
! WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
! See the License for the specific language governing permissions and
! limitations under the License.
!
!**********************************************************************************************************************************
! File last committed: $Date$
! (File) Revision #: $Rev$
! URL: $HeadURL$
!**********************************************************************************************************************************
!> WakeDynamics is a time-domain module for modeling wake dynamics of one or more horizontal-axis wind turbines.
module WakeDynamics
    
   use NWTC_Library
   use WakeDynamics_Types
     
   implicit none

   private
   
   type(ProgDesc), parameter  :: WD_Ver = ProgDesc( 'WakeDynamics', '', '' )
   character(*),   parameter  :: WD_Nickname = 'WD'      

   ! ..... Public Subroutines ...................................................................................................

   public :: WD_Init                           ! Initialization routine
   public :: WD_End                            ! Ending routine (includes clean up)
   public :: WD_UpdateStates                   ! Loose coupling routine for solving for constraint states, integrating
                                               !   continuous states, and updating discrete states
   public :: WD_CalcOutput                     ! Routine for computing outputs
   public :: WD_CalcConstrStateResidual        ! Tight coupling routine for returning the constraint state residual

   public :: WD_TEST_Axi2Cart
   public :: WD_TEST_VelocityCurl
   contains  

function  WD_Interp ( yVal, xArr, yArr )
   real(ReKi)                     :: WD_Interp
   real(ReKi),      intent(in   ) :: yVal
   real(ReKi),      intent(in   ) :: xArr(:) 
   real(ReKi),      intent(in   ) :: yArr(:)
   
   integer(IntKi)  :: i, nPts
   real(ReKi)      :: y1,y2,x1,x2,dy
   
   
   nPts = size(xArr)
   WD_Interp = 0.0_ReKi
   y2 = yArr(nPts) - yVal
   x2 = xArr(nPts)
   do i=nPts-1,1,-1
      y1 = yArr(i) - yVal 
      x1 = xArr(i)
      if( nint( sign(1.0_ReKi, y1) ) /= nint( sign(1.0_ReKi, y2) ) ) then
                 
         dy = y2-y1
         if (EqualRealNos(dy,0.0_ReKi) ) then
            WD_Interp =  x2
         else
            WD_Interp = (x2-x1)*(yVal-y1)/(dy) + x1  
         end if
         exit
         
      end if
      
      y2 = y1
      x2 = x1
   end do
   
end function WD_Interp
!----------------------------------------------------------------------------------------------------------------------------------   
!> This function sets the nacelle-yaw-related directional term for the yaw correction deflection calculations
!!   
function GetYawCorrection(yawErr, xhat_disk, dx, p, errStat, errMsg)
   real(ReKi), dimension(3) :: GetYawCorrection
   real(ReKi),                    intent(in   )  :: yawErr           !< Nacelle-yaw error at the wake planes
   real(ReKi),                    intent(in   )  :: xhat_disk(3)     !< Orientation of rotor centerline, normal to disk
   real(ReKi),                    intent(in   )  :: dx               !< Dot_product(xhat_plane,V_plane)*DT_low
   type(WD_ParameterType),        intent(in   )  :: p                !< Parameters
   integer(IntKi),                intent(  out)  :: errStat          !< Error status of the operation
   character(*),                  intent(  out)  :: errMsg           !< Error message if errStat /= ErrID_None
   
   real(ReKi) :: xydisk(3),yxdisk(3),yydisk(3),xxdisk(3),xydisknorm
   
   errStat = ErrID_None
   errMsg  = ''
   
   xydisk = (/0.0_ReKi, xhat_disk(1), 0.0_ReKi/)
   yxdisk = (/xhat_disk(2), 0.0_ReKi, 0.0_ReKi/)
   yydisk = (/0.0_ReKi, xhat_disk(2), 0.0_ReKi/)
   xxdisk = (/xhat_disk(1), 0.0_ReKi, 0.0_ReKi/)
   xydisknorm = TwoNorm(xxdisk + yydisk)
   
   if (EqualRealNos(xydisknorm,0.0_ReKi)) then
      ! TEST: E3
      call SetErrStat( ErrID_Fatal, 'Orientation of the rotor centerline at the rotor plane is directed vertically upward or downward, whereby the nacelle-yaw error and horizontal wake-deflection correction is undefined.', errStat, errMsg, 'GetYawCorrectionTermA' )
      return
   end if
 
   if (EqualRealNos(dx,0.0_ReKi)) then
      GetYawCorrection = ( p%C_HWkDfl_O + p%C_HWkDfl_OY*YawErr ) *      ( ( xydisk - yxdisk ) / (xydisknorm) )
   else
      GetYawCorrection = ( p%C_HWkDfl_x + p%C_HWkDfl_xY*yawErr ) * dx * ( ( xydisk - yxdisk ) / (xydisknorm) )
   end if
      
end function GetYawCorrection

!----------------------------------------------------------------------------------------------------------------------------------   
!> This subroutine calculates the eddy viscosity filter functions, prepresenting the delay in the turbulent stress generated by 
!! ambient turbulence or the development of turbulent stresses generated by the shear layer
real(ReKi) function EddyFilter(x_plane, D_rotor, C_Dmin, C_Dmax, C_Fmin, C_Exp)

   real(ReKi),          intent(in   ) :: x_plane         !< Downwind distance from rotor to each wake plane (m)
   real(ReKi),          intent(in   ) :: D_rotor         !< Rotor diameter (m)
   real(ReKi),          intent(in   ) :: C_Dmin          !< Calibrated parameter defining the transitional diameter fraction between the minimum and exponential regions
   real(ReKi),          intent(in   ) :: C_Dmax          !< Calibrated parameter defining the transitional diameter fraction between the exponential and maximum regions
   real(ReKi),          intent(in   ) :: C_Fmin          !< Calibrated parameter defining the functional value in the minimum region
   real(ReKi),          intent(in   ) :: C_Exp           !< Calibrated parameter defining the exponent in the exponential region


      ! Any errors due to invalid choices of the calibrated parameters have been raised when this module was initialized
   
   if ( x_plane <= C_Dmin*D_rotor ) then
      EddyFilter = C_Fmin
   else if (x_plane >= C_Dmax*D_rotor) then
      EddyFilter = 1_ReKi
   else
      EddyFilter = C_Fmin + (1_ReKi-C_Fmin)*( ( (x_plane/D_rotor) - C_DMin ) / (C_Dmax-C_Dmin) )**C_Exp
   end if


end function EddyFilter

!----------------------------------------------------------------------------------------------------------------------------------   
!> This subroutine calculates the wake diameter at a wake plane, based on one of four models
real(ReKi) function WakeDiam( Mod_WakeDiam, nr, dr, rArr, Vx_wake, Vx_wind_disk, D_rotor, C_WakeDiam)

   integer(intKi),      intent(in   ) :: Mod_WakeDiam       !< Wake diameter calculation model [ 1=Rotor diameter, 2=Velocity, 3=Mass flux, 4=Momentum flux]
   integer(intKi),      intent(in   ) :: nr                 !< Number of radii in the radial finite-difference grid
   real(ReKi),          intent(in   ) :: dr                 !< Radial increment of radial finite-difference grid (m)
   real(ReKi),          intent(in   ) :: rArr(0:)           !< Discretization of radial finite-difference grid (m)
   real(ReKi),          intent(in   ) :: Vx_wake(0:)        !< Axial wake velocity deficit at a wake plane, distributed radially (m/s)
   real(ReKi),          intent(in   ) :: Vx_wind_disk       !< Rotor-disk-averaged ambient wind speed, normal to planes (m/s)
   real(ReKi),          intent(in   ) :: D_rotor            !< Rotor diameter (m)
   real(ReKi),          intent(in   ) :: C_WakeDiam         !< Calibrated parameter for wake diameter calculation

   integer(IntKi) :: ILo
   real(ReKi) :: m(0:nr-1)
   integer(IntKi) :: i
   ILo = 0
    
   ! Any errors due to invalid values of dr and C_WakeDiam have been raised when this module was initialized
   
   select case ( Mod_WakeDiam )
      case (WakeDiamMod_RotDiam) 
      
         WakeDiam = D_rotor
         
      case (WakeDiamMod_Velocity)  
         
            ! Ensure the wake diameter is at least as large as the rotor diameter   
         
         WakeDiam = max(D_rotor, 2.0_ReKi*WD_Interp( (C_WakeDiam-1_ReKi)*Vx_wind_disk, rArr, Vx_wake ) )
         
      case (WakeDiamMod_MassFlux) 
         
         m(0) = 0.0
         do i = 1,nr-1
            m(i) = m(i-1) + pi*dr*(Vx_wake(i)*rArr(i) + Vx_wake(i-1)*rArr(i-1))
         end do
         
         WakeDiam = max(D_rotor, 2.0_ReKi*WD_Interp( C_WakeDiam*m(nr-1), rArr, m ) )
         
      case (WakeDiamMod_MtmFlux)
         
         m(0) = 0.0
         do i = 1,nr-1
            m(i) = m(i-1) + pi*dr*( (Vx_wake(i)**2)*rArr(i) + (Vx_wake(i-1)**2)*rArr(i-1))
         end do
         
         WakeDiam = max(D_rotor, 2.0_ReKi*WD_Interp( C_WakeDiam*m(nr-1), rArr, m ) )
   
   end select

      
end function WakeDiam


!----------------------------------------------------------------------------------------------------------------------------------   
!> This subroutine computes the near wake correction : Vx_wake  
subroutine NearWakeCorrection( Ct_azavg_filt, Vx_rel_disk_filt, p, m, Vx_wake, D_rotor, errStat, errMsg )
   real(ReKi),                   intent(in   ) :: Ct_azavg_filt(0:) !< Time-filtered azimuthally averaged thrust force coefficient (normal to disk), distributed radially
   real(ReKi),                   intent(in   ) :: D_rotor           !< Rotor diameter
   real(ReKi),                   intent(in   ) :: Vx_rel_disk_filt  !< Time-filtered rotor-disk-averaged relative wind speed (ambient + deficits + motion), normal to disk
   type(WD_ParameterType),       intent(in   ) :: p                 !< Parameters
   type(WD_MiscVarType),         intent(inout) :: m                 !< Initial misc/optimization variables
   real(ReKi),                   intent(inout) :: Vx_wake(0:)       !< Axial wake velocity deficit at first plane
   integer(IntKi),               intent(  out) :: errStat           !< Error status of the operation
   character(*),                 intent(  out) :: errMsg            !< Error message if errStat /= ErrID_None
   real(ReKi) :: alpha
   real(ReKi) :: Ct_avg            ! Rotor-disk averaged Ct
   integer(IntKi) :: j, errStat2
   character(*), parameter  :: RoutineName = 'NearWakeCorrection'
   real(ReKi), parameter    :: Ct_low = 0.96_ReKi, Ct_high = 1.10_ReKi ! Limits for blending
   
   errStat = ErrID_None
   errMsg  = ''

   ! Computing average Ct = \int r Ct dr / \int r dr = 2/R^2 \int r Ct dr using trapz
   ! NOTE: r goes beyond the rotor (works since Ct=0 beyond that)
   Ct_avg = 0.0_ReKi
   do j=1,p%NumRadii-1
      Ct_avg = Ct_avg + 0.5_ReKi * (p%r(j) * Ct_azavg_filt(j) + p%r(j-1) * Ct_azavg_filt(j-1)) * p%dr
   enddo
   Ct_avg = 8.0_ReKi*Ct_avg/(D_rotor*D_rotor)

   if (Ct_avg > 2.0_ReKi ) then
      ! THROW ERROR because we are in the prop-brake region
      ! TEST: E5
      call SetErrStat(ErrID_FATAL, 'Wake model is not valid in the propeller-brake region, i.e., Ct > 2.0.', errStat, errMsg, RoutineName)
      return

   else if ( Ct_avg < Ct_low ) then
      ! Low Ct region
      call Vx_low_Ct(Vx_wake, p%r) ! Compute Vx_wake at p%r

   else if ( Ct_avg > Ct_high ) then
      ! high Ct region
      call Vx_high_Ct(Vx_wake, p%r, Ct_avg) ! Compute Vx_wake at p%r
      ! m%r_wake = p%r ! No distinction between r_wake and r, r_wake is just a temp variable anyway

   else
      ! Blending Ct region between Ct_low and Ct_high
      call Vx_low_Ct (Vx_wake, p%r)         ! Evaluate Vx_wake (Ct_low)  at p%r
      call Vx_high_Ct(m%Vx_high, p%r, Ct_avg) ! Evaluate Vx_high (Ct_high) at p%r

      alpha = 1.0_ReKi - (Ct_avg - Ct_low) / (Ct_high-Ct_low) ! linear blending coefficient
      do j=0,p%NumRadii-1
         Vx_wake(j) = alpha*Vx_wake(j)+(1.0_ReKi-alpha)*m%Vx_high(j)  ! Blended CT velocity
      end do
   end if   

contains

   !> Compute the induced velocity distribution in the wake for low thrust region 
   subroutine Vx_low_Ct(Vx, r_eval)
      real(ReKi), dimension(0:), intent(out) :: Vx     !< Wake induced velocity (<0)
      real(ReKi), dimension(0:), intent(in ) :: r_eval !< Radial position where velocity is to be evaluated
      integer(IntKi) :: ILo ! index for interpolation
      real(ReKi) :: a_interp

      ! compute r_wake and m%a using Ct_azavg_filt
      m%r_wake(0) = 0.0_ReKi
      do j=0,p%NumRadii-1
         ! NOTE: Ct clipped instead of (2.0_ReKi + 3.0_ReKi*sqrt(14.0_ReKi*Ct_azavg_filt(j)-12.0_ReKi))/14.0_ReKi
         m%a(j) =  0.5_ReKi - 0.5_ReKi*sqrt( 1.0_ReKi-min(Ct_azavg_filt(j),24.0_ReKi/25.0_ReKi))
         if (j > 0) then
            m%r_wake(j)  = sqrt(m%r_wake(j-1)**2.0_ReKi + p%dr*( ((1.0_ReKi - m%a(j))*p%r(j)) / (1.0_ReKi-p%C_NearWake*m%a(j)) + ((1.0_ReKi - m%a(j-1))*p%r(j-1)) / (1.0_ReKi-p%C_NearWake*m%a(j-1)) ) )
         end if
      end do
      ! Use a and rw to determine Vx
      Vx(0) = -Vx_rel_disk_filt*p%C_Nearwake*m%a(0)
      ILo = 0
      do j=1,p%NumRadii-1
         ! given r_wake and m%a at p%dr increments, find value of m%a(r_wake) using interpolation 
         a_interp = InterpBin( r_eval(j), m%r_wake, m%a, ILo, p%NumRadii ) !( XVal, XAry, YAry, ILo, AryLen )
         Vx(j) = -Vx_rel_disk_filt*p%C_NearWake*a_interp !! Low CT velocity
      end do
   end subroutine Vx_low_Ct

   !> Compute the induced velocity distribution in the wake for high thrust region 
   subroutine Vx_high_Ct(Vx, r_eval, Ct_avg)
      real(ReKi), dimension(0:), intent(out) :: Vx     !< Wake induced velocity (<0)
      real(ReKi), dimension(0:), intent(in ) :: r_eval !< Wake radial coordinate 
      real(ReKi),                intent(in ) :: Ct_avg !< Rotor-disk averaged Ct
      real(ReKi) :: mu, sigma ! Gaussian shape parameters for high thrust region
      real(ReKi), parameter :: x_bar=4._ReKi ! dimensionless downstream distance used to tune the model
      mu    = (3._ReKi/(2._ReKi*Ct_avg*Ct_avg-1._ReKi)  + 4._ReKi -0.5_ReKi*x_bar) /10._ReKi
      sigma = D_rotor*  (0.5_ReKi*Ct_avg + x_bar/(25._ReKi))
      do j=0,p%NumRadii-1
         Vx(j) = -Vx_rel_disk_filt*mu*exp(-r_eval(j)*r_eval(j)/(sigma*sigma)) !! High CT Velocity
      end do
   end subroutine Vx_high_Ct
   
end subroutine NearWakeCorrection



!----------------------------------------------------------------------------------------------------------------------------------   
!> This subroutine solves the tridiagonal linear system for x() using the Thomas algorithm   
subroutine ThomasAlgorithm(nr, a, b, c, d, x, errStat, errMsg)

   integer(IntKi),      intent(in   ) :: nr                 !< Number of radii in the radial finite-difference grid
   real(ReKi),          intent(inout) :: a(0:)              !< Sub diagonal
   real(ReKi),          intent(inout) :: b(0:)              !< Main diagonal
   real(ReKi),          intent(inout) :: c(0:)              !< Super diagonal
   real(ReKi),          intent(inout) :: d(0:)              !< Right-hand side
   real(ReKi),          intent(inout) :: x(0:)              !< Solution of the linear solve
   integer(IntKi),      intent(  out) :: errStat            !< Error status of the operation
   character(*),        intent(  out) :: errMsg             !< Error message if errStat /= ErrID_None
   real(ReKi)     :: m
   integer(IntKi) :: i
   character(*), parameter            :: RoutineName = 'ThomasAlgorithm'
   
   errStat = ErrID_None
   errMsg  = ''
   
   ! Assumes all arrays are the same length
   
      ! Check that tridiagonal matrix is not diagonally dominant
   if ( abs(b(0)) <= abs(c(0)) ) then
      ! TEST: E16
       call SetErrStat( ErrID_Fatal, 'Tridiagonal matrix is not diagonally dominant, i.e., abs(b(0)) <= abs(c(0)). Try reducing the FAST.Farm timestep.', errStat, errMsg, RoutineName )
       return
   end if
   do i = 1,nr-2
      if ( abs(b(i)) <= ( abs(a(i))+abs(c(i)) ) ) then
         ! TEST: E17
          call SetErrStat( ErrID_Fatal, 'Tridiagonal matrix is not diagonally dominant, i.e., abs(b(i)) <= ( abs(a(i))+abs(c(i)) ). Try reducing the FAST.Farm timestep.', errStat, errMsg, RoutineName )
          return
      end if
   end do
   if ( abs(b(nr-1)) <= abs(a(nr-1)) ) then
      ! TEST: E18
       call SetErrStat( ErrID_Fatal, 'Tridiagonal matrix is not diagonally dominant, i.e., abs(b(nr-1)) <= abs(a(nr-1)). Try reducing the FAST.Farm timestep.', errStat, errMsg, RoutineName )
       return
   end if
   
   do i = 1,nr-1 
      m = -a(i)/b(i-1)
      b(i) = b(i) +  m*c(i-1)
      d(i) = d(i) +  m*d(i-1)
   end do
   
   x(nr-1) = d(nr-1)/b(nr-1)
   do i = nr-2,0, -1
      x(i) = ( d(i) - c(i)*x(i+1) ) / b(i)
   end do

end subroutine ThomasAlgorithm

!----------------------------------------------------------------------------------------------------------------------------------   
!> This routine is called at the start of the simulation to perform initialization steps.
!! The parameters are set here and not changed during the simulation.
!! The initial states and initial guess for the input are defined.
subroutine WD_Init( InitInp, u, p, x, xd, z, OtherState, y, m, Interval, InitOut, errStat, errMsg )
!..................................................................................................................................

   type(WD_InitInputType),       intent(in   ) :: InitInp       !< Input data for initialization routine
   type(WD_InputType),           intent(  out) :: u             !< An initial guess for the input; input mesh must be defined
   type(WD_ParameterType),       intent(  out) :: p             !< Parameters
   type(WD_ContinuousStateType), intent(  out) :: x             !< Initial continuous states
   type(WD_DiscreteStateType),   intent(  out) :: xd            !< Initial discrete states
   type(WD_ConstraintStateType), intent(  out) :: z             !< Initial guess of the constraint states
   type(WD_OtherStateType),      intent(  out) :: OtherState    !< Initial other states
   type(WD_OutputType),          intent(  out) :: y             !< Initial system outputs (outputs are not calculated;
                                                                !!   only the output mesh is initialized)
   type(WD_MiscVarType),         intent(  out) :: m             !< Initial misc/optimization variables
   real(DbKi),                   intent(in   ) :: interval      !< Coupling interval in seconds: the rate that
                                                                !!   (1) WD_UpdateStates() is called in loose coupling &
                                                                !!   (2) WD_UpdateDiscState() is called in tight coupling.
                                                                !!   Input is the suggested time from the glue code;
                                                                !!   Output is the actual coupling interval that will be used
                                                                !!   by the glue code.
   type(WD_InitOutputType),      intent(  out) :: InitOut       !< Output for initialization routine
   integer(IntKi),               intent(  out) :: errStat       !< Error status of the operation
   character(*),                 intent(  out) :: errMsg        !< Error message if errStat /= ErrID_None
   

      ! Local variables
   integer(IntKi)                              :: i             ! loop counter
   
   integer(IntKi)                              :: errStat2      ! temporary error status of the operation
   character(ErrMsgLen)                        :: errMsg2       ! temporary error message 
   character(*), parameter                     :: RoutineName = 'WD_Init'
   
   
      ! Initialize variables for this routine

   call WD_TEST_VelocityCurl() ! Temporary hack

   errStat = ErrID_None
   errMsg  = ""
  
      ! Initialize the NWTC Subroutine Library

   call NWTC_Init( EchoLibVer=.FALSE. )

   
      ! Display the module information

   if (InitInp%TurbNum <= 1) call DispNVD( WD_Ver )       
      
      ! Validate the initialization inputs
   call ValidateInitInputData( interval, InitInp, InitInp%InputFileData, ErrStat2, ErrMsg2 )
      call SetErrStat( ErrStat2, ErrMsg2, errStat, errMsg, RoutineName ) 
      if (errStat >= AbortErrLev) return
      
      !............................................................................................
      ! Define parameters
      !............................................................................................
      
   
      
      ! set the rest of the parameters
   p%DT_low        = interval         
   p%Mod_Wake      = InitInp%InputFileData%Mod_Wake 
   p%NumPlanes     = InitInp%InputFileData%NumPlanes   
   p%NumRadii      = InitInp%InputFileData%NumRadii    
   p%dr            = InitInp%InputFileData%dr  
   p%k_VortexDecay = InitInp%InputFileData%k_VortexDecay 
   p%C_HWkDfl_O    = InitInp%InputFileData%C_HWkDfl_O 
   p%C_HWkDfl_OY   = InitInp%InputFileData%C_HWkDfl_OY
   p%C_HWkDfl_x    = InitInp%InputFileData%C_HWkDfl_x 
   p%C_HWkDfl_xY   = InitInp%InputFileData%C_HWkDfl_xY
   p%C_NearWake    = InitInp%InputFileData%C_NearWake  
   p%C_vAmb_DMin   = InitInp%InputFileData%C_vAmb_DMin 
   p%C_vAmb_DMax   = InitInp%InputFileData%C_vAmb_DMax 
   p%C_vAmb_FMin   = InitInp%InputFileData%C_vAmb_FMin 
   p%C_vAmb_Exp    = InitInp%InputFileData%C_vAmb_Exp  
   p%C_vShr_DMin   = InitInp%InputFileData%C_vShr_DMin 
   p%C_vShr_DMax   = InitInp%InputFileData%C_vShr_DMax 
   p%C_vShr_FMin   = InitInp%InputFileData%C_vShr_FMin 
   p%C_vShr_Exp    = InitInp%InputFileData%C_vShr_Exp  
   p%k_vAmb        = InitInp%InputFileData%k_vAmb      
   p%k_vShr        = InitInp%InputFileData%k_vShr      
   p%Mod_WakeDiam  = InitInp%InputFileData%Mod_WakeDiam
   p%C_WakeDiam    = InitInp%InputFileData%C_WakeDiam  
   
   ! Finite difference grid coordinates r, y, z
   allocate( p%r(0:p%NumRadii-1),stat=errStat2)
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for p%r.', errStat, errMsg, RoutineName )
   allocate(p%y(-p%NumRadii+1:p%NumRadii-1), stat=errStat2); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for p%y.', errStat, errMsg, RoutineName )
   allocate(p%z(-p%NumRadii+1:p%NumRadii-1), stat=errStat2); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for p%z.', errStat, errMsg, RoutineName )
   if (errStat /= ErrID_None) return
      
   do i = 0,p%NumRadii-1
      p%r(i)       = p%dr*i     
   end do
   do i = -p%NumRadii+1,p%NumRadii-1
      p%y(i)       = p%dr*i     
      p%z(i)       = p%dr*i     
   end do


   
   p%filtParam         = exp(-2.0_ReKi*pi*p%dt_low*InitInp%InputFileData%f_c)
   p%oneMinusFiltParam = 1.0_ReKi - p%filtParam
      !............................................................................................
      ! Define and initialize inputs here 
      !............................................................................................
   
   allocate( u%V_plane       (3,0:p%NumPlanes-1),stat=errStat2)
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for u%V_plane.', errStat, errMsg, RoutineName )
   allocate( u%Ct_azavg      (  0:p%NumRadii-1 ),stat=errStat2)
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for u%Ct_azavg.', errStat, errMsg, RoutineName )   
   allocate( u%Cq_azavg      (  0:p%NumRadii-1 ),stat=errStat2)
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for u%Cq_azavg.', errStat, errMsg, RoutineName )   
   if (errStat /= ErrID_None) return
   

         
      
      !............................................................................................
      ! Define outputs here
      !............................................................................................

   
   
      !............................................................................................
      ! Initialize states and misc vars : Note these are not the correct initializations because
      ! that would require valid input data, which we do not have here.  Instead we will check for
      ! an firstPass flag on the miscVars and if it is false we will properly initialize these state
      ! in CalcOutput or UpdateStates, as necessary.
      !............................................................................................
      
   allocate ( xd%xhat_plane       (3, 0:p%NumPlanes-1) , STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for xd%xhat_plane.', errStat, errMsg, RoutineName )   
   allocate ( xd%p_plane          (3, 0:p%NumPlanes-1) , STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for xd%p_plane.', errStat, errMsg, RoutineName )   
   allocate ( xd%V_plane_filt     (3, 0:p%NumPlanes-1) , STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for xd%V_plane_filt.', errStat, errMsg, RoutineName )
   allocate ( xd%Vx_wind_disk_filt(0:p%NumPlanes-1) , STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for xd%Vx_wind_disk_filt.', errStat, errMsg, RoutineName )   
   allocate ( xd%x_plane          (0:p%NumPlanes-1) , STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for xd%x_plane.', errStat, errMsg, RoutineName )   
   allocate ( xd%YawErr_filt      (0:p%NumPlanes-1) , STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for xd%YawErr_filt.', errStat, errMsg, RoutineName )   
   allocate ( xd%TI_amb_filt      (0:p%NumPlanes-1) , STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for xd%TI_amb_filt.', errStat, errMsg, RoutineName )   
   allocate ( xd%D_rotor_filt     (0:p%NumPlanes-1) , STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for xd%D_rotor_filt.', errStat, errMsg, RoutineName )   
   allocate ( xd%Ct_azavg_filt    (0:p%NumRadii-1) , STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for xd%Ct_azavg_filt.', errStat, errMsg, RoutineName )   
   allocate ( xd%Cq_azavg_filt    (0:p%NumRadii-1) , STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for xd%Cq_azavg_filt.', errStat, errMsg, RoutineName )   
   allocate ( xd%Vx_wake     (0:p%NumRadii-1,0:p%NumPlanes-1) , STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for xd%Vx_wake.', errStat, errMsg, RoutineName )   
   allocate ( xd%Vr_wake     (0:p%NumRadii-1,0:p%NumPlanes-1) , STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for xd%Vr_wake.', errStat, errMsg, RoutineName )   
   allocate ( xd%Vx_wake2   (-p%NumRadii+1:p%NumRadii-1,-p%NumRadii+1:p%NumRadii-1,0:p%NumPlanes-1), STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for xd%Vx_wake.', errStat, errMsg, RoutineName )  
   allocate ( xd%Vy_wake2   (-p%NumRadii+1:p%NumRadii-1,-p%NumRadii+1:p%NumRadii-1,0:p%NumPlanes-1), STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for xd%Vy_wake.', errStat, errMsg, RoutineName )  
   allocate ( xd%Vz_wake2   (-p%NumRadii+1:p%NumRadii-1,-p%NumRadii+1:p%NumRadii-1,0:p%NumPlanes-1), STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for xd%Vz_wake.', errStat, errMsg, RoutineName )  
   if (errStat /= ErrID_None) return

   xd%YawErr_filt         = 0.0_ReKi !NOTE: initialized in InitStatesWithInputs
   xd%psi_skew_filt       = 0.0_ReKi !NOTE: initialized in InitStatesWithInputs
   xd%chi_skew_filt       = 0.0_ReKi !NOTE: initialized in InitStatesWithInputs

   xd%xhat_plane          = 0.0_ReKi
   xd%p_plane             = 0.0_ReKi
   xd%x_plane             = 0.0_ReKi
   xd%Vx_wake             = 0.0_ReKi
   xd%Vr_wake             = 0.0_ReKi
   xd%Vx_wake2            = 0.0_ReKi
   xd%Vy_wake2            = 0.0_ReKi
   xd%Vz_wake2            = 0.0_ReKi
   xd%V_plane_filt        = 0.0_ReKi
   xd%Vx_wind_disk_filt   = 0.0_ReKi
   xd%TI_amb_filt         = 0.0_ReKi
   xd%D_rotor_filt        = 0.0_ReKi
   xd%Vx_rel_disk_filt    = 0.0_ReKi
   xd%Ct_azavg_filt       = 0.0_ReKi
   xd%Cq_azavg_filt       = 0.0_ReKi
   OtherState%firstPass            = .true.     
   
      ! miscvars to avoid the allocation per timestep
   if (p%Mod_Wake == Mod_Wake_Polar) then
      allocate (   m%dvtdr  (0:p%NumRadii-1 ) , STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%dvtdr.', errStat, errMsg, RoutineName )  
      allocate (   m%vt_tot (0:p%NumRadii-1,0:p%NumPlanes-1 ) , STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%vt_tot.', errStat, errMsg, RoutineName )  
      allocate (   m%vt_amb (0:p%NumRadii-1,0:p%NumPlanes-1 ) , STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%vt_amb.', errStat, errMsg, RoutineName )  
      allocate (   m%vt_shr (0:p%NumRadii-1,0:p%NumPlanes-1 ) , STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%vt_shr.', errStat, errMsg, RoutineName )  
   else if (p%Mod_Wake == Mod_Wake_Cartesian .or. p%Mod_Wake == Mod_Wake_Curl) then
      allocate (   m%vt_tot2(-p%NumRadii+1:p%NumRadii-1,-p%NumRadii+1:p%NumRadii-1,0:p%NumPlanes-1), STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%vt_tot2.', errStat, errMsg, RoutineName )  
      allocate (   m%vt_amb2(-p%NumRadii+1:p%NumRadii-1,-p%NumRadii+1:p%NumRadii-1,0:p%NumPlanes-1), STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%vt_amb2.', errStat, errMsg, RoutineName )  
      allocate (   m%vt_shr2(-p%NumRadii+1:p%NumRadii-1,-p%NumRadii+1:p%NumRadii-1,0:p%NumPlanes-1), STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%vt_shr2.', errStat, errMsg, RoutineName )  

      allocate (   m%dvx_dy   (-p%NumRadii+1:p%NumRadii-1,-p%NumRadii+1:p%NumRadii-1,0:p%NumPlanes-1), STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%dvx_dy.', errStat, errMsg, RoutineName )  
      allocate (   m%dvx_dz   (-p%NumRadii+1:p%NumRadii-1,-p%NumRadii+1:p%NumRadii-1,0:p%NumPlanes-1), STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%dvx_dz.', errStat, errMsg, RoutineName )  
      allocate (   m%nu_dvx_dy(-p%NumRadii+1:p%NumRadii-1,-p%NumRadii+1:p%NumRadii-1), STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%nu_dvx_dy.', errStat, errMsg, RoutineName )  
      allocate (   m%nu_dvx_dz(-p%NumRadii+1:p%NumRadii-1,-p%NumRadii+1:p%NumRadii-1), STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%nu_dvx_dz.', errStat, errMsg, RoutineName )  
      allocate (   m%dnuvx_dy (-p%NumRadii+1:p%NumRadii-1,-p%NumRadii+1:p%NumRadii-1), STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%dnuvx_dy.', errStat, errMsg, RoutineName )  
      allocate (   m%dnuvx_dz (-p%NumRadii+1:p%NumRadii-1,-p%NumRadii+1:p%NumRadii-1), STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%dnuvx_dz.', errStat, errMsg, RoutineName )  
      m%vt_tot2   = 0.0_ReKi
      m%vt_amb2   = 0.0_ReKi
      m%vt_shr2   = 0.0_ReKi
      m%dvx_dy    = 0.0_ReKi
      m%dvx_dz    = 0.0_ReKi
      m%nu_dvx_dy = 0.0_ReKi
      m%nu_dvx_dz = 0.0_ReKi
      m%dnuvx_dy  = 0.0_ReKi
      m%dnuvx_dz  = 0.0_ReKi
   else
      STOP ! should never happen
   endif


   allocate (    m%a(0:p%NumRadii-1 ) , STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%a.', errStat, errMsg, RoutineName )  
   allocate (    m%b(0:p%NumRadii-1 ) , STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%b.', errStat, errMsg, RoutineName )  
   allocate (    m%c(0:p%NumRadii-1 ) , STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%c.', errStat, errMsg, RoutineName )  
   allocate (    m%d(0:p%NumRadii-1 ) , STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%d.', errStat, errMsg, RoutineName )  
   allocate (    m%r_wake(0:p%NumRadii-1 ) , STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%r_wake.', errStat, errMsg, RoutineName )  
   allocate (    m%Vx_high(0:p%NumRadii-1 ) , STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%Vx_high.', errStat, errMsg, RoutineName )  
   allocate (    m%Vx_polar(0:p%NumRadii-1 ) , STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for m%Vx_polar.', errStat, errMsg, RoutineName )  
   if (errStat /= ErrID_None) return
   
      !............................................................................................
      ! Define initialization output here
      !............................................................................................
   
   InitOut%Ver = WD_Ver
   
   allocate ( y%xhat_plane(3,0:p%NumPlanes-1), STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for y%xhat_plane.', errStat, errMsg, RoutineName )     
   allocate ( y%p_plane   (3,0:p%NumPlanes-1), STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for y%p_plane.', errStat, errMsg, RoutineName )  
   ! TODO consider renaming these variables to "polar", and place in misc. Kept for now, might be needed for radial outputs
   allocate ( y%Vx_wake   (0:p%NumRadii-1,0:p%NumPlanes-1), STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for y%Vx_wake.', errStat, errMsg, RoutineName )  
   allocate ( y%Vr_wake   (0:p%NumRadii-1,0:p%NumPlanes-1), STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for y%Vr_wake.', errStat, errMsg, RoutineName )  

   allocate ( y%Vx_wake2   (-p%NumRadii+1:p%NumRadii-1,-p%NumRadii+1:p%NumRadii-1,0:p%NumPlanes-1), STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for y%Vx_wake.', errStat, errMsg, RoutineName )  
   allocate ( y%Vy_wake2   (-p%NumRadii+1:p%NumRadii-1,-p%NumRadii+1:p%NumRadii-1,0:p%NumPlanes-1), STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for y%Vy_wake.', errStat, errMsg, RoutineName )  
   allocate ( y%Vz_wake2   (-p%NumRadii+1:p%NumRadii-1,-p%NumRadii+1:p%NumRadii-1,0:p%NumPlanes-1), STAT=ErrStat2 ); if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for y%Vz_wake.', errStat, errMsg, RoutineName )  

   allocate ( y%D_wake    (0:p%NumPlanes-1), STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for y%D_wake.', errStat, errMsg, RoutineName )  
   allocate ( y%x_plane   (0:p%NumPlanes-1), STAT=ErrStat2 )
      if (errStat2 /= 0) call SetErrStat ( ErrID_Fatal, 'Could not allocate memory for y%x_plane.', errStat, errMsg, RoutineName )  
   if (errStat /= ErrID_None) return
   
   y%xhat_plane = 0.0_Reki
   y%p_plane    = 0.0_Reki
   y%Vx_wake    = 0.0_Reki
   y%Vr_wake    = 0.0_Reki
   y%Vx_wake2   = 0.0_Reki
   y%Vy_wake2   = 0.0_Reki
   y%Vz_wake2   = 0.0_Reki
   y%D_wake     = 0.0_Reki
   y%x_plane    = 0.0_Reki

      
end subroutine WD_Init

!----------------------------------------------------------------------------------------------------------------------------------
!> This routine is called at the end of the simulation.
subroutine WD_End( u, p, x, xd, z, OtherState, y, m, errStat, errMsg )
!..................................................................................................................................

      type(WD_InputType),           intent(inout)  :: u           !< System inputs
      type(WD_ParameterType),       intent(inout)  :: p           !< Parameters
      type(WD_ContinuousStateType), intent(inout)  :: x           !< Continuous states
      type(WD_DiscreteStateType),   intent(inout)  :: xd          !< Discrete states
      type(WD_ConstraintStateType), intent(inout)  :: z           !< Constraint states
      type(WD_OtherStateType),      intent(inout)  :: OtherState  !< Other states
      type(WD_OutputType),          intent(inout)  :: y           !< System outputs
      type(WD_MiscVarType),         intent(inout)  :: m           !< Misc/optimization variables
      integer(IntKi),               intent(  out)  :: errStat     !< Error status of the operation
      character(*),                 intent(  out)  :: errMsg      !< Error message if errStat /= ErrID_None



         ! Initialize errStat

      errStat = ErrID_None
      errMsg  = ""


         ! Place any last minute operations or calculations here:


         ! Close files here:



         ! Destroy the input data:

      call WD_DestroyInput( u, errStat, errMsg )


         ! Destroy the parameter data:

      call WD_DestroyParam( p, errStat, errMsg )


         ! Destroy the state data:

      call WD_DestroyContState(   x,           errStat, errMsg )
      call WD_DestroyDiscState(   xd,          errStat, errMsg )
      call WD_DestroyConstrState( z,           errStat, errMsg )
      call WD_DestroyOtherState(  OtherState,  errStat, errMsg )
      call WD_DestroyMisc(        m,           errStat, errMsg ) 

         ! Destroy the output data:

      call WD_DestroyOutput( y, errStat, errMsg )




end subroutine WD_End
!----------------------------------------------------------------------------------------------------------------------------------
!> Loose coupling routine for solving for constraint states, integrating continuous states, and updating discrete and other states.
!! Continuous, constraint, discrete, and other states are updated for t + Interval
subroutine WD_UpdateStates( t, n, u, p, x, xd, z, OtherState, m, errStat, errMsg )
!..................................................................................................................................

   real(DbKi),                     intent(in   ) :: t          !< Current simulation time in seconds
   integer(IntKi),                 intent(in   ) :: n          !< Current simulation time step n = 0,1,...
   type(WD_InputType),             intent(in   ) :: u          !< Inputs at utimes (out only for mesh record-keeping in ExtrapInterp routine)
  ! real(DbKi),                     intent(in   ) :: utimes   !< Times associated with u(:), in seconds
   type(WD_ParameterType),         intent(in   ) :: p          !< Parameters
   type(WD_ContinuousStateType),   intent(inout) :: x          !< Input: Continuous states at t;
                                                               !!   Output: Continuous states at t + Interval
   type(WD_DiscreteStateType),     intent(inout) :: xd         !< Input: Discrete states at t;
                                                               !!   Output: Discrete states at t  + Interval
   type(WD_ConstraintStateType),   intent(inout) :: z          !< Input: Constraint states at t;
                                                               !!   Output: Constraint states at t+dt
   type(WD_OtherStateType),        intent(inout) :: OtherState !< Input: Other states at t;
                                                               !!   Output: Other states at t+dt
   type(WD_MiscVarType),           intent(inout) :: m          !< Misc/optimization variables
   integer(IntKi),                 intent(  out) :: errStat    !< Error status of the operation
   character(*),                   intent(  out) :: errMsg     !< Error message if errStat /= ErrID_None

   ! local variables
   type(WD_InputType)                           :: uInterp           ! Interpolated/Extrapolated input
   integer(intKi)                               :: errStat2          ! temporary Error status
   character(ErrMsgLen)                         :: errMsg2           ! temporary Error message
   character(*), parameter                      :: RoutineName = 'WD_UpdateStates'
   real(ReKi)                                   :: dx, norm2_xhat_plane  
   real(ReKi)                                   :: dy_HWkDfl(3), EddyTermA, EddyTermB, lstar, Vx_wake_min
   real(ReKi)                                   :: dvdr
   integer(intKi)                               :: i,j, maxPln
   integer(intKi)                               :: iy, iz ! indices on y and z
   
   errStat = ErrID_None
   errMsg  = ""
   
   print'(A,F9.3)','-------------------------- WD_UpdateStates  t = ',t
   
   if ( EqualRealNos(u%D_Rotor,0.0_ReKi) .or. u%D_Rotor < 0.0_ReKi ) then
      ! TEST: E7
      call SetErrStat(ErrID_Fatal, 'Rotor diameter must be greater than zero.', errStat, errMsg, RoutineName)
      return
   end if
   
      ! Check if we are fully initialized
   if ( OtherState%firstPass ) then
      call InitStatesWithInputs(p%NumPlanes, p%NumRadii, u, p, xd, m, errStat2, errMsg2)
         call SetErrStat(errStat2, errMsg2, errStat, errMsg, RoutineName)
      OtherState%firstPass = .false.        
      if (errStat >= AbortErrLev) then
         ! TEST: E3 
         return
      end if
      
   end if         
   

   ! --------------------------------------------------------------------------------
   ! --- Update states for all planes except disk plane
   ! --------------------------------------------------------------------------------
   ! --- Update V_plane_filt to [n+1]:
   maxPln = min(n,p%NumPlanes-2)
   do i = 0,maxPln 
      xd%V_plane_filt(:,i       ) = xd%V_plane_filt(:,i)*p%filtParam + u%V_plane(:,i       )*p%oneMinusFiltParam
   end do
   xd%V_plane_filt   (:,maxPln+1) =                                    u%V_plane(:,maxPln+1)
   
   maxPln = min(n+2,p%NumPlanes-1)

   
   ! --- Compute eddy viscosity terms
   ! compute eddy-viscosity terms for all planes, NOTE: starting from maxPln+1 here
   do i = maxPln+1, 1, -1  
      lstar = WakeDiam( p%Mod_WakeDiam, p%numRadii, p%dr, p%r, xd%Vx_wake(:,i-1), xd%Vx_wind_disk_filt(i-1), xd%D_rotor_filt(i-1), p%C_WakeDiam) / 2.0_ReKi     

      Vx_wake_min = huge(ReKi)
      do j = 0,p%NumRadii-1
         Vx_wake_min = min(Vx_wake_min, xd%Vx_wake(j,i-1))
      end do
        
      EddyTermA = EddyFilter(xd%x_plane(i-1),xd%D_rotor_filt(i-1), p%C_vAmb_DMin, p%C_vAmb_DMax, p%C_vAmb_FMin, p%C_vAmb_Exp) * p%k_vAmb * xd%TI_amb_filt(i-1) * xd%Vx_wind_disk_filt(i-1) * xd%D_rotor_filt(i-1)/2.0_ReKi
      EddyTermB = EddyFilter(xd%x_plane(i-1),xd%D_rotor_filt(i-1), p%C_vShr_DMin, p%C_vShr_DMax, p%C_vShr_FMin, p%C_vShr_Exp) * p%k_vShr
      if (p%Mod_Wake == Mod_Wake_Polar) then
         ! Polar grid
         do j = 0,p%NumRadii-1      
            if ( j == 0 ) then
             dvdr =   0.0_ReKi
            elseif (j <= p%NumRadii-2) then
               dvdr = ( xd%Vx_wake(j+1,i-1) - xd%Vx_wake(j-1,i-1) ) / (2_ReKi*p%dr)
            else
               dvdr = - xd%Vx_wake(j-1,i-1)  / (2_ReKi*p%dr)
            end if
               ! All of the following states are at [n] 
            m%vt_amb(j,i-1) = EddyTermA
            m%vt_shr(j,i-1) = EddyTermB * max( (lstar**2)*abs(dvdr) , lstar*(xd%Vx_wind_disk_filt(i-1) + Vx_wake_min ) )
            m%vt_tot(j,i-1) = m%vt_amb(j,i-1) + m%vt_shr(j,i-1)                                                   
         end do
      else if (p%Mod_Wake == Mod_Wake_Cartesian .or. p%Mod_Wake == Mod_Wake_Curl) then
         ! First compute gradients of dVx/dy and dVx/dz
         ! call gradient_y(m%Vx_wake2(:,:,i-1), dy, m%dvx_dy(:,:,i-1))
         ! call gradient_z(m%Vx_wake2(:,:,i-1), dz, m%dvy_dz(:,:,i-1))
         print*,'TODO gradient y,z'

         ! Eddy viscosity
         do iz = -p%NumRadii+1, p%NumRadii-1
            do iy = -p%NumRadii+1, p%NumRadii-1
               dvdr = sqrt(m%dvx_dy(iy,iz,i-1)**2 + m%dvx_dz(iy,iz,i-1)**2)
               m%vt_amb2(iy,iz,i-1) = EddyTermA
               m%vt_shr2(iy,iz,i-1) = EddyTermB * max( (lstar**2)*dvdr , lstar*(xd%Vx_wind_disk_filt(i-1) + Vx_wake_min ) )
               m%vt_tot2(iy,iz,i-1) = m%vt_amb2(iy,iz,i-1)+ m%vt_shr2(iy,iz,i-1)
            enddo
         enddo
      endif
   end do ! loop on planes i = maxPln+1, 1, -1
  
   ! --- Update Vx and Vr
   if (p%Mod_Wake == Mod_Wake_Polar) then
      call updateVelocityPolar()
   else if (p%Mod_Wake == Mod_Wake_Cartesian .or. p%Mod_Wake == Mod_Wake_Curl) then
      call updateVelocityCartesian()
   else
      ! Should never happen
   endif
   if (errStat >= AbortErrLev) then
      call CleanUp()
      return
   endif
 
   ! --- Update plane positions and filtered states
   do i = maxPln, 1, -1  
      ! dx  = xd%x_plane(i) - xd%x_plane(i-1)
      dx = dot_product(xd%xhat_plane(:,i-1),xd%V_plane_filt(:,i-1))*p%DT_low

      ! Update these states to [n+1]
      xd%x_plane     (i) = xd%x_plane    (i-1) + abs(dx)   ! dx = dot_product(xd%xhat_plane(:,i-1),xd%V_plane_filt(:,i-1))*p%DT_low ; don't use absdx here  
      xd%YawErr_filt (i) = xd%YawErr_filt(i-1)
      xd%xhat_plane(:,i) = xd%xhat_plane(:,i-1)
      
      ! The function state-related arguments must be at time [n+1], so we must update YawErr_filt and xhat_plane before computing the deflection
      dy_HWkDfl = GetYawCorrection(xd%YawErr_filt(i), xd%xhat_plane(:,i), dx, p, errStat2, errMsg2)
         call SetErrStat(errStat2, errMsg2, errStat, errMsg, RoutineName)   
         if (errStat >= AbortErrLev) then
            ! TEST: E3          
            call Cleanup()
            return
         end if
      xd%p_plane        (:,i) =  xd%p_plane(:,i-1) + xd%xhat_plane(:,i-1)*dx + dy_HWkDfl &
                              + ( u%V_plane(:,i-1) - xd%xhat_plane(:,i-1)*dot_product(xd%xhat_plane(:,i-1),u%V_plane(:,i-1)) )*p%DT_low
         
      xd%Vx_wind_disk_filt(i) = xd%Vx_wind_disk_filt(i-1)
      xd%TI_amb_filt      (i) = xd%TI_amb_filt(i-1)
      xd%D_rotor_filt     (i) = xd%D_rotor_filt(i-1)

   end do ! loop on planes i = maxPln+1, 1, -1
      
   ! --------------------------------------------------------------------------------
   ! --- Update states at disk-plane (0) to time [n+1] 
   ! --------------------------------------------------------------------------------
   !xd%x_plane         (0) = 0.0_ReKi ! already initialized to zero
   xd%xhat_plane     (:,0) =  xd%xhat_plane(:,0)*p%filtParam + u%xhat_disk(:)*p%oneMinusFiltParam  ! 2-step calculation for xhat_plane at disk
   norm2_xhat_plane        =  TwoNorm( xd%xhat_plane(:,0) ) 
   if ( EqualRealNos(norm2_xhat_plane, 0.0_ReKi) ) then
      ! TEST: E1
      call SetErrStat(ErrID_FATAL, 'The nacelle-yaw has rotated 180 degrees between time steps, i.e., the L2 norm of xd%xhat_plane(:,0)*p%filtParam + u%xhat_disk(:)*(1-p%filtParam) is zero.', errStat, errMsg, RoutineName) 
      call Cleanup()
      return
   end if
   xd%xhat_plane     (:,0) =  xd%xhat_plane(:,0) / norm2_xhat_plane
   
   xd%psi_skew_filt        =  filter_angles(xd%psi_skew_filt, u%psi_skew, p%filtParam, p%oneMinusFiltParam)
   xd%chi_skew_filt        =  xd%chi_skew_filt *p%filtParam + u%chi_skew*p%oneMinusFiltParam
   xd%YawErr_filt      (0) =  xd%YawErr_filt(0)*p%filtParam + u%YawErr  *p%oneMinusFiltParam
   if ( EqualRealNos(abs(xd%YawErr_filt(0)), pi/2) .or. abs(xd%YawErr_filt(0)) > pi/2 ) then
      ! TEST: E4
      call SetErrStat(ErrID_FATAL, 'The time-filtered nacelle-yaw error has reached +/- pi/2.', errStat, errMsg, RoutineName) 
      call Cleanup()
      return
   end if
   
   ! The function state-related arguments must be at time [n+1], so we must update YawErr_filt and xhat_plane before computing the deflection
   dx = 0.0_ReKi
   dy_HWkDfl = GetYawCorrection(xd%YawErr_filt(0), xd%xhat_plane(:,0), dx, p, errStat2, errMsg2)
   call SetErrStat(ErrStat2, ErrMsg2, errStat, errMsg, RoutineName)   
   if (errStat /= ErrID_None) then
      ! TEST: E3
      call Cleanup()
      return
   end if

   ! Disk plane states filtered based on inputs
   xd%p_plane        (:,0) =  xd%p_plane(:,0)        *p%filtParam + ( u%p_hub(:) + dy_HWkDfl(:) )*p%oneMinusFiltParam
   xd%Vx_wind_disk_filt(0) =  xd%Vx_wind_disk_filt(0)*p%filtParam + u%Vx_wind_disk               *p%oneMinusFiltParam
   xd%TI_amb_filt      (0) =  xd%TI_amb_filt(0)      *p%filtParam + u%TI_amb                     *p%oneMinusFiltParam
   xd%D_rotor_filt     (0) =  xd%D_rotor_filt(0)     *p%filtParam + u%D_rotor                    *p%oneMinusFiltParam  
   xd%Vx_rel_disk_filt     =  xd%Vx_rel_disk_filt    *p%filtParam + u%Vx_rel_disk                *p%oneMinusFiltParam 
   
   !  filtered, azimuthally-averaged Ct values at each radial station
   xd%Ct_azavg_filt (:) = xd%Ct_azavg_filt(:)*p%filtParam + u%Ct_azavg(:)*p%oneMinusFiltParam
   xd%Cq_azavg_filt (:) = xd%Cq_azavg_filt(:)*p%filtParam + u%Cq_azavg(:)*p%oneMinusFiltParam
   
   ! --- Set velocity at disk plane
   if (p%Mod_Wake == Mod_Wake_Polar) then
      call updateVelocityPolar()
      ! Compute wake deficit of first plane based on rotor loading, outputs: Vx_Wake, m
      call NearWakeCorrection( xd%Ct_azavg_filt, xd%Vx_rel_disk_filt, p, m, xd%Vx_wake(:,0), xd%D_rotor_filt(0), errStat, errMsg )
   else if (p%Mod_Wake == Mod_Wake_Cartesian .or. p%Mod_Wake == Mod_Wake_Curl) then

      ! --- Compute Vx
      ! Compute Vx(r)
      call NearWakeCorrection( xd%Ct_azavg_filt, xd%Vx_rel_disk_filt, p, m, m%Vx_polar(:), xd%D_rotor_filt(0), errStat, errMsg )
      ! Convert to Cartesian
      call Axisymmetric2CartesianVx(m%Vx_polar, p%r, p%y, p%z, xd%Vx_wake2(:,:,0))

      !CT_avg = getCT_avg( xd%Ct_azavg_filt, xd%Vx_rel_disk_filt, p, m, m%Vx_polar(:), xd%D_rotor_filt(0), errStat, errMsg )
      ! --- Compute Vy
      ! --- Add V/W from vorticies 
      if (p%Mod_Wake == Mod_Wake_Curl) then
         !call VelocityCurl(nVortex, Gamma0  xd%Vy_wake2(:,:,0), xd%Vz_wake2(:,:,0)  )
      endif
      ! --- Add Swirl
      !call AddSwirl(  xd%Vy_wake2(:,:,0), xd%Vz_wake2(:,:,0)  )
      ! 
   endif

   !Used for debugging: write(51,'(I5,100(1x,ES10.2E2))') n, xd%x_plane(n), xd%x_plane(n)/xd%D_rotor_filt(n), xd%Vx_wind_disk_filt(n) + xd%Vx_wake(:,n), xd%Vr_wake(:,n)    
   
   call Cleanup()
   
contains
      
   subroutine updateVelocityPolar()
      integer(intKi) :: i,j
      real(ReKi)  :: dx, absdx
      ! The quantities in these loops are all at time [n], so we need to compute prior to updating the states to [n+1] (loop in reversed)
      do i = maxPln, 1, -1  
         ! dx is used instead of delta t since plane are updated based on their indices
         !           [n+1]             [n]
         ! dx      = xd%x_plane(i) - xd%x_plane(i-1)
         dx = dot_product(xd%xhat_plane(:,i-1),xd%V_plane_filt(:,i-1))*p%DT_low
         absdx = abs(dx)
         if ( EqualRealNos( dx, 0.0_ReKi ) ) absdx = 1.0_ReKi  ! This is to avoid division by zero problems in the formation of m%b and m%d below, which are not used when dx=0; the value of unity is arbitrary

         ! All of the m%a,m%b,m%c,m%d vectors use states at time increment [n]
         ! These need to be inside another radial loop because m%dvtdr depends on the j+1 and j-1 indices of m%vt()
         m%dvtdr(0) = 0.0_ReKi
         m%a(0)     = 0.0_ReKi
         m%b(0)     = p%dr * ( xd%Vx_wind_disk_filt(i-1) + xd%Vx_wake(0,i-1)) / absdx + m%vt_tot(0,i-1)/p%dr
         m%c(0)     = -m%vt_tot(0,i-1)/p%dr
         m%c(p%NumRadii-1) = 0.0_ReKi
         m%d(0)     = (p%dr * (xd%Vx_wind_disk_filt(i-1) + xd%Vx_wake(0,i-1)) / absdx - m%vt_tot(0,i-1)/p%dr  ) * xd%Vx_wake(0,i-1) + ( m%vt_tot(0,i-1)/p%dr ) * xd%Vx_wake(1,i-1) 
         
         do j = p%NumRadii-1, 1, -1 
            
            if (j <= p%NumRadii-2) then
               m%dvtdr(j) = ( m%vt_tot(j+1,i-1) - m%vt_tot(j-1,i-1) ) / (2_ReKi*p%dr)
               m%c(j) = real(j,ReKi)*xd%Vr_wake(j,i-1)/4.0_ReKi - (1_ReKi+2_ReKi*real(j,ReKi))*m%vt_tot(j,i-1)/(4.0_ReKi*p%dr) - real(j,ReKi)*m%dvtdr(j)/4.0_ReKi
               m%d(j) =    ( real(j,ReKi)*xd%Vr_wake(j,i-1)/4.0_ReKi - (1_ReKi-2_ReKi*real(j,ReKi))*m%vt_tot(j,i-1)/(4.0_ReKi*p%dr) - real(j,ReKi)*m%dvtdr(j)/4.0_ReKi) * xd%Vx_wake(j-1,i-1) &
                       + ( p%r(j)*( xd%Vx_wind_disk_filt(i-1) + xd%Vx_wake(j,i-1)  )/absdx -  real(j,ReKi)*m%vt_tot(j,i-1)/p%dr  ) * xd%Vx_wake(j,i-1) &
                       + (-real(j,ReKi)*xd%Vr_wake(j,i-1)/4.0_ReKi + (1_ReKi+2_ReKi*real(j,ReKi))*m%vt_tot(j,i-1)/(4.0_ReKi*p%dr) + real(j,ReKi)*m%dvtdr(j)/4.0_ReKi ) * xd%Vx_wake(j+1,i-1)
                
            else
               m%dvtdr(j) = 0.0_ReKi
               m%d(j) = ( real(j,ReKi)*xd%Vr_wake(j,i-1)/4.0_ReKi - (1_ReKi-2_ReKi*real(j,ReKi))*m%vt_tot(j,i-1)/(4.0_ReKi*p%dr) - real(j,ReKi)*m%dvtdr(j)/4.0_ReKi) * xd%Vx_wake(j-1,i-1) &
                       + ( p%r(j)*( xd%Vx_wind_disk_filt(i-1) + xd%Vx_wake(j,i-1)  )/absdx -  real(j,ReKi)*m%vt_tot(j,i-1)/p%dr  ) * xd%Vx_wake(j,i-1) 
                       
            end if  
            
            m%a(j) = -real(j,ReKi)*xd%Vr_wake(j,i-1)/4.0_ReKi + (1.0_ReKi-2.0_ReKi*real(j,ReKi))*m%vt_tot(j,i-1)/(4.0_ReKi*p%dr) + real(j,ReKi)*m%dvtdr(j)/4.0_ReKi 
            m%b(j) = p%r(j) * ( xd%Vx_wind_disk_filt(i-1) + xd%Vx_wake(j,i-1)  ) / absdx + real(j,ReKi)*m%vt_tot(j,i-1)/p%dr
           
         end do ! j = 1,p%NumRadii-1
      
         ! Update Vx_wake and Vr_wake to [n+1]
         if ( EqualRealNos( dx, 0.0_ReKi ) ) then
            xd%Vx_wake(:,i) = xd%Vx_wake(:,i-1)
            xd%Vr_wake(:,i) = xd%Vr_wake(:,i-1)
         else
            call ThomasAlgorithm(p%NumRadii, m%a, m%b, m%c, m%d, xd%Vx_wake(:,i), errStat2, errMsg2)
               call SetErrStat(errStat2, errMsg2, errStat, errMsg, RoutineName) 
               if (errStat >= AbortErrLev) then
                  ! TEST: E16, E17, or E18
                  return
               end if  
            do j = 1,p%NumRadii-1
               ! NOTE: xd%Vr_wake(0,:) was initialized to 0 and remains 0.
               xd%Vr_wake(j,i) = real(  j-1,ReKi)*(  xd%Vr_wake(j-1,i)  )/real(j,ReKi) &
               !  Vx_wake is for the                           [n+1]       ,      [n+1]        ,      [n]          , and    [n]        increments             
                               - real(2*j-1,ReKi)*p%dr * (  xd%Vx_wake(j,i) + xd%Vx_wake(j-1,i) - xd%Vx_wake(j,i-1) - xd%Vx_wake(j-1,i-1)  ) / ( real(4*j,ReKi) * absdx )
            end do  
         end if
      end do ! i = 1,min(n+2,p%NumPlanes-1) 
   end subroutine updateVelocityPolar

   !> 
   subroutine updateVelocityCartesian()
      integer(intKi) :: iy,iz,i
      real(ReKi)  :: dx
      real(ReKi)  :: xp !< x position of the plane
      real(ReKi)  :: divTau  
      divTau =0.0_ReKi
      ! The quantities in these loops are all at time [n], so we need to compute prior to updating the states to [n+1] (loop in reversed)
      do i = maxPln, 1, -1  

         ! NOTE: we cannot use data at i here since positions of planes have not been updated yet
         dx = dot_product(xd%xhat_plane(:,i-1),xd%V_plane_filt(:,i-1))*p%DT_low
         !xp = xd%p_plane(1,i-1)/u%D_rotor ! Current plane downstream x position in D
         xp = (xd%x_plane(i-1) + abs(dx))/u%D_rotor 
         !print*,'Decay factor', exp(-p%k_VortexDecay * xp), 'at i=',i,'x/D=',xp

         ! Gradients for eddy viscosity term 
         m%nu_dvx_dy(:,:) = m%vt_tot2(:,:,i-1) * m%dvx_dy(:,:,i-1)
         m%nu_dvx_dz(:,:) = m%vt_tot2(:,:,i-1) * m%dvx_dz(:,:,i-1)
         ! call gradient_y(m%nu_dvx_dy, dy, m%dnuvx_dy   )
         ! call gradient_z(m%nu_dvx_dz, dz, m%dnuvx_dz   )
         !
         do iz = -p%NumRadii+1, p%NumRadii-1
            do iy = -p%NumRadii+1, p%NumRadii-1
               ! Eddy viscosity term
               divTau = m%dnuvx_dy(iy,iz) + m%dnuvx_dz(iy,iz)
               ! Update state of Vx
               xd%Vx_wake2(iy,iz,i)  = xd%Vx_wake2(iy,iz,i-1) + dx/(xd%Vx_wake2(iy,iz,i-1)+xd%Vx_wind_disk_filt(i-1))&
                                        *(xd%Vy_wake2(iy,iz,i-1) * m%dvx_dy(iy,iz,i-1) + xd%Vz_wake2(iy,iz,i-1) * m%dvx_dz(iy,iz,i-1) &
                                     + divTau) 
               ! Update sate (decay) of Vy and Vz
               xd%Vy_wake2(iy,iz,i)  = xd%Vy_wake2(iy,iz,i-1) * exp( - p%k_VortexDecay * xp)
               xd%Vz_wake2(iy,iz,i)  = xd%Vz_wake2(iy,iz,i-1) * exp( - p%k_VortexDecay * xp)
               !xd%Vx_wake2(iy,iz,i)  = xd%Vx_wake2(iy,iz,i-1) * exp( - p%k_VortexDecay * xp) ! HACK HACK HACK
            enddo ! iy
         enddo ! iz
      enddo ! i, planes

   end subroutine updateVelocityCartesian


   subroutine Cleanup()

   end subroutine Cleanup
   
end subroutine WD_UpdateStates

! subroutine VelocityCurl(Gamma0, nVortex, R, psi_skew, Vy_curl, Vz_curl)
!     real(ReKi), parameter :: sigma_D = 0.2
!     real(ReKi) :: sigma 
! 
!    !z0=zi * cos(psi_skew)
! end subroutine

subroutine WD_TEST_VelocityCurl()
   ! See WD_TEST_Axi2Cart for inspiration
   ! real(ReKi) :: Vy_curl(:,:)
   ! real(ReKi) :: Vy_curl_ref(:,:)
   ! allocate(Vy_curl(2,2))
   ! allocate(Vy_curl_ref(2,2))
   !Vy_curl_ref(1,1) =1.2
   !Vy_curl_ref(1,2) =1.3
   !Vy_curl_ref(2,2) =1.3

   ! call VelocityCurl()
   !for 
   !if (Vy_curl_ref(0,0)==Vy_curl)
   !
   print*,'Test OK'
end subroutine



!> Weighted average of two angles
function filter_angles(t1, t2, alpha, alpha_bar) result (t_filt)
   real(ReKi), intent(in) :: t1 !< Angle 1
   real(ReKi), intent(in) :: t2 !< Angle 2
   real(ReKi), intent(in) :: alpha     !< filter weight
   real(ReKi), intent(in) :: alpha_bar !< 1-alpha
   real(ReKi) :: t_filt !< output
   real(ReKi) :: x,y
   ! exp(i t1 alpha)* exp(i t2 alpha_bar ) = exp( i [ t1 alpha + (1-alpha) t2] )
   ! alpha exp(i t1) + alpha_bar exp(i t2)
   x = alpha*cos(t1)+alpha_bar*cos(t2)
   y = alpha*sin(t1)+alpha_bar*sin(t2)
   t_filt = atan2(y,x) 

   ! TODO, Dig into mesh mapping, Ask bonnie to find the proper routine
   !call Angles_ExtrapInterp(t1, t2, tin, Angle_out, tin_out)

end function filter_angles

!> Converts velocity vectors from an axisymmetric system in radial coordinates to a Cartesian system in Cartesian coordinates
subroutine Axisymmetric2Cartesian(Vx_axi, Vr_axi, r, y, z, Vx, Vy, Vz)
   real(ReKi), dimension(:),   intent(in)    :: Vx_axi !< Axial velocity, distributed radially (m/s)
   real(ReKi), dimension(:),   intent(in)    :: Vr_axi !< Radial velocity deficit, distributed radially (m/s)
   real(ReKi), dimension(:),   intent(in)    :: r      !< Discretization of radial finite-difference grid (m
   real(ReKi), dimension(:),   intent(in)    :: y      !< Horizontal discretization of Cartesian grid (m)
   real(ReKi), dimension(:),   intent(in)    :: z      !< Nominally vertical discretization of Cartesian grid (m)
   real(ReKi), dimension(:,:), intent(inout) :: Vx     !< Axial velocity, distributed across the plane (m/s)
   real(ReKi), dimension(:,:), intent(inout) :: Vy     !< Transverse horizontal velocity, distributed across the plane (m/s)
   real(ReKi), dimension(:,:), intent(inout) :: Vz     !< Transverse nominally vertical velocity, distributed across the plane (m/s)
   integer(IntKi) :: iz, iy, nr, iLow
   real(ReKi) :: r_tmp, r_max
   real(ReKi) :: Vr, S, C ! Sine and cosine
   nr = size(r)
   r_max = r(nr)
   do iz = 1,size(z)
      do iy = 1,size(y)
         r_tmp =  sqrt(y(iy)**2 + z(iz)**2) 
         if (EqualRealNos(r_tmp,0.0_ReKi) ) then
            S = 0.0_ReKi 
            C = 0.0_ReKi
         else
            S=z(iz)/r_tmp
            C=y(iy)/r_tmp
         endif
         r_tmp = min(r_tmp, r_max) ! we can't interpolate beyond r_max
         iLow=0
         Vx(iy,iz) = InterpBin(r_tmp, r, Vx_axi, iLow, nr) !( XVal, XAry, YAry, ILo, AryLen )
         Vr        = InterpBin(r_tmp, r, Vr_axi, iLow, nr)
         Vy(iy,iz) = Vr * C
         Vz(iy,iz) = Vr * S
      enddo
   enddo
end subroutine Axisymmetric2Cartesian

!> Converts velocity vector Vx from an axisymmetric system in radial coordinates to a Cartesian system in Cartesian coordinates
subroutine Axisymmetric2CartesianVx(Vx_axi, r, y, z, Vx)
   real(ReKi), dimension(:),   intent(in)    :: Vx_axi !< Axial velocity, distributed radially (m/s)
   real(ReKi), dimension(:),   intent(in)    :: r      !< Discretization of radial finite-difference grid (m
   real(ReKi), dimension(:),   intent(in)    :: y      !< Horizontal discretization of Cartesian grid (m)
   real(ReKi), dimension(:),   intent(in)    :: z      !< Nominally vertical discretization of Cartesian grid (m)
   real(ReKi), dimension(:,:), intent(inout) :: Vx     !< Axial velocity, distributed across the plane (m/s)
   integer(IntKi) :: iz, iy, nr, iLow
   real(ReKi) :: r_tmp, r_max
   real(ReKi) :: Vr, S, C ! Sine and cosine
   nr = size(r)
   r_max = r(nr)
   do iz = 1,size(z)
      do iy = 1,size(y)
         r_tmp =  sqrt(y(iy)**2 + z(iz)**2) 
         r_tmp = min(r_tmp, r_max) ! we can't interpolate beyond r_max
         iLow=0
         Vx(iy,iz) = InterpBin(r_tmp, r, Vx_axi, iLow, nr) !( XVal, XAry, YAry, ILo, AryLen )
      enddo
   enddo
end subroutine Axisymmetric2CartesianVx


subroutine WD_TEST_Axi2Cart()
   real(ReKi) :: r(4)=(/0.,1.,2.,3./)
!    real(ReKi) :: y(4)=(/-1.,0.,1.5,2./)
!    real(ReKi) :: z(5)=(/-2.5,-1.5,0.,1.5,2./)
   real(ReKi) :: y(4)=(/0.,1. ,1.5, 2./)
   real(ReKi) :: z(5)=(/0.,0.5,1. ,1.5,2./)
   real(ReKi) :: Vr_axi(4)
   real(ReKi) :: Vx_axi(4)
   real(ReKi) :: Vx(4,5)=0.0_ReKi
   real(ReKi) :: Vy(4,5)=0.0_ReKi
   real(ReKi) :: Vz(4,5)=0.0_ReKi
   integer :: i,j 
   real(ReKi) :: Vr, r_tmp
   Vr_axi=4._ReKi*r
   Vx_axi=3._ReKi*r
   call Axisymmetric2Cartesian(Vx_axi, Vr_axi, r, y, z, Vx, Vy, Vz)

   do i = 1,size(y)
      do j = 1,size(z)
         r_tmp = sqrt(y(i)**2+z(j)**2)
         Vr    = sqrt(Vy(i,j)**2 + Vz(i,j)**2)
         if (abs(Vr-4*r_tmp)>1e-3) then
            print*,'>>Error Axi2Cart Vr',Vr,4*r_tmp
            STOP
         endif
         if (abs(Vx(i,j)-3*r_tmp)>1e-3) then
            print*,'>>Error Axi2Cart Vx',Vx(i,j),3*r_tmp
            STOP
         endif
      enddo
   enddo
end subroutine 



!----------------------------------------------------------------------------------------------------------------------------------
!> Routine for computing outputs, used in both loose and tight coupling.
!! This subroutine is used to compute the output channels (motions and loads) and place them in the WriteOutput() array.
!! The descriptions of the output channels are not given here. Please see the included OutListParameters.xlsx sheet for
!! for a complete description of each output parameter.
subroutine WD_CalcOutput( t, u, p, x, xd, z, OtherState, y, m, errStat, errMsg )
! NOTE: no matter how many channels are selected for output, all of the outputs are calcalated
! All of the calculated output channels are placed into the m%AllOuts(:), while the channels selected for outputs are
! placed in the y%WriteOutput(:) array.
!..................................................................................................................................
   use FVW_VTK ! 

   REAL(DbKi),                   INTENT(IN   )  :: t           !< Current simulation time in seconds
   TYPE(WD_InputType),           INTENT(IN   )  :: u           !< Inputs at Time t
   TYPE(WD_ParameterType),       INTENT(IN   )  :: p           !< Parameters
   TYPE(WD_ContinuousStateType), INTENT(IN   )  :: x           !< Continuous states at t
   TYPE(WD_DiscreteStateType),   INTENT(IN   )  :: xd          !< Discrete states at t
   TYPE(WD_ConstraintStateType), INTENT(IN   )  :: z           !< Constraint states at t
   TYPE(WD_OtherStateType),      INTENT(IN   )  :: OtherState  !< Other states at t
   TYPE(WD_OutputType),          INTENT(INOUT)  :: y           !< Outputs computed at t (Input only so that mesh con-
                                                               !!   nectivity information does not have to be recalculated)
   type(WD_MiscVarType),         intent(inout)  :: m           !< Misc/optimization variables
   INTEGER(IntKi),               INTENT(  OUT)  :: errStat     !< Error status of the operation
   CHARACTER(*),                 INTENT(  OUT)  :: errMsg      !< Error message if errStat /= ErrID_None


   integer, parameter                           :: indx = 1  
   integer(intKi)                               :: n, i
   integer(intKi)                               :: ErrStat2
   character(ErrMsgLen)                         :: ErrMsg2
   character(*), parameter                      :: RoutineName = 'WD_CalcOutput'
   real(ReKi)                                   :: correction(3)
   character(256):: Filename
   type(FVW_VTK_Misc)   :: mvtk
   real(ReKi), dimension(3) :: dx
   errStat = ErrID_None
   errMsg  = ""

   print'(A,F9.3)','-------------------------- WD_CalcOutput at t = ',t

   n = nint(t/p%DT_low)
   
      ! Check if we are fully initialized
   if ( OtherState%firstPass ) then
                        
      correction = 0.0_ReKi
      do i = 0, 1
         y%x_plane(i) = u%Vx_rel_disk*real(i,ReKi)*real(p%DT_low,ReKi)
       
         correction = correction + GetYawCorrection(u%YawErr, u%xhat_disk, y%x_plane(i), p,  errStat2, errMsg2)
            call SetErrStat(errStat2, errMsg2, errStat, errMsg, RoutineName)
            if (errStat >= AbortErrLev) then
               ! TEST: E3
               return
            end if
      
         y%p_plane   (:,i) = u%p_hub(:) + y%x_plane(i)*u%xhat_disk(:) + correction
         y%xhat_plane(:,i) = u%xhat_disk(:)
                 
         
            ! NOTE: Since we are in firstPass=T, then xd%Vx_wake is already set to zero, so just pass that into WakeDiam
         y%D_wake(i)  =  WakeDiam( p%Mod_WakeDiam, p%NumRadii, p%dr, p%r, xd%Vx_wake(:,i), u%Vx_wind_disk, u%D_rotor, p%C_WakeDiam)
      end do
     
         ! Initialze Vx_wake; Vr_wake is already initialized to zero, so, we don't need to do that here.
      call NearWakeCorrection( u%Ct_azavg, u%Vx_rel_disk, p, m, y%Vx_wake(:,0), u%D_rotor, errStat, errMsg )
         if (errStat > AbortErrLev)  return
      y%Vx_wake(:,1) = y%Vx_wake(:,0) 
 
      
   else
      y%x_plane    = xd%x_plane
      y%p_plane    = xd%p_plane
      y%xhat_plane = xd%xhat_plane
      y%Vx_wake    = xd%Vx_wake
      y%Vr_wake    = xd%Vr_wake
      do i = 0, min(n+1,p%NumPlanes-1)
         
         y%D_wake(i)  =  WakeDiam( p%Mod_WakeDiam, p%NumRadii, p%dr, p%r, xd%Vx_wake(:,i), xd%Vx_wind_disk_filt(i), xd%D_rotor_filt(i), p%C_WakeDiam)

      end do
   end if

   if (p%Mod_Wake == Mod_Wake_Polar) then
      ! Convert to Cartesian
      do i = 0, min(n+1,p%NumPlanes-1)
         call Axisymmetric2Cartesian(y%Vx_wake(:,i), y%Vr_wake(:,i), p%r, p%y, p%z, y%Vx_wake2(:,:,i), y%Vy_wake2(:,:,i), y%Vz_wake2(:,:,i))
      enddo
   else if (p%Mod_Wake == Mod_Wake_Cartesian .or. p%Mod_Wake == Mod_Wake_Curl) then
      do i = 0, min(n+1,p%NumPlanes-1)
         y%Vx_wake2(:,:,i) = xd%Vx_wake2(:,:,i)
         y%Vy_wake2(:,:,i) = xd%Vy_wake2(:,:,i)
         y%Vz_wake2(:,:,i) = xd%Vz_wake2(:,:,i)
      enddo


      ! --- Temporary VTK output hack
      call vtk_misc_init(mvtk)
      call set_vtk_binary_format(.false.,mvtk) ! TODO binary fails

      if ( OtherState%firstPass ) then
         call MKDIR('vtk_ff_planes')
      endif
      do i = 0, min(n,p%NumPlanes-1) ! TODO changed to n
         if (EqualRealNos(t,0.0_DbKi) ) then
            write(Filename,'(A,I4.4,A)') 'vtk_ff_planes/PlaneOutputsAtPlane_',i,'_Init.vtk'
         else
            write(Filename,'(A,I4.4,A,I9.9,A)') 'vtk_ff_planes/PlaneOutputsAtPlane_',i,'_Time_',int(t*10),'.vtk'
         endif
         !print*,trim(Filename)
         if ( vtk_new_ascii_file(trim(filename),'vel',mvtk) ) then
            dx(1) = 0.0
            dx(2) = p%dr
            dx(3) = p%dr
            call vtk_dataset_structured_points((/xd%p_plane(1,i),xd%p_plane(2,i)-dx*p%NumRadii,xd%p_plane(3,i)-dx*p%NumRadii/),dx,(/1,p%NumRadii*2-1,p%NumRadii*2-1/),mvtk)
            call vtk_point_data_init(mvtk)
            call vtk_point_data_scalar_2D(xd%Vx_wake2(:,:,i),'Vx',mvtk) 
            call vtk_point_data_scalar_2D(xd%Vy_wake2(:,:,i),'Vy',mvtk) 
            call vtk_point_data_scalar_2D(xd%Vz_wake2(:,:,i),'Vz',mvtk) 
            call vtk_close_file(mvtk)
         endif

         ! --- Output Plane "per time"
         write(Filename,'(A,I9.9,A,I4.4,A)') 'vtk_ff_planes/PlaneOutputsAtTime_',int(t*100),'_Plane_',i,'.vtk'
         !print*,trim(Filename)
         if ( vtk_new_ascii_file(trim(filename),'vel',mvtk) ) then
            dx(1) = 0.0
            dx(2) = p%dr
            dx(3) = p%dr
            call vtk_dataset_structured_points((/0.0_ReKi,xd%p_plane(2,i)-dx*p%NumRadii,xd%p_plane(3,i)-dx*p%NumRadii/),dx,(/1,p%NumRadii*2-1,p%NumRadii*2-1/),mvtk)
            call vtk_point_data_init(mvtk)
            call vtk_point_data_scalar_2D(xd%Vx_wake2(:,:,i),'Vx',mvtk) 
            call vtk_point_data_scalar_2D(xd%Vy_wake2(:,:,i),'Vy',mvtk) 
            call vtk_point_data_scalar_2D(xd%Vz_wake2(:,:,i),'Vz',mvtk) 
            call vtk_close_file(mvtk)
         endif


      enddo

   endif
   
end subroutine WD_CalcOutput

!----------------------------------------------------------------------------------------------------------------------------------
!> Tight coupling routine for solving for the residual of the constraint state equations
subroutine WD_CalcConstrStateResidual( Time, u, p, x, xd, z, OtherState, m, z_residual, errStat, errMsg )
!..................................................................................................................................

   REAL(DbKi),                   INTENT(IN   )   :: Time        !< Current simulation time in seconds
   TYPE(WD_InputType),           INTENT(IN   )   :: u           !< Inputs at Time
   TYPE(WD_ParameterType),       INTENT(IN   )   :: p           !< Parameters
   TYPE(WD_ContinuousStateType), INTENT(IN   )   :: x           !< Continuous states at Time
   TYPE(WD_DiscreteStateType),   INTENT(IN   )   :: xd          !< Discrete states at Time
   TYPE(WD_ConstraintStateType), INTENT(IN   )   :: z           !< Constraint states at Time (possibly a guess)
   TYPE(WD_OtherStateType),      INTENT(IN   )   :: OtherState  !< Other states at Time
   TYPE(WD_MiscVarType),         INTENT(INOUT)   :: m           !< Misc/optimization variables
   TYPE(WD_ConstraintStateType), INTENT(INOUT)   :: Z_residual  !< Residual of the constraint state equations using
                                                                !!     the input values described above
   INTEGER(IntKi),               INTENT(  OUT)   :: errStat     !< Error status of the operation
   CHARACTER(*),                 INTENT(  OUT)   :: errMsg      !< Error message if errStat /= ErrID_None


   
      ! Local variables   
   integer, parameter                            :: indx = 1  
   integer(intKi)                                :: ErrStat2
   character(ErrMsgLen)                          :: ErrMsg2
   character(*), parameter                       :: RoutineName = 'WD_CalcConstrStateResidual'
   
   
   
   errStat = ErrID_None
   errMsg  = ""

         
   
   
end subroutine WD_CalcConstrStateResidual

subroutine InitStatesWithInputs(numPlanes, numRadii, u, p, xd, m, errStat, errMsg)

   integer(IntKi),               intent(in   )   :: numPlanes
   integer(IntKi),               intent(in   )   :: numRadii
   TYPE(WD_InputType),           intent(in   )   :: u           !< Inputs at Time
   TYPE(WD_ParameterType),       intent(in   )   :: p           !< Parameters
   TYPE(WD_DiscreteStateType),   intent(inout)   :: xd          !< Discrete states at Time
   type(WD_MiscVarType),         intent(inout)   :: m           !< Misc/optimization variables
   INTEGER(IntKi),               intent(  out)   :: errStat     !< Error status of the operation
   CHARACTER(*),                 intent(  out)   :: errMsg      !< Error message if errStat /= ErrID_None
   character(*), parameter                       :: RoutineName = 'InitStatesWithInputs'
   integer(IntKi) :: i
   integer(intKi)                               :: ErrStat2
   character(ErrMsgLen)                         :: ErrMsg2
   real(ReKi)     :: correction(3)
   ! Note, all of these states will have been set to zero in the WD_Init routine
   
     
   ErrStat = ErrID_None
   ErrMsg = ""
   
   
   correction = 0.0_ReKi
   do i = 0, 1
      xd%x_plane     (i)   = u%Vx_rel_disk*real(i,ReKi)*real(p%DT_low,ReKi)
      xd%YawErr_filt (i)   = u%YawErr
      xd%psi_skew_filt     = u%psi_skew
      xd%chi_skew_filt     = u%chi_skew
      
      correction = correction + GetYawCorrection(u%YawErr, u%xhat_disk, xd%x_plane(i), p,  errStat2, errMsg2)
      call SetErrStat(errStat2, errMsg2, errStat, errMsg, RoutineName)   
      if (errStat >= AbortErrLev) then
         ! TEST: E3      
         return
      end if
      
      !correction = ( p%C_HWkDfl_x + p%C_HWkDfl_xY*u%YawErr )*xd%x_plane(i) + correctionA
      
      xd%p_plane   (:,i)      = u%p_hub(:) + xd%x_plane(i)*u%xhat_disk(:) + correction
      xd%xhat_plane(:,i)      = u%xhat_disk(:)
      xd%V_plane_filt(:,i)    = u%V_plane(:,i)
      xd%Vx_wind_disk_filt(i) = u%Vx_wind_disk
      xd%TI_amb_filt      (i) = u%TI_amb
      xd%D_rotor_filt     (i) = u%D_rotor
     
      
   end do
   
   xd%Vx_rel_disk_filt     = u%Vx_rel_disk    
   
      ! Initialze Ct_azavg_filt and Vx_wake; Vr_wake is already initialized to zero, so, we don't need to do that here.
   xd%Ct_azavg_filt (:) = u%Ct_azavg(:) 
   
   call NearWakeCorrection( xd%Ct_azavg_filt, xd%Vx_rel_disk_filt, p, m, xd%Vx_wake(:,0), xd%D_rotor_filt(0), errStat, errMsg )
   xd%Vx_wake(:,1) = xd%Vx_wake(:,0)
      
end subroutine InitStatesWithInputs
   
!----------------------------------------------------------------------------------------------------------------------------------
!> This routine validates the inputs from the WakeDynamics input files.
SUBROUTINE ValidateInitInputData( DT_low, InitInp, InputFileData, errStat, errMsg )
!..................................................................................................................................
      
      ! Passed variables:
   real(DbKi),               intent(in   )  :: DT_low                            !< requested simulation time step size (s)
   type(WD_InitInputType),   intent(in   )  :: InitInp                           !< Input data for initialization routine
   type(WD_InputFileType),   intent(in)     :: InputFileData                     !< All the data in the WakeDynamics input file
   integer(IntKi),           intent(out)    :: errStat                           !< Error status
   character(*),             intent(out)    :: errMsg                            !< Error message

   
      ! local variables
   integer(IntKi)                           :: k                                 ! Blade number
   integer(IntKi)                           :: j                                 ! node number
   character(*), parameter                  :: RoutineName = 'ValidateInitInputData'
   
   errStat = ErrID_None
   errMsg  = ""
   
   
   ! TODO: Talk to Bonnie about whether we want to convert <= or >= checks to EqualRealNos() .or. >  checks, etc.  GJH
   ! TEST: E13,
   !if (NumBl > MaxBl .or. NumBl < 1) call SetErrStat( ErrID_Fatal, 'Number of blades must be between 1 and '//trim(num2lstr(MaxBl))//'.', ErrSTat, errMsg, RoutineName )
   if (  DT_low                    <=  0.0)  call SetErrStat ( ErrID_Fatal, 'DT_low must be greater than zero.', errStat, errMsg, RoutineName )  
   if (  InputFileData%NumPlanes   <   2  )  call SetErrStat ( ErrID_Fatal, 'Number of wake planes must be greater than one.', ErrSTat, errMsg, RoutineName )
   if (  InputFileData%NumRadii    <   2  )  call SetErrStat ( ErrID_Fatal, 'Number of radii in the radial finite-difference grid must be greater than one.', ErrSTat, errMsg, RoutineName )
   if (  InputFileData%dr          <=  0.0)  call SetErrStat ( ErrID_Fatal, 'dr must be greater than zero.', errStat, errMsg, RoutineName ) 
   if (  InputFileData%f_c         <=  0.0)  call SetErrStat ( ErrID_Fatal, 'f_c must be greater than or equal to zero.', errStat, errMsg, RoutineName ) 
   if ( (InputFileData%C_NearWake  <=  1.0)  .or. (InputFileData%C_NearWake  >= 2.5)) call SetErrStat ( ErrID_Fatal, 'C_NearWake must be greater than 1.0 and less than 2.5.', errStat, errMsg, RoutineName ) 
   if (  InputFileData%k_vAmb      <   0.0)  call SetErrStat ( ErrID_Fatal, 'k_vAmb must be greater than or equal to zero.', errStat, errMsg, RoutineName ) 
   if (  InputFileData%k_vShr      <   0.0)  call SetErrStat ( ErrID_Fatal, 'k_vShr must be greater than or equal to zero.', errStat, errMsg, RoutineName ) 
   if (  InputFileData%C_vAmb_DMin <   0.0)  call SetErrStat ( ErrID_Fatal, 'C_vAmb_DMin must be greater than or equal to zero.', errStat, errMsg, RoutineName ) 
   if (  InputFileData%C_vAmb_DMax <=  InputFileData%C_vAmb_DMin)  call SetErrStat ( ErrID_Fatal, 'C_vAmb_DMax must be greater than C_vAmb_DMin.', errStat, errMsg, RoutineName ) 
   if ( (InputFileData%C_vAmb_FMin <   0.0)  .or. (InputFileData%C_vAmb_FMin > 1.0) ) call SetErrStat ( ErrID_Fatal, 'C_vAmb_FMin must be greater than or equal to zero and less than or equal to 1.0.', errStat, errMsg, RoutineName ) 
   if (  InputFileData%C_vAmb_Exp  <=  0.0)  call SetErrStat ( ErrID_Fatal, 'C_vAmb_Exp must be greater than zero.', errStat, errMsg, RoutineName ) 
   if (  InputFileData%C_vShr_DMin <   0.0)  call SetErrStat ( ErrID_Fatal, 'C_vShr_DMin must be greater than or equal to zero.', errStat, errMsg, RoutineName ) 
   if (  InputFileData%C_vShr_DMax <=  InputFileData%C_vShr_DMin)  call SetErrStat ( ErrID_Fatal, 'C_vShr_DMax must be greater than C_vShr_DMin.', errStat, errMsg, RoutineName ) 
   if ( (InputFileData%C_vShr_FMin <   0.0)  .or. (InputFileData%C_vShr_FMin > 1.0) ) call SetErrStat ( ErrID_Fatal, 'C_vShr_FMin must be greater than or equal to zero and less than or equal to 1.0.', errStat, errMsg, RoutineName ) 
   if (  InputFileData%C_vShr_Exp  <=  0.0)  call SetErrStat ( ErrID_Fatal, 'C_vShr_Exp must be greater than zero.', errStat, errMsg, RoutineName ) 
   if (.not. ((InputFileData%Mod_WakeDiam == 1) .or. (InputFileData%Mod_WakeDiam == 2) .or. (InputFileData%Mod_WakeDiam == 3) .or. (InputFileData%Mod_WakeDiam == 4)) ) call SetErrStat ( ErrID_Fatal, 'Mod_WakeDiam must be equal to 1, 2, 3, or 4.', errStat, errMsg, RoutineName ) 
   if ( (.not. (InputFileData%Mod_WakeDiam == 1)) .and.( (InputFileData%C_WakeDiam <=   0.0)  .or. (InputFileData%C_WakeDiam >= 1.0)) ) call SetErrStat ( ErrID_Fatal, 'When Mod_WakeDiam is not equal to 1, then C_WakeDiam must be greater than zero and less than 1.0.', errStat, errMsg, RoutineName ) 
   
END SUBROUTINE ValidateInitInputData

end module WakeDynamics