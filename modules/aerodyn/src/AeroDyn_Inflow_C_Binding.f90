!**********************************************************************************************************************************
! LICENSING
! Copyright (C) 2021 National Renewable Energy Lab
!
! This file is part of AeroDyn.
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
MODULE AeroDyn_Inflow_C_BINDING

   USE ISO_C_BINDING
   USE AeroDyn_Inflow
   USE AeroDyn_Inflow_Types
   USE NWTC_Library
 
   IMPLICIT NONE

   PUBLIC :: AeroDyn_Inflow_C_Init
   !PUBLIC :: AeroDyn_Inflow_C_ReInit
   PUBLIC :: AeroDyn_Inflow_C_CalcOutput
   PUBLIC :: AeroDyn_Inflow_C_UpdateStates
   PUBLIC :: AeroDyn_Inflow_C_End

   !------------------------------------------------------------------------------------
   !  Debugging: debugverbose
   !     0  - none
   !     1  - some summary info
   !     2  - above + all position/orientation info
   !     3  - above + input files (if direct passed)
   !     4  - above + meshes
   integer(IntKi),   parameter            :: debugverbose = 0

   !------------------------------------------------------------------------------------
   !  Error handling
   !     This must exactly match the value in the python-lib. If ErrMsgLen changes at
   !     some point in the nwtc-library, this should be updated, but the logic exists
   !     to correctly handle different lengths of the strings
   integer(IntKi),   parameter            :: ErrMsgLen_C = 1025
   integer(IntKi),   parameter            :: IntfStrLen  = 1025       ! length of other strings through the C interface

   !------------------------------------------------------------------------------------
   !  Potential issues
   !     -  if MaxADIOutputs is sufficiently large, we may overrun the buffer on the Python
   !        side (OutputChannelNames_C,OutputChannelUnits_C).  Don't have a good method to
   !        check this in code yet.  Might be best to pass the max length over to Init and
   !        do some checks here.  May also want to convert this to C_NULL_CHAR delimiter
   !        instead of fixed width.
   !     -  NOTE: AD:  MaxOutputs = 1291
   !              IfW: MaxOutputs = 59
   integer(IntKi),   parameter            :: MaxADIOutputs = 8000

   !------------------------------------------------------------------------------------
   !  Data storage
   !     All AeroDyn data is stored within the following data structures inside this
   !     module.  No data is stored within AeroDyn itself, but is instead passed in
   !     from this module.  This data is not available to the calling code unless
   !     explicitly passed through the interface (derived types such as these are
   !     non-trivial to pass through the c-bindings).
   !------------------------------
   !  Extrapolation and interpolation
   !     For the solver in AD, previous timesteps input must be stored for extrapolation
   !     to the t+dt timestep.  This can be either linear (1) quadratic (2).  The
   !     InterpOrder variable tracks what this is and sets the size of the inputs `u`
   !     passed into AD. Inputs `u` will be sized as follows:
   !        linear    interp     u(2)  with inputs at T,T-dt
   !        quadratic interp     u(3)  with inputs at T,T-dt,T-2*dt
   integer(IntKi)                         :: InterpOrder
   !------------------------------
   !  Primary AD derived data types
   type(ADI_InputType),  allocatable :: u(:)              !< Inputs at T, T-dt, T-2*dt (history kept for updating states)
   type(ADI_InitInputType)           :: InitInp           !< Initialization data
   type(ADI_InitOutputType)          :: InitOutData       !< Initial output data -- Names, units, and version info.
   type(ADI_ParameterType)           :: p                 !< Parameters
   type(ADI_ContinuousStateType)     :: x(0:2)            !< continuous states at Time t and t+dt (predicted)
   type(ADI_DiscreteStateType)       :: xd(0:2)           !< discrete states   at Time t and t+dt (predicted)
   type(ADI_ConstraintStateType)     :: z(0:2)            !< Constraint states at Time t and t+dt (predicted)
   type(ADI_OtherStateType)          :: OtherStates(0:2)  !< Initial other/optimization states
   type(ADI_OutputType)              :: y                 !< Initial output (outputs are not calculated; only the output mesh is initialized)
   type(ADI_MiscVarType)             :: m                 !< Misc variables for optimization (not copied in glue code)
   !------------------------------
   !  Time tracking
   !     When we are performing a correction step, time information of previous
   !     calls is needed to decide how to apply correction logic or cycle the inputs
   !     and resave the previous timestep states.
   !  Correction steps
   !     OpenFAST has the ability to perform correction steps.  During a correction
   !     step, new input values are passed in but the timestep remains the same.
   !     When this occurs the new input data at time t is used with the state
   !     information from the previous timestep (t) to calculate new state values
   !     time t+dt in the UpdateStates routine.  In OpenFAST this is all handled by
   !     the glue code.  However, here we do not pass state information through the
   !     interface and therefore must store it here analogously to how it is handled
   !     in the OpenFAST glue code.
   real(DbKi)                             :: dT_Global         ! dT of the code calling this module
   integer(IntKi)                         :: N_Global          ! global timestep
   real(DbKi)                             :: T_Initial         ! initial Time of simulation
   real(DbKi)                             :: TMax              ! initial Time of simulation
   real(DbKi),       allocatable          :: InputTimes(:)     ! input times corresponding to u(:) array
   real(DbKi)                             :: InputTimePrev     ! input time of last UpdateStates call
   ! Note that we are including the previous state info here (not done in OF this way)
   integer(IntKi),   parameter            :: STATE_LAST = 0    ! Index for previous state (not needed in OF, but necessary here)
   integer(IntKi),   parameter            :: STATE_CURR = 1    ! Index for current state
   integer(IntKi),   parameter            :: STATE_PRED = 2    ! Index for predicted state
   ! Note the indexing is different on inputs (no clue why, but thats how OF handles it)
   integer(IntKi),   parameter            :: INPUT_LAST = 3    ! Index for previous  input at t-dt
   integer(IntKi),   parameter            :: INPUT_CURR = 2    ! Index for current   input at t
   integer(IntKi),   parameter            :: INPUT_PRED = 1    ! Index for predicted input at t+dt
   !------------------------------------------------------------------------------------



   !------------------------------------------------------------------------------------
   !  Meshes for motions and loads
   !     Meshes are used within AD to handle all motions and loads. Rather than directly
   !     map to those nodes, we will create a mapping to go between the array of node
   !     positions passed into this module and what is used inside AD.  This is done
   !     through a pair of meshes for the motion and loads corresponding to the node
   !     positions passed in.
   !------------------------------
   !  Meshes for external nodes
   !     These point meshes are merely used to simplify the mapping of motions/loads
   !     to/from AD using the library mesh mapping routines.  These meshes may contain
   !     one or multiple points.
   !        - 1 point   -- rigid floating body assumption
   !        - N points  -- flexible structure (either floating or fixed bottom)
   integer(IntKi)                         :: NumTurbines = 1         ! Number of turbines (only one at present) 
   integer(IntKi)                         :: NumBlades               ! Number of blades (only one rotor allowed at present)
   integer(IntKi)                         :: NumMeshPts              ! Number of mesh points we are interfacing motions/loads to/from AD
   type(MeshType)                         :: BldPtMotionMesh         ! mesh for motions of external nodes
   type(MeshType)                         :: BldPtLoadMesh           ! mesh for loads  for external nodes
   type(MeshType)                         :: BldPtLoadMesh_tmp       ! mesh for loads  for external nodes -- temporary
!   type(MeshType)                         :: NacMotionMesh           ! mesh for motion  of nacelle -- TODO: add this mesh for nacelle load transfers
!   type(MeshType)                         :: NacLoadMesh             ! mesh for loads  for nacelle loads -- TODO: add this mesh for nacelle load transfers
   integer(IntKi)                         :: WrVTK                   !< Write VTK outputs [0: none, 1: init only, 2: animation]
   integer(IntKi)                         :: WrVTK_Type              !< Write VTK outputs as [1: surface, 2: lines, 3: both]
   real(SiKi)                             :: VTKNacDim(6)            !< Nacelle dimension passed in for VTK surface rendering [0,y0,z0,Lx,Ly,Lz] (m)
   real(SiKi)                             :: VTKHubrad               !< Hub radius for VTK surface rendering
   real(SiKi)                             :: VTK_RefPos(3) = (/ 0.0_SiKi, 0.0_SiKi, 0.0_SiKi /)    !TODO: should this be an input?
   integer(IntKi)                         :: VTK_tWidth              !< width of the time field in the VTK
   character(IntfStrLen)                  :: VTK_OutFileRoot         !< Root name to use for echo files, vtk, etc.
   logical                                :: TransposeDCM            !< Transpose DCMs as passed in -- test the vtk outputs to see if needed
   !------------------------------
   !  Mesh mapping: motions
   !     The mapping of motions from the nodes passed in to the corresponding AD meshes
   type(MeshMapType),   allocatable       :: Map_BldPtMotion_2_AD_Blade(:)    ! Mesh mapping between input motion mesh for blade
!   type(MeshMapType)                      :: Map_AD_Nac_2_NacPtLoad           ! Mesh mapping between input motion mesh for nacelle 
   !------------------------------
   !  Mesh mapping: loads
   !     The mapping of loads from the AD meshes to the corresponding external nodes
   type(MeshMapType),   allocatable       :: Map_AD_BldLoad_P_2_BldPtLoad(:)  ! Mesh mapping between AD output blade line2 load to BldPtLoad for return
!   type(MeshMapType)                      :: Map_NacPtMotion_2_AD_Nac         ! Mesh mapping between AD output nacelle pt  load to NacLoad   for return
   !  Motions input (so we don't have to reallocate all the time
   real(ReKi), allocatable                :: tmpBldPtMeshPos(:,:)       ! temp array.  Probably don't need this, but makes conversion from C clearer.
   real(ReKi), allocatable                :: tmpBldPtMeshOri(:,:,:)     ! temp array.  Probably don't need this, but makes conversion from C clearer.
   real(ReKi), allocatable                :: tmpBldPtMeshVel(:,:)       ! temp array.  Probably don't need this, but makes conversion from C clearer.
   real(ReKi), allocatable                :: tmpBldPtMeshAcc(:,:)       ! temp array.  Probably don't need this, but makes conversion from C clearer.
   real(ReKi), allocatable                :: tmpBldPtMeshFrc(:,:)       ! temp array.  Probably don't need this, but makes conversion to   C clearer.
   !------------------------------------------------------------------------------------

   !------------------------------------------------------------------------------------
   !  Visualization
   !     The data stored here is a duplicate of DvrVTK_BLSurfaceType from the AeroDyn_Driver_Registry.  This
   !     is a temporary solution to get visualization up and running with ADI_C.  Ultimately this data will
   !     be combined into the ADI library, but that won't happen right now.
   ! =========  DvrVTK_SurfaceType  =======
   TYPE, PUBLIC :: DvrVTK_SurfaceType
      INTEGER(IntKi)  :: NumSectors      !< number of sectors in which to split circles (higher number gives smoother surface) [-]
      REAL(SiKi) , DIMENSION(1:3,1:8)  :: NacelleBox   !< X-Y-Z locations of 8 points that define the nacelle box, relative to the nacelle position [m]
      REAL(SiKi) , DIMENSION(1:3,1:8)  :: BaseBox      !< X-Y-Z locations of 8 points that define the base box [m]
   END TYPE DvrVTK_SurfaceType
   ! =======================
   type(DvrVTK_SurfaceType),  allocatable :: VTK_surface(:)
   !------------------------------------------------------------------------------------


CONTAINS

!> This routine sets the error status in C_CHAR for export to calling code.
!! Make absolutely certain that we do not overrun the end of ErrMsg_C.  That is hard coded to 1025,
!! but ErrMsgLen is set in the nwtc_library, and could change without updates here.  We don't want an
!! inadvertant buffer overrun -- that can lead to bad things.
subroutine SetErr(ErrStat, ErrMsg, ErrStat_C, ErrMsg_C)
   integer,                intent(in   )  :: ErrStat                 !< aggregated error message (fortran type)
   character(ErrMsgLen),   intent(in   )  :: ErrMsg                  !< aggregated error message (fortran type)
   integer(c_int),         intent(  out)  :: ErrStat_C
   character(kind=c_char), intent(  out)  :: ErrMsg_C(ErrMsgLen_C)
   integer                                :: i
   ErrStat_C = ErrStat     ! We will send back the same error status that is used in OpenFAST
   if (ErrMsgLen > ErrMsgLen_C-1) then   ! If ErrMsgLen is > the space in ErrMsg_C, do not copy everything over
      ErrMsg_C = TRANSFER( trim(ErrMsg(1:ErrMsgLen_C-1))//C_NULL_CHAR, ErrMsg_C )
   else
      ErrMsg_C = TRANSFER( trim(ErrMsg)//C_NULL_CHAR, ErrMsg_C )
   endif
end subroutine SetErr


!===============================================================================================================
!--------------------------------------------- AeroDyn Init----------------------------------------------------
!===============================================================================================================
SUBROUTINE AeroDyn_Inflow_C_Init( ADinputFilePassed, ADinputFileString_C, ADinputFileStringLength_C, &
               IfWinputFilePassed, IfWinputFileString_C, IfWinputFileStringLength_C, OutRootName_C,  &
               gravity_C, defFldDens_C, defKinVisc_C, defSpdSound_C,      &
               defPatm_C, defPvap_C, WtrDpth_C, MSL2SWL_C,                &
               AeroProjMod_C,                                             &
               InterpOrder_C, T_initial_C, DT_C, TMax_C,                  &
               storeHHVel, TransposeDCM_in,                               &
               WrVTK_in, WrVTK_inType, VTKNacDim_in, VTKHubRad_in,        &
               HubPos_C, HubOri_C,                                        &
               NacPos_C, NacOri_C,                                        &
               NumBlades_C, BldRootPos_C, BldRootOri_C,                   &
               NumMeshPts_C,  InitMeshPos_C,  InitMeshOri_C,              &
               NumChannels_C, OutputChannelNames_C, OutputChannelUnits_C, &
               ErrStat_C, ErrMsg_C) BIND (C, NAME='AeroDyn_Inflow_C_Init')
   implicit none
#ifndef IMPLICIT_DLLEXPORT
!DEC$ ATTRIBUTES DLLEXPORT :: AeroDyn_Inflow_C_Init
!GCC$ ATTRIBUTES DLLEXPORT :: AeroDyn_Inflow_C_Init
#endif
   ! Input file info
   logical(c_bool),           intent(in   )  :: ADinputFilePassed                      !< Write VTK outputs [0: none, 1: init only, 2: animation]
   type(c_ptr),               intent(in   )  :: ADinputFileString_C                    !< Input file as a single string with lines deliniated by C_NULL_CHAR
   integer(c_int),            intent(in   )  :: ADinputFileStringLength_C              !< lenght of the input file string
   logical(c_bool),           intent(in   )  :: IfWinputFilePassed                     !< Write VTK outputs [0: none, 1: init only, 2: animation]
   type(c_ptr),               intent(in   )  :: IfWinputFileString_C                   !< Input file as a single string with lines deliniated by C_NULL_CHAR
   integer(c_int),            intent(in   )  :: IfWinputFileStringLength_C             !< lenght of the input file string
   character(kind=c_char),    intent(in   )  :: OutRootName_C(IntfStrLen)              !< Root name to use for echo files and other
   ! Environmental
   real(c_float),             intent(in   )  :: gravity_C                              !< Gravitational acceleration (m/s^2)
   real(c_float),             intent(in   )  :: defFldDens_C                           !< Air density (kg/m^3)
   real(c_float),             intent(in   )  :: defKinVisc_C                           !< Kinematic viscosity of working fluid (m^2/s)
   real(c_float),             intent(in   )  :: defSpdSound_C                          !< Speed of sound in working fluid (m/s)
   real(c_float),             intent(in   )  :: defPatm_C                              !< Atmospheric pressure (Pa) [used only for an MHK turbine cavitation check]
   real(c_float),             intent(in   )  :: defPvap_C                              !< Vapour pressure of working fluid (Pa) [used only for an MHK turbine cavitation check]
   real(c_float),             intent(in   )  :: WtrDpth_C                              !< Water depth (m)
   real(c_float),             intent(in   )  :: MSL2SWL_C                              !< Offset between still-water level and mean sea level (m) [positive upward]
   ! Aero calculation method -- AeroProjMod
   !     APM_BEM_NoSweepPitchTwist - 1 -  "Original AeroDyn model where momentum balance is done in the WithoutSweepPitchTwist system"
   !     APM_BEM_Polar             - 2 -  "Use staggered polar grid for momentum balance in each annulus"
   !     APM_LiftingLine           - 3 -  "Use the blade lifting line (i.e. the structural) orientation (currently for OLAF with VAWT)"
   integer(c_int),            intent(in   )  :: AeroProjMod_C                          !< Type of aerodynamic projection
   ! Initial hub and blade root positions/orientations
   real(c_float),             intent(in   )  :: HubPos_C( 3 )                          !< Hub position
   real(c_double),            intent(in   )  :: HubOri_C( 9 )                          !< Hub orientation
   real(c_float),             intent(in   )  :: NacPos_C( 3 )                          !< Nacelle position
   real(c_double),            intent(in   )  :: NacOri_C( 9 )                          !< Nacelle orientation
   integer(c_int),            intent(in   )  :: NumBlades_C                            !< Number of blades
   real(c_float),             intent(in   )  :: BldRootPos_C( 3*NumBlades_C )          !< Blade root positions
   real(c_double),            intent(in   )  :: BldRootOri_C( 9*NumBlades_C )          !< Blade root orientations
   ! Initial nodes
   integer(c_int),            intent(in   )  :: NumMeshPts_C                           !< Number of mesh points we are transfering motions to and output loads to
   real(c_float),             intent(in   )  :: InitMeshPos_C( 3*NumMeshPts_C )        !< A 3xNumMeshPts_C array [x,y,z]
   real(c_double),            intent(in   )  :: InitMeshOri_C( 9*NumMeshPts_C )        !< A 9xNumMeshPts_C array [r11,r12,r13,r21,r22,r23,r31,r32,r33]
   ! Interpolation
   integer(c_int),            intent(in   )  :: InterpOrder_C                          !< Interpolation order to use (must be 1 or 2)
   ! Time
   real(c_double),            intent(in   )  :: T_initial_C
   real(c_double),            intent(in   )  :: DT_C                                   !< Timestep used with AD for stepping forward from t to t+dt.  Must be constant.
   real(c_double),            intent(in   )  :: TMax_C                                 !< Maximum time for simulation
   ! Flags
   logical(c_bool),           intent(in   )  :: storeHHVel                             !< Store hub height time series from IfW
   logical(c_bool),           intent(in   )  :: TransposeDCM_in                        !< Transpose DCMs as they are passed in
   ! VTK
   integer(c_int),            intent(in   )  :: WrVTK_in                               !< Write VTK outputs [0: none, 1: init only, 2: animation]
   integer(c_int),            intent(in   )  :: WrVTK_inType                           !< Write VTK outputs as [1: surface, 2: lines, 3: both]
   real(c_float),             intent(in   )  :: VTKNacDim_in(6)                        !< Nacelle dimension passed in for VTK surface rendering [0,y0,z0,Lx,Ly,Lz] (m)
   real(c_float),             intent(in   )  :: VTKHubrad_in                           !< Hub radius for VTK surface rendering
   ! Output
   integer(c_int),            intent(  out)  :: NumChannels_C                          !< Number of output channels requested from the input file
   character(kind=c_char),    intent(  out)  :: OutputChannelNames_C(ChanLen*MaxADIOutputs+1)    !< NOTE: if MaxADIOutputs is sufficiently large, we may overrun the buffer on the Python side.
   character(kind=c_char),    intent(  out)  :: OutputChannelUnits_C(ChanLen*MaxADIOutputs+1)
   integer(c_int),            intent(  out)  :: ErrStat_C                              !< Error status
   character(kind=c_char),    intent(  out)  :: ErrMsg_C(ErrMsgLen_C)                  !< Error message (C_NULL_CHAR terminated)

   ! Local Variable4
   character(IntfStrLen)                                          :: OutRootName       !< Root name to use for echo files and other
   character(IntfStrLen)                                          :: TmpFileName       !< Temporary file name if not passing AD or IfW input file contents directly
   character(kind=C_char, len=ADinputFileStringLength_C), pointer :: ADinputFileString !< Input file as a single string with NULL chracter separating lines
   character(kind=C_char, len=IfWinputFileStringLength_C), pointer:: IfWinputFileString !< Input file as a single string with NULL chracter separating lines

   integer(IntKi)                                                 :: ErrStat           !< aggregated error message
   character(ErrMsgLen)                                           :: ErrMsg            !< aggregated error message
   integer(IntKi)                                                 :: ErrStat2          !< temporary error status  from a call
   character(ErrMsgLen)                                           :: ErrMsg2           !< temporary error message from a call
   integer(IntKi)                                                 :: i,j,k             !< generic counters
   character(*), parameter                                        :: RoutineName = 'AeroDyn_Inflow_C_Init'  !< for error handling

   ! Initialize error handling
   ErrStat  =  ErrID_None
   ErrMsg   =  ""
   NumChannels_C = 0_c_int
   OutputChannelNames_C(:) = ''
   OutputChannelUnits_C(:) = ''


   !--------------------------
   ! Input files
   !--------------------------
   ! RootName -- for output of echo or other files
   OutRootName = TRANSFER( OutRootName_C, OutRootName )
   i = INDEX(OutRootName,C_NULL_CHAR) - 1             ! if this has a c null character at the end...
   if ( i > 0 ) OutRootName = OutRootName(1:I)        ! remove it


   ! For debugging the interface:
   if (debugverbose > 0) then
      call ShowPassedData()
   endif


   ! Get fortran pointer to C_NULL_CHAR deliniated input files as a string
   call C_F_pointer(ADinputFileString_C,  ADinputFileString)
   call C_F_pointer(IfWinputFileString_C, IfWinputFileString)

   ! Format AD input file contents
   InitInp%AD%RootName                 = OutRootName
   if (ADinputFilePassed) then
      InitInp%AD%UsePrimaryInputFile   = .FALSE.            ! Don't try to read an input -- use passed data instead (blades and AF tables not passed)
      InitInp%AD%InputFile             = "passed_ad_file"   ! not actually used
      call InitFileInfo(ADinputFileString, InitInp%AD%PassedPrimaryInputData, ErrStat2, ErrMsg2); if (Failed())  return
   else
      InitInp%AD%UsePrimaryInputFile   = .TRUE.             ! Read input info from a primary input file
      i = min(IntfStrLen,ADinputFileStringLength_C)
      TmpFileName = ''
      TmpFileName(1:i) = ADinputFileString(1:i)
      i = INDEX(TmpFileName,C_NULL_CHAR) - 1                ! if this has a c null character at the end...
      if ( i > 0 ) TmpFileName = TmpFileName(1:I)           ! remove it
      InitInp%AD%InputFile             = TmpFileName
   endif

   ! Format IfW input file contents
   !     RootName is set in ADI_Init using AD%RootName
   if (IfWinputFilePassed) then
      InitInp%IW_InitInp%UseInputFile   = .FALSE.           ! Don't try to read an input -- use passed data instead (blades and AF tables not passed)
      InitInp%IW_InitInp%InputFile      = "passed_ifw_file" ! not actually used
      call InitFileInfo(IfWinputFileString, InitInp%IW_InitInp%PassedFileData, ErrStat2, ErrMsg2); if (Failed())  return
   else
      InitInp%IW_InitINp%UseInputFile   = .TRUE.            ! Read input info from a primary input file
      i = min(IntfStrLen,IfWinputFileStringLength_C)
      TmpFileName = ''
      TmpFileName(1:i) = IfWinputFileString(1:i)
      i = INDEX(TmpFileName,C_NULL_CHAR) - 1                ! if this has a c null character at the end...
      if ( i > 0 ) TmpFileName = TmpFileName(1:I)           ! remove it
      InitInp%IW_InitInp%InputFile      = TmpFileName
   endif


   ! For diagnostic purposes, the following can be used to display the contents
   ! of the InFileInfo data structure.
   !     CU is the screen -- system dependent.
   if (debugverbose >= 3) then
      if (ADinputFilePassed)     call Print_FileInfo_Struct( CU, InitInp%AD%PassedPrimaryInputData )
      if (IfWinputFilePassed)    call Print_FileInfo_Struct( CU, InitInp%IW_InitInp%PassedFileData )
   endif


   ! Store some data at library level
   NumBlades              = int(NumBlades_C,   IntKi)
   ! Timekeeping
   dT_Global              = REAL(DT_C,          DbKi)
   N_Global               = 0_IntKi                     ! Assume we are on timestep 0 at start
   t_initial              = REAL(T_Initial_C,   DbKi)
   TMax                   = REAL(TMax_C,        DbKi)
   ! Interpolation order
   InterpOrder            = int(InterpOrder_C, IntKi)
   if ( InterpOrder < 1_IntKi .or. InterpOrder > 2_IntKi ) then
      ErrStat2 =  ErrID_Fatal
      ErrMsg2  =  "InterpOrder passed into AeroDyn_Inflow_C_Init must be 1 (linear) or 2 (quadratic)"
      if (Failed())  return
   endif
   ! VTK outputs
   WrVTK       = int(WrVTK_in,     IntKi)
   WrVTK_Type  = int(WrVTK_inType, IntKi)
   VTKNacDim   = real(VTKNacDim_in, SiKi)
   VTKHubrad   = real(VTKHubrad_in, SiKi)
   if ( WrVTK < 0_IntKi .or. WrVTK > 2_IntKi ) then
      ErrStat2 =  ErrID_Fatal
      ErrMsg2  =  "WrVTK option for writing VTK visualization files must be [0: none, 1: init only, 2: animation]"
      if (Failed())  return
   endif
   if ( WrVTK_Type > 0_IntKi ) then
      if ( WrVTK_Type < 1_IntKi .or. WrVTK_Type > 3_IntKi ) then
         ErrStat2 =  ErrID_Fatal
         ErrMsg2  =  "WrVTK_Type option for writing VTK visualization files must be [1: surface, 2: lines, 3: both]"
         if (Failed())  return
      endif
      if (VTKHubRad < 0.0_SiKi) then
         ErrStat2 =  ErrID_Warn
         ErrMsg2  =  "VTKHubRad for surface visualization of hub less than zero.  Setting to zero."
         if (Failed())  return
         VTKHubRad = 0.0_SiKi
      endif
   endif
   ! Flag to transpose DCMs as they are passed in
   TransposeDCM      = TransposeDCM_in


   ! Linearization
   !     for now, set linearization to false. Pass this in later when interface supports it
   InitInp%AD%Linearize          = .FALSE.
   !InitInp%IW_InitInp%Linearize  = .FALSE.


   ! AeroDyn values passed in through interface
   InitInp%AD%Gravity     = REAL(gravity_C,     ReKi)
   InitInp%AD%defFldDens  = REAL(defFldDens_C,  ReKi)
   InitInp%AD%defKinVisc  = REAL(defKinVisc_C,  ReKi)
   InitInp%AD%defSpdSound = REAL(defSpdSound_C, ReKi)
   InitInp%AD%defPatm     = REAL(defPatm_C,     ReKi)
   InitInp%AD%defPvap     = REAL(defPvap_C,     ReKi)
   InitInp%AD%WtrDpth     = REAL(WtrDpth_C,     ReKi)
   InitInp%AD%MSL2SWL     = REAL(MSL2SWL_C,     ReKi)
   InitInp%storeHHVel     = storeHHVel
   InitInp%WrVTK          = WrVTK
   InitInp%WrVTK_Type     = WrVTK_Type
   InitInp%IW_InitInp%CompInflow = 1    ! Use InflowWind

   ! setup rotors for AD -- interface only supports one rotor at present
   allocate (InitInp%AD%rotors(1),stat=errStat2)
   if (errStat/=0) then
      ErrStat2 = ErrID_Fatal
      ErrMsg2  = 'Allocating rotors'
      if (Failed())  return
   end if
   InitInp%AD%rotors(1)%AeroProjMod = int(AeroProjMod_C, IntKi)
   InitInp%AD%rotors(1)%numBlades = NumBlades
   call AllocAry(InitInp%AD%rotors(1)%BladeRootPosition,       3, NumBlades_c, 'BldRootPos', errStat2, errMsg2 ); if (Failed()) return
   call AllocAry(InitInp%AD%rotors(1)%BladeRootOrientation, 3, 3, NumBlades_c, 'BldRootOri', errStat2, errMsg2 ); if (Failed()) return
   InitInp%AD%rotors(1)%HubPosition          = real(HubPos_C(1:3),ReKi)
   InitInp%AD%rotors(1)%HubOrientation       = reshape( real(HubOri_C(1:9),R8Ki), (/3,3/) )
   InitInp%AD%rotors(1)%NacellePosition      = real(NacPos_C(1:3),ReKi)
   InitInp%AD%rotors(1)%NacelleOrientation   = reshape( real(NacOri_C(1:9),R8Ki), (/3,3/) )
   InitInp%AD%rotors(1)%BladeRootPosition    = reshape( real(BldRootPos_C(1:3*NumBlades_c),ReKi), (/  3,NumBlades_c/) )
   InitInp%AD%rotors(1)%BladeRootOrientation = reshape( real(BldRootOri_C(1:9*NumBlades_c),R8Ki), (/3,3,NumBlades_c/) )
   if (TransposeDCM) then
      InitInp%AD%rotors(1)%HubOrientation       = transpose(InitInp%AD%rotors(1)%HubOrientation)
      InitInp%AD%rotors(1)%NacelleOrientation   = transpose(InitInp%AD%rotors(1)%NacelleOrientation)
      do i=1,NumBlades
         InitInp%AD%rotors(1)%BladeRootOrientation(1:3,1:3,i) = transpose(InitInp%AD%rotors(1)%BladeRootOrientation(1:3,1:3,i))
      enddo
   endif

   ! Remap the orientation DCM just in case there is some issue with passed 
   call OrientRemap(InitInp%AD%rotors(1)%HubOrientation)
   call OrientRemap(InitInp%AD%rotors(1)%NacelleOrientation)
   do i=1,NumBlades
      call OrientRemap(InitInp%AD%rotors(1)%BladeRootOrientation(1:3,1:3,i))
   enddo


   ! Number of blades and initial positions
   !  -  NumMeshPts is the number of interface Mesh points we are expecting on the python
   !     side.  Will validate this against what AD reads from the initialization info.
   NumMeshPts                    = int(NumMeshPts_C, IntKi)
   if (NumMeshPts < 1) then
      ErrStat2 =  ErrID_Fatal
      ErrMsg2  =  "At least one node point must be specified"
      if (Failed())  return
   endif
   ! Allocate temporary arrays to simplify data conversions
   call AllocAry( tmpBldPtMeshPos,    3, NumMeshPts, "tmpBldPtMeshPos", ErrStat2, ErrMsg2 );    if (Failed())  return
   call AllocAry( tmpBldPtMeshOri, 3, 3, NumMeshPts, "tmpBldPtMeshOri", ErrStat2, ErrMsg2 );    if (Failed())  return
   call AllocAry( tmpBldPtMeshVel,    6, NumMeshPts, "tmpBldPtMeshVel", ErrStat2, ErrMsg2 );    if (Failed())  return
   call AllocAry( tmpBldPtMeshAcc,    6, NumMeshPts, "tmpBldPtMeshAcc", ErrStat2, ErrMsg2 );    if (Failed())  return
   call AllocAry( tmpBldPtMeshFrc,    6, NumMeshPts, "tmpBldPtMeshFrc", ErrStat2, ErrMsg2 );    if (Failed())  return
   tmpBldPtMeshPos(    1:3,1:NumMeshPts) = reshape( real(InitMeshPos_C(1:3*NumMeshPts),ReKi), (/  3,NumMeshPts/) )
   tmpBldPtMeshOri(1:3,1:3,1:NumMeshPts) = reshape( real(InitMeshOri_C(1:9*NumMeshPts),ReKi), (/3,3,NumMeshPts/) )


   !----------------------------------------------------
   ! Allocate input array u and corresponding InputTimes
   !----------------------------------------------------
   !     These inputs are used in the time stepping algorithm within AD_UpdateStates
   !     For quadratic interpolation (InterpOrder==2), 3 timesteps are used.  For
   !     linear (InterOrder==1), 2 timesteps (the AD code can handle either).
   !        u(1)  inputs at t
   !        u(2)  inputs at t -   dt
   !        u(3)  inputs at t - 2*dt      ! quadratic only
   allocate(u(InterpOrder+1), STAT=ErrStat2)
      if (ErrStat2 /= 0) then
         ErrStat2 = ErrID_Fatal
         ErrMsg2  = "Could not allocate inuput"
         if (Failed())  return
      endif
   call AllocAry( InputTimes, InterpOrder+1, "InputTimes", ErrStat2, ErrMsg2 );  if (Failed())  return


   ! Call the main subroutine AeroDyn_Inflow_Init
   !     dT_Global and InitInp are passed into AD_Init, all the rest are set by AD_Init
   !
   !     NOTE: Pass u(1) only (this is empty and will be set inside Init).  We will copy
   !           this to u(2) and u(3) afterwards
   call ADI_Init( InitInp, u(1), p, x(STATE_CURR), xd(STATE_CURR), z(STATE_CURR), OtherStates(STATE_CURR), y, m, dT_Global, InitOutData, ErrStat2, ErrMsg2 )
      if (Failed())  return


   !-------------------------------------------------------------
   ! Sanity checks
   !-------------------------------------------------------------
   call CheckNodes(ErrStat2,ErrMsg2);     if (Failed())  return


   !-------------------------------------------------------------
   ! Set the interface  meshes for motion inputs and loads output
   !-------------------------------------------------------------
   call SetMotionLoadsInterfaceMeshes(ErrStat2,ErrMsg2);    if (Failed())  return
   if (WrVTK > 0_IntKi) then
      call SetVTKParameters(OutRootName,u(1)%AD%rotors(:),VTK_RefPos,ErrStat2,ErrMsg2)
      if (Failed())  return
      call WrVTK_refMeshes(u(1)%AD%rotors(:),VTK_RefPos,ErrStat2,ErrMsg2)
      if (Failed())  return
   endif

   !-------------------------------------------------------------
   ! Setup other prior timesteps
   !     We fill InputTimes with negative times, but the Input values are identical for each of those times; this allows
   !     us to use, e.g., quadratic interpolation that effectively acts as a zeroth-order extrapolation and first-order extrapolation
   !     for the first and second time steps.  (The interpolation order in the ExtrapInput routines are determined as
   !     order = SIZE(Input)
   !-------------------------------------------------------------
   do i=2,InterpOrder+1
      call ADI_CopyInput (u(1),  u(i),  MESH_NEWCOPY, Errstat2, ErrMsg2)
         if (Failed())  return
   enddo
   do i = 1, InterpOrder + 1
      InputTimes(i) = t_initial - (i - 1) * dT_Global
   enddo
   InputTimePrev = InputTimes(1) - dT_Global    ! Initialize for UpdateStates


   !-------------------------------------------------------------
   ! Initial setup of other pieces of x,xd,z,OtherStates
   !-------------------------------------------------------------
   CALL ADI_CopyContState  ( x(          STATE_CURR), x(          STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2);    if (Failed())  return
   CALL ADI_CopyDiscState  ( xd(         STATE_CURR), xd(         STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2);    if (Failed())  return
   CALL ADI_CopyConstrState( z(          STATE_CURR), z(          STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2);    if (Failed())  return
   CALL ADI_CopyOtherState ( OtherStates(STATE_CURR), OtherStates(STATE_PRED), MESH_NEWCOPY, Errstat2, ErrMsg2);    if (Failed())  return

   !-------------------------------------------------------------
   ! Setup the previous timestep copies of states
   !-------------------------------------------------------------
   CALL ADI_CopyContState  ( x(          STATE_CURR), x(          STATE_LAST), MESH_NEWCOPY, Errstat2, ErrMsg2);    if (Failed())  return
   CALL ADI_CopyDiscState  ( xd(         STATE_CURR), xd(         STATE_LAST), MESH_NEWCOPY, Errstat2, ErrMsg2);    if (Failed())  return
   CALL ADI_CopyConstrState( z(          STATE_CURR), z(          STATE_LAST), MESH_NEWCOPY, Errstat2, ErrMsg2);    if (Failed())  return
   CALL ADI_CopyOtherState ( OtherStates(STATE_CURR), OtherStates(STATE_LAST), MESH_NEWCOPY, Errstat2, ErrMsg2);    if (Failed())  return


   !TODO: Is there any other InitOutData should be returned?


   !-------------------------------------------------
   !  Set output channel information for driver code
   !-------------------------------------------------

   ! Number of channels
   NumChannels_C = size(InitOutData%WriteOutputHdr)

   ! transfer the output channel names and units to c_char arrays for returning
   !     Upgrade idea:  use C_NULL_CHAR as delimiters.  Requires rework of Python
   !                    side of code.
   k=1
   do i=1,NumChannels_C
      do j=1,ChanLen    ! max length of channel name.  Same for units
         OutputChannelNames_C(k)=InitOutData%WriteOutputHdr(i)(j:j)
         OutputChannelUnits_C(k)=InitOutData%WriteOutputUnt(i)(j:j)
         k=k+1
      enddo
   enddo

   ! null terminate the string
   OutputChannelNames_C(k) = C_NULL_CHAR
   OutputChannelUnits_C(k) = C_NULL_CHAR


   call SetErr(ErrStat,ErrMsg,ErrStat_C,ErrMsg_C)


CONTAINS
   logical function Failed()
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      Failed = ErrStat >= AbortErrLev
      if (Failed) then
         call FailCleanup()
         call SetErr(ErrStat,ErrMsg,ErrStat_C,ErrMsg_C)
      endif
   end function Failed

   subroutine FailCleanup()
      if (allocated(tmpBldPtMeshPos))  deallocate(tmpBldPtMeshPos)
      if (allocated(tmpBldPtMeshOri))  deallocate(tmpBldPtMeshOri)
      if (allocated(tmpBldPtMeshVel))  deallocate(tmpBldPtMeshVel)
      if (allocated(tmpBldPtMeshAcc))  deallocate(tmpBldPtMeshAcc)
      if (allocated(tmpBldPtMeshFrc))  deallocate(tmpBldPtMeshFrc)
   end subroutine FailCleanup

   !> This subroutine prints out all the variables that are passed in.  Use this only
   !! for debugging the interface on the Fortran side.
   subroutine ShowPassedData()
      character(1) :: TmpFlag
      integer      :: i,j
      call WrScr("Interface debugging:  Variables passed in through interface")
      call WrScr("-----------------------------------------------------------")
      call WrScr("   FileInfo")
      TmpFlag="F";   if (ADinputFilePassed) TmpFlag="T"
      call WrScr("       ADinputFilePassed_C            "//TmpFlag )
      call WrScr("       ADinputFileString_C (ptr addr) "//trim(Num2LStr(LOC(ADinputFileString_C))) )
      call WrScr("       ADinputFileStringLength_C      "//trim(Num2LStr( ADinputFileStringLength_C )) )
      TmpFlag="F";   if (IfWinputFilePassed) TmpFlag="T"
      call WrScr("       IfWinputFilePassed_C           "//TmpFlag )
      call WrScr("       IfWinputFileString_C (ptr addr)"//trim(Num2LStr(LOC(IfWinputFileString_C))) )
      call WrScr("       IfWinputFileStringLength_C     "//trim(Num2LStr( IfWinputFileStringLength_C )) )
      call WrScr("       OutRootName                    "//trim(OutRootName) )
      call WrScr("   Environment variables")
      call WrScr("       gravity_C                      "//trim(Num2LStr( gravity_C     )) )
      call WrScr("       defFldDens_C                   "//trim(Num2LStr( defFldDens_C  )) )
      call WrScr("       defKinVisc_C                   "//trim(Num2LStr( defKinVisc_C  )) )
      call WrScr("       defSpdSound_C                  "//trim(Num2LStr( defSpdSound_C )) )
      call WrScr("       defPatm_C                      "//trim(Num2LStr( defPatm_C     )) )
      call WrScr("       defPvap_C                      "//trim(Num2LStr( defPvap_C     )) )
      call WrScr("       WtrDpth_C                      "//trim(Num2LStr( WtrDpth_C     )) )
      call WrScr("       MSL2SWL_C                      "//trim(Num2LStr( MSL2SWL_C     )) )
      call WrScr("   Interpolation")
      call WrScr("       InterpOrder_C                  "//trim(Num2LStr( InterpOrder_C )) )
      call WrScr("   Time variables")
      call WrScr("       T_initial_C                    "//trim(Num2LStr( T_initial_C   )) )
      call WrScr("       DT_C                           "//trim(Num2LStr( DT_C          )) )
      call WrScr("       TMax_C                         "//trim(Num2LStr( TMax_C        )) )
      call WrScr("   Flags")
      TmpFlag="F";   if (storeHHVel) TmpFlag="T"
      call WrScr("       storeHHVel                     "//TmpFlag )
      call WrScr("       WrVTK_in                       "//trim(Num2LStr( WrVTK_in      )) )
      call WrScr("       WrVTK_inType                   "//trim(Num2LStr( WrVTK_inType  )) )
      TmpFlag="F";   if (TransposeDCM_in) TmpFlag="T"
      call WrScr("       TransposeDCM_in                "//TmpFlag )
      call WrScr("   Init Data")
      call WrNR("       Hub Position         ")
      call WrMatrix(HubPos_C,CU,'(3(ES15.7e2))')
      call WrNR("       Hub Orientation      ")
      call WrMatrix(HubOri_C,CU,'(9(ES23.15e2))')
      call WrNR("       Nacelle Position     ")
      call WrMatrix(NacPos_C,CU,'(3(ES15.7e2))')
      call WrNR("       Nacelle Orientation  ")
      call WrMatrix(NacOri_C,CU,'(9(ES23.15e2))')
      call WrScr("       NumBlades_C                    "//trim(Num2LStr( NumBlades_C   )) )
      if (debugverbose > 1) then
         call WrScr("          Root Positions")
         do i=1,NumBlades_C
            j=3*(i-1)
            call WrMatrix(BldRootPos_C(j+1:j+3),CU,'(3(ES15.7e2))')
         enddo
         call WrScr("          Root Orientations")
         do i=1,NumBlades_C
            j=9*(i-1)
            call WrMatrix(BldRootOri_C(j+1:j+9),CU,'(9(ES23.15e2))')
         enddo
      endif
      call WrScr("       NumMeshPts_C                   "//trim(Num2LStr( NumMeshPts_C  )) )
      if (debugverbose > 1) then
         call WrScr("          Mesh Positions")
         do i=1,NumMeshPts_C
            j=3*(i-1)
            call WrMatrix(InitMeshPos_C(j+1:j+3),CU,'(3(ES15.7e2))')
         enddo
         call WrScr("          Mesh Orientations")
         do i=1,NumMeshPts_C
            j=9*(i-1)
            call WrMatrix(InitMeshOri_C(j+1:j+9),CU,'(9(ES23.15e2))')
         enddo
      endif
      call WrScr("-----------------------------------------------------------")
   end subroutine ShowPassedData

   !> This subroutine sets the interface meshes to map to the input motions to the AD
   !! meshes
   subroutine SetMotionLoadsInterfaceMeshes(ErrStat3,ErrMsg3)
      integer(IntKi),         intent(  out)  :: ErrStat3    !< temporary error status
      character(ErrMsgLen),   intent(  out)  :: ErrMsg3     !< temporary error message
      integer(IntKi)          :: iNode
      real(ReKi)              :: InitPos(3)
      real(R8Ki)              :: Orient(3,3)
      !-------------------------------------------------------------
      ! Set the interface  meshes for motion inputs and loads output
      !-------------------------------------------------------------
      ! Motion mesh for blades
      call MeshCreate(  BldPtMotionMesh                     ,  &
                        IOS              = COMPONENT_INPUT  ,  &
                        Nnodes           = NumMeshPts       ,  &
                        ErrStat          = ErrStat3         ,  &
                        ErrMess          = ErrMsg3          ,  &
                        TranslationDisp  = .TRUE.,    Orientation = .TRUE., &
                        TranslationVel   = .TRUE.,    RotationVel = .TRUE., &
                        TranslationAcc   = .TRUE.,    RotationAcc = .FALSE. )
         if (ErrStat3 >= AbortErrLev) return

      do iNode=1,NumMeshPts
         ! initial position and orientation of node
         InitPos  = tmpBldPtMeshPos(1:3,iNode)
         if (TransposeDCM) then
            Orient   = transpose(tmpBldPtMeshOri(1:3,1:3,iNode))
         else
            Orient   = tmpBldPtMeshOri(1:3,1:3,iNode)
         endif
         call OrientRemap(Orient)
         call MeshPositionNode(  BldPtMotionMesh          , &
                                 iNode                    , &
                                 InitPos                  , &  ! position
                                 ErrStat3, ErrMsg3        , &
                                 Orient                     )  ! orientation
            if (ErrStat3 >= AbortErrLev) return
!FIXME: if we need to switch to line2 instead of point, do that here.
         call MeshConstructElement ( BldPtMotionMesh, ELEMENT_POINT, ErrStat3, ErrMsg3, iNode )
            if (ErrStat3 >= AbortErrLev) return
      enddo

      call MeshCommit ( BldPtMotionMesh, ErrStat3, ErrMsg3 )
         if (ErrStat3 >= AbortErrLev) return
      BldPtMotionMesh%RemapFlag  = .TRUE.

      ! For checking the mesh, uncomment this.
      !     note: CU is is output unit (platform dependent).
      if (debugverbose >= 4)  call MeshPrintInfo( CU, BldPtMotionMesh, MeshName='BldPtMotionMesh' )


!      !-------------------------------------------------------------
!      ! Motion mesh for nacelle -- TODO: add this mesh for nacelle load transfers
!      call MeshCreate(  NacMotionMesh                       ,  &
!                        IOS              = COMPONENT_INPUT  ,  &
!                        Nnodes           = 1                ,  &
!                        ErrStat          = ErrStat3         ,  &
!                        ErrMess          = ErrMsg3          ,  &
!                        TranslationDisp  = .TRUE.,    Orientation = .TRUE., &
!                        TranslationVel   = .TRUE.,    RotationVel = .TRUE., &
!                        TranslationAcc   = .TRUE.,    RotationAcc = .FALSE. )
!         if (ErrStat3 >= AbortErrLev) return
!
!      InitPos = real(NacPos_C(   1:3),ReKi)
!      Orient  = reshape( real(NacOri_C(1:9),ReKi), (/3,3/) )
!      call OrientRemap(Orient)
!      call MeshPositionNode(  NacMotionMesh           , &
!                              1                       , &
!                              InitPos                 , &  ! position
!                              ErrStat3, ErrMsg3       , &
!                              Orient                    )  ! orientation
!         if (ErrStat3 >= AbortErrLev) return
!
!      call MeshConstructElement ( NacMotionMesh, ELEMENT_POINT, ErrStat3, ErrMsg3, p1=1 )
!         if (ErrStat3 >= AbortErrLev) return
!
!      call MeshCommit ( NacMotionMesh, ErrStat3, ErrMsg3 )
!         if (ErrStat3 >= AbortErrLev) return
!      NacMotionMesh%RemapFlag    = .TRUE.
!
!      ! For checking the mesh, uncomment this.
!      !     note: CU is is output unit (platform dependent).
!      if (debugverbose >= 4)  call MeshPrintInfo( CU, NacMotionMesh, MeshName='NacMotionMesh' )
!
!
      !-------------------------------------------------------------
      ! Load mesh for blades
      CALL MeshCopy( SrcMesh  = BldPtMotionMesh    ,&
                     DestMesh = BldPtLoadMesh      ,&
                     CtrlCode = MESH_SIBLING       ,&
                     IOS      = COMPONENT_OUTPUT   ,&
                     ErrStat  = ErrStat3           ,&
                     ErrMess  = ErrMsg3            ,&
                     Force    = .TRUE.             ,&
                     Moment   = .TRUE.             )
         if (ErrStat3 >= AbortErrLev) return
      BldPtLoadMesh%RemapFlag  = .TRUE.

      ! Temp mesh for load transfer
      CALL MeshCopy( SrcMesh  = BldPtLoadMesh      ,&
                     DestMesh = BldPtLoadMesh_tmp  ,&
                     CtrlCode = MESH_COUSIN        ,&
                     IOS      = COMPONENT_OUTPUT   ,&
                     ErrStat  = ErrStat3           ,&
                     ErrMess  = ErrMsg3            ,&
                     Force    = .TRUE.             ,&
                     Moment   = .TRUE.             )
         if (ErrStat3 >= AbortErrLev) return
      BldPtLoadMesh_tmp%RemapFlag  = .TRUE.


      ! For checking the mesh
      !     note: CU is is output unit (platform dependent).
      if (debugverbose >= 4)  call MeshPrintInfo( CU, BldPtLoadMesh, MeshName='BldPtLoadMesh' )


!      !-------------------------------------------------------------
!      ! Load mesh for nacelle  -- TODO: add this mesh for nacelle load transfers
!      CALL MeshCopy( SrcMesh  = NacMotionMesh      ,&
!                     DestMesh = NacLoadMesh        ,&
!                     CtrlCode = MESH_SIBLING       ,&
!                     IOS      = COMPONENT_OUTPUT   ,&
!                     ErrStat  = ErrStat3           ,&
!                     ErrMess  = ErrMsg3            ,&
!                     Force    = .TRUE.             ,&
!                     Moment   = .TRUE.             )
!         if (ErrStat3 >= AbortErrLev) return
!      NacLoadMesh%RemapFlag  = .TRUE.
!
!      ! For checking the mesh, uncomment this.
!      !     note: CU is is output unit (platform dependent).
!      if (debugverbose >= 4)  call MeshPrintInfo( CU, NacLoadMesh, MeshName='NacLoadMesh' )


      !-------------------------------------------------------------
      ! Set the mapping meshes
      ! blades
      allocate(Map_BldPtMotion_2_AD_Blade(NumBlades),Map_AD_BldLoad_P_2_BldPtLoad(NumBlades),STAT=ErrStat3)
         if (ErrStat3 /= 0) then
            ErrStat3 = ErrID_Fatal
            ErrMsg3  = "Could not allocate Map_BldPtMotion_2_AD_Blade"
            return
         endif
      do i=1,NumBlades
         call MeshMapCreate( BldPtMotionMesh, u(1)%AD%rotors(1)%BladeMotion(i), Map_BldPtMotion_2_AD_Blade(i), ErrStat3, ErrMsg3 )
            if (ErrStat3 >= AbortErrLev) return
         call MeshMapCreate( y%AD%rotors(1)%BladeLoad(i), BldPtLoadMesh, Map_AD_BldLoad_P_2_BldPtLoad(i), ErrStat3, ErrMsg3 )
            if (ErrStat3 >= AbortErrLev) return
      enddo
      ! nacelle -- TODO: add this mesh for nacelle load transfers
!      if ( y%AD%rotors(1)%NacelleLoad%Committed ) then
!         call MeshMapCreate( NacMotionMesh, u(1)%AD%rotors(1)%NacelleMotion, Map_NacPtMotion_2_AD_Nac, ErrStat3, ErrMsg3 )
!            if (ErrStat3 >= AbortErrLev) return
!         call MeshMapCreate( y%AD%rotors(1)%NacelleLoad, NacLoadMesh, Map_AD_Nac_2_NacPtLoad, ErrStat3, ErrMsg3 )
!            if (ErrStat3 >= AbortErrLev) return
!      endif

   end subroutine SetMotionLoadsInterfaceMeshes


   !-------------------------------------------------------------
   !> Sanity check the nodes
   !!    If more than one input node was passed in, but only a single AD node
   !!    exists, then give error that too many
   !!    nodes passed.
   subroutine CheckNodes(ErrStat3,ErrMsg3)
      integer(IntKi),         intent(  out)  :: ErrStat3    !< temporary error status
      character(ErrMsgLen),   intent(  out)  :: ErrMsg3     !< temporary error message
      ErrStat3 = ErrID_None
      ErrMsg3  = ""
      ! FIXME: this is a placeholder in case we think of some sanity checks to perform.
      !     - some check that nodes make some sense -- might be caught in meshmapping
      !     - some checks on hub/nacelle being near middle of the rotor?  Not sure if that matters
   end subroutine CheckNodes

END SUBROUTINE AeroDyn_Inflow_C_Init


!!===============================================================================================================
!!--------------------------------------------- AeroDyn ReInit---------------------------------------------------
!!===============================================================================================================
!!TODO: finish this routine so it is usable if we need re-init capability for coupling
!SUBROUTINE AeroDyn_Inflow_C_ReInit( T_initial_C, DT_C, TMax_C,                     &
!               ErrStat_C, ErrMsg_C) BIND (C, NAME='AeroDyn_Inflow_C_ReInit')
!   implicit none
!#ifndef IMPLICIT_DLLEXPORT
!!DEC$ ATTRIBUTES DLLEXPORT :: AeroDyn_Inflow_C_ReInit
!!GCC$ ATTRIBUTES DLLEXPORT :: AeroDyn_Inflow_C_ReInit
!#endif
!
!   real(c_double),            intent(in   )  :: T_initial_C
!   real(c_double),            intent(in   )  :: DT_C              !< Timestep used with AD for stepping forward from t to t+dt.  Must be constant.
!   real(c_double),            intent(in   )  :: TMax_C            !< Maximum time for simulation (used to set arrays for wave kinematics)
!   integer(c_int),            intent(  out)  :: ErrStat_C                              !< Error status
!   character(kind=c_char),    intent(  out)  :: ErrMsg_C(ErrMsgLen_C)                  !< Error message (C_NULL_CHAR terminated)
!
!   integer(IntKi)                            :: ErrStat           !< aggregated error message
!   character(ErrMsgLen)                      :: ErrMsg            !< aggregated error message
!   integer(IntKi)                            :: ErrStat2          !< temporary error status  from a call
!   character(ErrMsgLen)                      :: ErrMsg2           !< temporary error message from a call
!   character(*), parameter                   :: RoutineName = 'AeroDyn_Inflow_C_ReInit'  !< for error handling
!
!   ! Initialize error handling
!   ErrStat  =  ErrID_None
!   ErrMsg   =  ""
!
!ErrStat  =  ErrID_Fatal
!ErrMsg   =  "AeroDyn_Inflo_C_ReInit is not currently functional. Aborting."
!call SetErr(ErrStat,ErrMsg,ErrStat_C,ErrMsg_C)
!
!   call ADI_ReInit(p, x(STATE_CURR), xd(STATE_CURR), z(STATE_CURR), OtherStates(STATE_CURR), m, dT_Global, errStat2, errMsg2)
!      if (Failed())  return
!
!   call SetErr(ErrStat,ErrMsg,ErrStat_C,ErrMsg_C)
!
!CONTAINS
!   logical function Failed()
!      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!      Failed = ErrStat >= AbortErrLev
!      if (Failed) then
!         call SetErr(ErrStat,ErrMsg,ErrStat_C,ErrMsg_C)
!      endif
!   end function Failed
!END SUBROUTINE AeroDyn_Inflow_C_ReInit


!===============================================================================================================
!--------------------------------------------- AeroDyn CalcOutput ---------------------------------------------
!===============================================================================================================

SUBROUTINE AeroDyn_Inflow_C_CalcOutput(Time_C, &
               HubPos_C,   HubOri_C,   HubVel_C,   HubAcc_C,     &
               NacPos_C,   NacOri_C,   NacVel_C,   NacAcc_C,     &
               BldRootPos_C, BldRootOri_C, BldRootVel_C, BldRootAcc_C, &
               NumMeshPts_C,  &
               MeshPos_C,  MeshOri_C,  MeshVel_C,  MeshAcc_C,  &
               MeshFrc_C, OutputChannelValues_C, ErrStat_C, ErrMsg_C) BIND (C, NAME='AeroDyn_Inflow_C_CalcOutput')
   implicit none
#ifndef IMPLICIT_DLLEXPORT
!DEC$ ATTRIBUTES DLLEXPORT :: AeroDyn_Inflow_C_CalcOutput
!GCC$ ATTRIBUTES DLLEXPORT :: AeroDyn_Inflow_C_CalcOutput
#endif
   real(c_double),            intent(in   )  :: Time_C
   real(c_float),             intent(in   )  :: HubPos_C( 3 )                 !< Hub position
   real(c_double),            intent(in   )  :: HubOri_C( 9 )                 !< Hub orientation
   real(c_float),             intent(in   )  :: HubVel_C( 6 )                 !< Hub velocity
   real(c_float),             intent(in   )  :: HubAcc_C( 6 )                 !< Hub acceleration
   real(c_float),             intent(in   )  :: NacPos_C( 3 )                 !< Nacelle position
   real(c_double),            intent(in   )  :: NacOri_C( 9 )                 !< Nacelle orientation
   real(c_float),             intent(in   )  :: NacVel_C( 6 )                 !< Nacelle velocity
   real(c_float),             intent(in   )  :: NacAcc_C( 6 )                 !< Nacelle acceleration
   real(c_float),             intent(in   )  :: BldRootPos_C( 3*NumBlades )   !< Blade root positions
   real(c_double),            intent(in   )  :: BldRootOri_C( 9*NumBlades )   !< Blade root orientations
   real(c_float),             intent(in   )  :: BldRootVel_C( 6*NumBlades )   !< Blade root velocities
   real(c_float),             intent(in   )  :: BldRootAcc_C( 6*NumBlades )   !< Blade root accelerations
   ! Blade mesh nodes
   integer(c_int),            intent(in   )  :: NumMeshPts_C                  !< Number of mesh points we are transfering motions to and output loads to
   real(c_float),             intent(in   )  :: MeshPos_C( 3*NumMeshPts_C )   !< A 3xNumMeshPts_C array [x,y,z]
   real(c_double),            intent(in   )  :: MeshOri_C( 9*NumMeshPts_C )   !< A 9xNumMeshPts_C array [r11,r12,r13,r21,r22,r23,r31,r32,r33]
   real(c_float),             intent(in   )  :: MeshVel_C( 6*NumMeshPts_C )   !< A 6xNumMeshPts_C array [x,y,z]
   real(c_float),             intent(in   )  :: MeshAcc_C( 6*NumMeshPts_C )   !< A 6xNumMeshPts_C array [x,y,z]
   real(c_float),             intent(  out)  :: MeshFrc_C( 6*NumMeshPts_C )   !< A 6xNumMeshPts_C array [Fx,Fy,Fz,Mx,My,Mz]       -- forces and moments (global)
   real(c_float),             intent(  out)  :: OutputChannelValues_C(p%NumOuts)
   integer(c_int),            intent(  out)  :: ErrStat_C
   character(kind=c_char),    intent(  out)  :: ErrMsg_C(ErrMsgLen_C)

   ! Local variables
   real(DbKi)                                :: Time
   integer(IntKi)                            :: iNode
   integer(IntKi)                            :: ErrStat                       !< aggregated error status
   character(ErrMsgLen)                      :: ErrMsg                        !< aggregated error message
   integer(IntKi)                            :: ErrStat2                      !< temporary error status  from a call
   character(ErrMsgLen)                      :: ErrMsg2                       !< temporary error message from a call
   character(*), parameter                   :: RoutineName = 'AeroDyn_Inflow_C_CalcOutput' !< for error handling

   ! Initialize error handling
   ErrStat  =  ErrID_None
   ErrMsg   =  ""

   ! Sanity check -- number of node points cannot change
   if ( NumMeshPts /= int(NumMeshPts_C, IntKi) ) then
      ErrStat2 =  ErrID_Fatal
      ErrMsg2  =  "Number of node points passed in changed.  This must be constant throughout simulation"
      if (Failed())  return
   endif


   ! Convert the inputs from C to Fortrn
   Time = REAL(Time_C,DbKi)

   ! Reshape mesh position, orientation, velocity, acceleration
   tmpBldPtMeshPos(1:3,1:NumMeshPts)      = reshape( real(MeshPos_C(1:3*NumMeshPts),ReKi), (/3,  NumMeshPts/) )
   tmpBldPtMeshOri(1:3,1:3,1:NumMeshPts)  = reshape( real(MeshOri_C(1:9*NumMeshPts),R8Ki), (/3,3,NumMeshPts/) )
   tmpBldPtMeshVel(1:6,1:NumMeshPts)      = reshape( real(MeshVel_C(1:6*NumMeshPts),ReKi), (/6,  NumMeshPts/) )
   tmpBldPtMeshAcc(1:6,1:NumMeshPts)      = reshape( real(MeshAcc_C(1:6*NumMeshPts),ReKi), (/6,  NumMeshPts/) )


   ! Transfer motions to input meshes
   call Set_MotionMesh( ErrStat2, ErrMsg2 );    if (Failed())  return
   call AD_SetInputMotion( u(1), &
            HubPos_C,   HubOri_C,   HubVel_C,   HubAcc_C,      &
            NacPos_C,   NacOri_C,   NacVel_C,   NacAcc_C,      &
            BldRootPos_C, BldRootOri_C, BldRootVel_C,   BldRootAcc_C,   &
            ErrStat2, ErrMsg2 )  ! transfer input motion mesh to u(1) meshes
      if (Failed())  return

   ! Call the main subroutine ADI_CalcOutput to get the resulting forces and moments at time T
   CALL ADI_CalcOutput( Time, u(1), p, x(STATE_CURR), xd(STATE_CURR), z(STATE_CURR), OtherStates(STATE_CURR), y, m, ErrStat2, ErrMsg2 )
      if (Failed())  return

   ! Transfer resulting load meshes to intermediate mesh
   call AD_TransferLoads( u(1), y, ErrStat2, ErrMsg2 )
      if (Failed())  return

   ! Set output force/moment array
   call Set_OutputLoadArray( )
   MeshFrc_C(1:6*NumMeshPts) = reshape( real(tmpBldPtMeshFrc(1:6,1:NumMeshPts), c_float), (/6*NumMeshPts/) )

   ! Get the output channel info out of y
   OutputChannelValues_C = REAL(y%WriteOutput, C_FLOAT)

   ! Write VTK if requested (animation=2)
   if (WrVTK > 1_IntKi)    call WrVTK_Meshes(u(1)%AD%rotors(:),(/0.0_SiKi,0.0_SiKi,0.0_SiKi/),ErrStat2,ErrMsg2)

   ! Set error status
   call SetErr(ErrStat,ErrMsg,ErrStat_C,ErrMsg_C)

CONTAINS
   logical function Failed()
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      Failed = ErrStat >= AbortErrLev
      if (Failed)    call SetErr(ErrStat,ErrMsg,ErrStat_C,ErrMsg_C)
   end function Failed
END SUBROUTINE AeroDyn_Inflow_C_CalcOutput

!===============================================================================================================
!--------------------------------------------- AeroDyn UpdateStates -------------------------------------------
!===============================================================================================================
!> This routine updates the states from Time_C to TimeNext_C.  It is assumed that the inputs are given for
!! TimeNext_C, but will be checked against the previous timestep values.
!! Since we don't really know if we are doing correction steps or not, we will track the previous state and
!! reset to those if we are repeating a timestep (normally this would be handled by the OF glue code, but since
!! the states are not passed across the interface, we must handle them here).
SUBROUTINE AeroDyn_Inflow_C_UpdateStates( Time_C, TimeNext_C, &
               HubPos_C,   HubOri_C,   HubVel_C,   HubAcc_C,     &
               NacPos_C,   NacOri_C,   NacVel_C,   NacAcc_C,     &
               BldRootPos_C, BldRootOri_C, BldRootVel_C, BldRootAcc_C, &
               NumMeshPts_C,  &
               MeshPos_C,  MeshOri_C,  MeshVel_C,  MeshAcc_C,  &
               ErrStat_C, ErrMsg_C) BIND (C, NAME='AeroDyn_Inflow_C_UpdateStates')
   implicit none
#ifndef IMPLICIT_DLLEXPORT
!DEC$ ATTRIBUTES DLLEXPORT :: AeroDyn_Inflow_C_UpdateStates
!GCC$ ATTRIBUTES DLLEXPORT :: AeroDyn_Inflow_C_UpdateStates
#endif
   real(c_double),            intent(in   )  :: Time_C
   real(c_double),            intent(in   )  :: TimeNext_C
   real(c_float),             intent(in   )  :: HubPos_C( 3 )                 !< Hub position
   real(c_double),            intent(in   )  :: HubOri_C( 9 )                 !< Hub orientation
   real(c_float),             intent(in   )  :: HubVel_C( 6 )                 !< Hub velocity
   real(c_float),             intent(in   )  :: HubAcc_C( 6 )                 !< Hub acceleration
   real(c_float),             intent(in   )  :: NacPos_C( 3 )                 !< Nacelle position
   real(c_double),            intent(in   )  :: NacOri_C( 9 )                 !< Nacelle orientation
   real(c_float),             intent(in   )  :: NacVel_C( 6 )                 !< Nacelle velocity
   real(c_float),             intent(in   )  :: NacAcc_C( 6 )                 !< Nacelle acceleration
   real(c_float),             intent(in   )  :: BldRootPos_C( 3*NumBlades )   !< Blade root positions
   real(c_double),            intent(in   )  :: BldRootOri_C( 9*NumBlades )   !< Blade root orientations
   real(c_float),             intent(in   )  :: BldRootVel_C( 6*NumBlades )   !< Blade root velocities
   real(c_float),             intent(in   )  :: BldRootAcc_C( 6*NumBlades )   !< Blade root accelerations
   ! Blade mesh nodes
   integer(c_int),            intent(in   )  :: NumMeshPts_C                  !< Number of mesh points we are transfering motions to and output loads to
   real(c_float),             intent(in   )  :: MeshPos_C( 3*NumMeshPts_C )   !< A 3xNumMeshPts_C array [x,y,z]
   real(c_double),            intent(in   )  :: MeshOri_C( 9*NumMeshPts_C )   !< A 9xNumMeshPts_C array [r11,r12,r13,r21,r22,r23,r31,r32,r33]
   real(c_float),             intent(in   )  :: MeshVel_C( 6*NumMeshPts_C )   !< A 6xNumMeshPts_C array [x,y,z]
   real(c_float),             intent(in   )  :: MeshAcc_C( 6*NumMeshPts_C )   !< A 6xNumMeshPts_C array [x,y,z]
   integer(c_int),            intent(  out)  :: ErrStat_C
   character(kind=c_char),    intent(  out)  :: ErrMsg_C(ErrMsgLen_C)

   ! Local variables
   logical                                   :: CorrectionStep                ! if we are repeating a timestep in UpdateStates, don't update the inputs array
   integer(IntKi)                            :: iNode
   integer(IntKi)                            :: ErrStat                       !< aggregated error status
   character(ErrMsgLen)                      :: ErrMsg                        !< aggregated error message
   integer(IntKi)                            :: ErrStat2                      !< temporary error status  from a call
   character(ErrMsgLen)                      :: ErrMsg2                       !< temporary error message from a call
   character(*), parameter                   :: RoutineName = 'AeroDyn_Inflow_C_UpdateStates' !< for error handling

   ! Initialize error handling
   ErrStat  =  ErrID_None
   ErrMsg   =  ""
   CorrectionStep = .false.

   ! Sanity check -- number of node points cannot change
   if ( NumMeshPts /= int(NumMeshPts_C, IntKi) ) then
      ErrStat2 =  ErrID_Fatal
      ErrMsg2  =  "Number of node points passed in changed.  This must be constant throughout simulation"
      if (Failed())  return
   endif


   !-------------------------------------------------------
   ! Check the time for current timestep and next timestep
   !-------------------------------------------------------
   !     These inputs are used in the time stepping algorithm within AD_UpdateStates
   !     For quadratic interpolation (InterpOrder==2), 3 timesteps are used.  For
   !     linear (InterOrder==1), 2 timesteps (the AD code can handle either).
   !        u(1)  inputs at t + dt        ! Next timestep
   !        u(2)  inputs at t             ! This timestep
   !        u(3)  inputs at t - dt        ! previous timestep (quadratic only)
   !
   !  NOTE: Within AD, the Radiation calculations can be done at an integer multiple of the
   !        timestep.  This is checked at each UpdateStates call.  However, if we compile
   !        in double precision, the values of Time_C and TimeNext_C are in double precison,
   !        but InputTimes is in DbKi (which is promoted quad precision when compiling in
   !        double precision) and the check may fail.  So we are going to set the times we
   !        we pass over to UpdateStates using the global timestep and the stored DbKi value
   !        for the timestep rather than the lower precision (when compiled double) time
   !        values passed in.  It is a bit of a clumsy workaround for this precision loss,
   !        but should not affect any results.

   !  Check if we are repeating an UpdateStates call (for example in a predictor/corrector loop)
   if ( EqualRealNos( real(Time_C,DbKi), InputTimePrev ) ) then
      CorrectionStep = .true.
   else ! Setup time input times array
      InputTimePrev          = real(Time_C,DbKi)            ! Store for check next time
      if (InterpOrder>1) then ! quadratic, so keep the old time
         InputTimes(INPUT_LAST) = ( N_Global - 1 ) * dT_Global    ! u(3) at T-dT
      endif
      InputTimes(INPUT_CURR) =   N_Global       * dT_Global       ! u(2) at T
      InputTimes(INPUT_PRED) = ( N_Global + 1 ) * dT_Global       ! u(1) at T+dT
      N_Global = N_Global + 1_IntKi                               ! increment counter to T+dT
   endif


   if (CorrectionStep) then
      ! Step back to previous state because we are doing a correction step
      !     -- repeating the T -> T+dt update with new inputs at T+dt
      !     -- the STATE_CURR contains states at T+dt from the previous call, so revert those
      CALL ADI_CopyContState   (x(          STATE_LAST), x(          STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2);  if (Failed())  return
      CALL ADI_CopyDiscState   (xd(         STATE_LAST), xd(         STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2);  if (Failed())  return
      CALL ADI_CopyConstrState (z(          STATE_LAST), z(          STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2);  if (Failed())  return
      CALL ADI_CopyOtherState  (OtherStates(STATE_LAST), OtherStates(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2);  if (Failed())  return
   else
      ! Cycle inputs back one timestep since we are moving forward in time.
      if (InterpOrder>1) then ! quadratic, so keep the old time
         call ADI_CopyInput( u(INPUT_CURR), u(INPUT_LAST), MESH_UPDATECOPY, ErrStat2, ErrMsg2);        if (Failed())  return
      endif
      ! Move inputs from previous t+dt (now t) to t
      call ADI_CopyInput( u(INPUT_PRED), u(INPUT_CURR), MESH_UPDATECOPY, ErrStat2, ErrMsg2);           if (Failed())  return
   endif

   !-------------------------------------------------------
   ! Set inputs for time T+dt -- u(INPUT_PRED)
   !-------------------------------------------------------
   ! Reshape mesh position, orientation, velocity, acceleration
   tmpBldPtMeshPos(1:3,1:NumMeshPts)      = reshape( real(MeshPos_C(1:3*NumMeshPts),ReKi), (/3,  NumMeshPts/) )
   tmpBldPtMeshOri(1:3,1:3,1:NumMeshPts)  = reshape( real(MeshOri_C(1:9*NumMeshPts),R8Ki), (/3,3,NumMeshPts/) )
   tmpBldPtMeshVel(1:6,1:NumMeshPts)      = reshape( real(MeshVel_C(1:6*NumMeshPts),ReKi), (/6,  NumMeshPts/) )
   tmpBldPtMeshAcc(1:6,1:NumMeshPts)      = reshape( real(MeshAcc_C(1:6*NumMeshPts),ReKi), (/6,  NumMeshPts/) )

   ! Transfer motions to input meshes
   call Set_MotionMesh( ErrStat2, ErrMsg2 );    if (Failed())  return
   call AD_SetInputMotion( u(INPUT_PRED), &
            HubPos_C,   HubOri_C,   HubVel_C,   HubAcc_C,      &
            NacPos_C,   NacOri_C,   NacVel_C,   NacAcc_C,      &
            BldRootPos_C, BldRootOri_C, BldRootVel_C,   BldRootAcc_C,   &
            ErrStat2, ErrMsg2 )  ! transfer input motion mesh to u(1) meshes
      if (Failed())  return


   ! Set copy the current state over to the predicted state for sending to UpdateStates
   !     -- The STATE_PREDicted will get updated in the call.
   !     -- The UpdateStates routine expects this to contain states at T at the start of the call (history not passed in)
   CALL ADI_CopyContState   (x(          STATE_CURR), x(          STATE_PRED), MESH_UPDATECOPY, Errstat2, ErrMsg2);  if (Failed())  return
   CALL ADI_CopyDiscState   (xd(         STATE_CURR), xd(         STATE_PRED), MESH_UPDATECOPY, Errstat2, ErrMsg2);  if (Failed())  return
   CALL ADI_CopyConstrState (z(          STATE_CURR), z(          STATE_PRED), MESH_UPDATECOPY, Errstat2, ErrMsg2);  if (Failed())  return
   CALL ADI_CopyOtherState  (OtherStates(STATE_CURR), OtherStates(STATE_PRED), MESH_UPDATECOPY, Errstat2, ErrMsg2);  if (Failed())  return


   ! Call the main subroutine ADI_UpdateStates to get the velocities
   CALL ADI_UpdateStates( InputTimes(INPUT_CURR), N_Global, u, InputTimes, p, x(STATE_PRED), xd(STATE_PRED), z(STATE_PRED), OtherStates(STATE_PRED), m, ErrStat2, ErrMsg2 )
      if (Failed())  return


   !-------------------------------------------------------
   ! cycle the states
   !-------------------------------------------------------
   ! move current state at T to previous state at T-dt
   !     -- STATE_LAST now contains info at time T
   !     -- this allows repeating the T --> T+dt update
   CALL ADI_CopyContState   (x(          STATE_CURR), x(          STATE_LAST), MESH_UPDATECOPY, Errstat2, ErrMsg2);  if (Failed())  return
   CALL ADI_CopyDiscState   (xd(         STATE_CURR), xd(         STATE_LAST), MESH_UPDATECOPY, Errstat2, ErrMsg2);  if (Failed())  return
   CALL ADI_CopyConstrState (z(          STATE_CURR), z(          STATE_LAST), MESH_UPDATECOPY, Errstat2, ErrMsg2);  if (Failed())  return
   CALL ADI_CopyOtherState  (OtherStates(STATE_CURR), OtherStates(STATE_LAST), MESH_UPDATECOPY, Errstat2, ErrMsg2);  if (Failed())  return
   ! Update the predicted state as the new current state
   !     -- we have now advanced from T to T+dt.  This allows calling with CalcOuput to get the outputs at T+dt
   CALL ADI_CopyContState   (x(          STATE_PRED), x(          STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2);  if (Failed())  return
   CALL ADI_CopyDiscState   (xd(         STATE_PRED), xd(         STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2);  if (Failed())  return
   CALL ADI_CopyConstrState (z(          STATE_PRED), z(          STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2);  if (Failed())  return
   CALL ADI_CopyOtherState  (OtherStates(STATE_PRED), OtherStates(STATE_CURR), MESH_UPDATECOPY, Errstat2, ErrMsg2);  if (Failed())  return



   call SetErr(ErrStat,ErrMsg,ErrStat_C,ErrMsg_C)

contains
   logical function Failed()
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      Failed = ErrStat >= AbortErrLev
      if (Failed)    call SetErr(ErrStat,ErrMsg,ErrStat_C,ErrMsg_C)
   end function Failed
END SUBROUTINE AeroDyn_Inflow_C_UpdateStates

!===============================================================================================================
!--------------------------------------------------- AeroDyn End-----------------------------------------------
!===============================================================================================================
!  NOTE: the error handling in this routine is slightly different than the other routines

SUBROUTINE AeroDyn_Inflow_C_End(ErrStat_C,ErrMsg_C) BIND (C, NAME='AeroDyn_Inflow_C_End')
   implicit none
#ifndef IMPLICIT_DLLEXPORT
!DEC$ ATTRIBUTES DLLEXPORT :: AeroDyn_Inflow_C_End
!GCC$ ATTRIBUTES DLLEXPORT :: AeroDyn_Inflow_C_End
#endif
   integer(c_int),          intent(  out) :: ErrStat_C
   character(kind=c_char),  intent(  out) :: ErrMsg_C(ErrMsgLen_C)

   ! Local variables
   integer(IntKi)             :: i                                !< generic loop counter
   integer                    :: ErrStat                          !< aggregated error status
   character(ErrMsgLen)       :: ErrMsg                           !< aggregated error message
   integer                    :: ErrStat2                         !< temporary error status  from a call
   character(ErrMsgLen)       :: ErrMsg2                          !< temporary error message from a call
   character(*), parameter    :: RoutineName = 'AeroDyn_Inflow_C_End'   !< for error handling

   ! Initialize error handling
   ErrStat  =  ErrID_None
   ErrMsg   =  ""

   ! clear out any globably allocated helper arrays
   if (allocated(tmpBldPtMeshPos))  deallocate(tmpBldPtMeshPos)
   if (allocated(tmpBldPtMeshOri))  deallocate(tmpBldPtMeshOri)
   if (allocated(tmpBldPtMeshVel))  deallocate(tmpBldPtMeshVel)
   if (allocated(tmpBldPtMeshAcc))  deallocate(tmpBldPtMeshAcc)
   if (allocated(tmpBldPtMeshFrc))  deallocate(tmpBldPtMeshFrc)


   ! Call the main subroutine ADI_End
   !     If u is not allocated, then we didn't get far at all in initialization,
   !     or AD_C_End got called before Init.  We don't want a segfault, so check
   !     for allocation.
   if (allocated(u)) then
      call ADI_End( u(:), p, x(STATE_CURR), xd(STATE_CURR), z(STATE_CURR), OtherStates(STATE_CURR), y, m, ErrStat2, ErrMsg2 )
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   endif

   !  NOTE: ADI_End only takes 1 instance of u, not the array.  So extra
   !        logic is required here (this isn't necessary in the fortran driver
   !        or in openfast, but may be when this code is called from C, Python,
   !        or some other code using the c-bindings.
   if (allocated(u)) then
      do i=2,size(u)
         call ADI_DestroyInput( u(i), ErrStat2, ErrMsg2 )
         call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      enddo
      if (allocated(u))             deallocate(u)
   endif

   ! Destroy any other copies of states (rerun on (STATE_CURR) is ok)
   call ADI_DestroyContState(   x(          STATE_LAST), ErrStat2, ErrMsg2 );  call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   call ADI_DestroyContState(   x(          STATE_CURR), ErrStat2, ErrMsg2 );  call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   call ADI_DestroyContState(   x(          STATE_PRED), ErrStat2, ErrMsg2 );  call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   call ADI_DestroyDiscState(   xd(         STATE_LAST), ErrStat2, ErrMsg2 );  call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   call ADI_DestroyDiscState(   xd(         STATE_CURR), ErrStat2, ErrMsg2 );  call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   call ADI_DestroyDiscState(   xd(         STATE_PRED), ErrStat2, ErrMsg2 );  call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   call ADI_DestroyConstrState( z(          STATE_LAST), ErrStat2, ErrMsg2 );  call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   call ADI_DestroyConstrState( z(          STATE_CURR), ErrStat2, ErrMsg2 );  call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   call ADI_DestroyConstrState( z(          STATE_PRED), ErrStat2, ErrMsg2 );  call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   call ADI_DestroyOtherState(  OtherStates(STATE_LAST), ErrStat2, ErrMsg2 );  call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   call ADI_DestroyOtherState(  OtherStates(STATE_CURR), ErrStat2, ErrMsg2 );  call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
   call ADI_DestroyOtherState(  OtherStates(STATE_PRED), ErrStat2, ErrMsg2 );  call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

   ! if deallocate other items now
   if (allocated(InputTimes))    deallocate(InputTimes)

   ! Clear out mesh related data storage
   call ClearMesh()

   call SetErr(ErrStat,ErrMsg,ErrStat_C,ErrMsg_C)
CONTAINS
   !> Don't leave junk in memory.  So destroy meshes and mappings.
   subroutine ClearMesh()
      ! Blade
      call MeshDestroy( BldPtMotionMesh, ErrStat2, ErrMsg2 )
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      call MeshDestroy( BldPtLoadMesh, ErrStat2, ErrMsg2 )
      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      ! Destroy mesh mappings
      if (allocated(Map_BldPtMotion_2_AD_Blade)) then
         do i=1,NumBlades
            call NWTC_Library_Destroymeshmaptype( Map_BldPtMotion_2_AD_Blade(i), ErrStat2, ErrMsg2 )
            call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
         enddo
         deallocate(Map_BldPtMotion_2_AD_Blade)
      endif
      if (allocated(Map_AD_BldLoad_P_2_BldPtLoad)) then
         do i=1,NumBlades
            call NWTC_Library_Destroymeshmaptype( Map_AD_BldLoad_P_2_BldPtLoad(i), ErrStat2, ErrMsg2 )
            call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
         enddo
         deallocate(Map_AD_BldLoad_P_2_BldPtLoad)
      endif
      ! Nacelle -- TODO: add this mesh for nacelle load transfers
!      call MeshDestroy( NacMotionMesh, ErrStat2, ErrMsg2 )
!      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!      call MeshDestroy( NacLoadMesh, ErrStat2, ErrMsg2 )
!      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!      call NWTC_Library_Destroymeshmaptype( Map_AD_Nac_2_NacPtLoad   , ErrStat2, ErrMsg2 )
!      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
!      call NWTC_Library_Destroymeshmaptype( Map_NacPtMotion_2_AD_Nac   , ErrStat2, ErrMsg2 )
!      call SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )

   end subroutine ClearMesh
END SUBROUTINE AeroDyn_Inflow_C_End


!> This routine is operating on module level data.  Error handling here in case checks added
subroutine Set_MotionMesh(ErrStat3, ErrMsg3)
   integer(IntKi),            intent(  out)  :: ErrStat3
   character(ErrMsgLen),      intent(  out)  :: ErrMsg3
   integer(IntKi)                            :: iNode
   ErrStat3 =  0_IntKi
   ErrMsg3  =  ''
   ! Set mesh corresponding to input motions
   do iNode=1,NumMeshPts
      BldPtMotionMesh%TranslationDisp(1:3,iNode) = tmpBldPtMeshPos(1:3,iNode) - real(BldPtMotionMesh%Position(1:3,iNode), R8Ki)
      BldPtMotionMesh%Orientation(1:3,1:3,iNode) = tmpBldPtMeshOri(1:3,1:3,iNode)
      BldPtMotionMesh%TranslationVel( 1:3,iNode) = tmpBldPtMeshVel(1:3,iNode)
      BldPtMotionMesh%RotationVel(    1:3,iNode) = tmpBldPtMeshVel(4:6,iNode)
      BldPtMotionMesh%TranslationAcc( 1:3,iNode) = tmpBldPtMeshAcc(1:3,iNode)
      !BldPtMotionMesh%RotationAcc(    1:3,iNode) = tmpBldPtMeshAcc(4:6,iNode)   ! Rotational acc not included
      call OrientRemap(BldPtMotionMesh%Orientation(1:3,1:3,iNode))
      if (TransposeDCM) then
         BldPtMotionMesh%Orientation(1:3,1:3,iNode) = transpose(BldPtMotionMesh%Orientation(1:3,1:3,iNode))
      endif
   enddo
end subroutine Set_MotionMesh

!> Map the motion of the intermediate input mesh over to the input meshes
!! This routine is operating on module level data, hence few inputs
subroutine AD_SetInputMotion( u_local,             &
         HubPos_C, HubOri_C, HubVel_C, HubAcc_C,   &
         NacPos_C, NacOri_C, NacVel_C, NacAcc_C,   &
         BldRootPos_C, BldRootOri_C, BldRootVel_C, BldRootAcc_C,  &
         ErrStat, ErrMsg )
   type(ADI_InputType),       intent(inout)  :: u_local                       ! Only one input (probably at T)
   real(c_float),             intent(in   )  :: HubPos_C( 3 )                 !< Hub position
   real(c_double),            intent(in   )  :: HubOri_C( 9 )                 !< Hub orientation
   real(c_float),             intent(in   )  :: HubVel_C( 6 )                 !< Hub velocity
   real(c_float),             intent(in   )  :: HubAcc_C( 6 )                 !< Hub acceleration
   real(c_float),             intent(in   )  :: NacPos_C( 3 )                 !< Nacelle position
   real(c_double),            intent(in   )  :: NacOri_C( 9 )                 !< Nacelle orientation
   real(c_float),             intent(in   )  :: NacVel_C( 6 )                 !< Nacelle velocity
   real(c_float),             intent(in   )  :: NacAcc_C( 6 )                 !< Nacelle acceleration
   real(c_float),             intent(in   )  :: BldRootPos_C( 3*NumBlades )   !< Blade root positions
   real(c_double),            intent(in   )  :: BldRootOri_C( 9*NumBlades )   !< Blade root orientations
   real(c_float),             intent(in   )  :: BldRootVel_C( 6*NumBlades )   !< Blade root velocities
   real(c_float),             intent(in   )  :: BldRootAcc_C( 6*NumBlades )   !< Blade root accelerations
   integer(IntKi),            intent(  out)  :: ErrStat
   character(ErrMsgLen),      intent(  out)  :: ErrMsg
   integer(IntKi)                            :: i
   ErrStat =  0_IntKi
   ErrMsg  =  ''
   ! Hub -- NOTE: RotationalAcc not present in the mesh
   if ( u_local%AD%rotors(1)%HubMotion%Committed ) then
      u_local%AD%rotors(1)%HubMotion%TranslationDisp(1:3,1) = real(HubPos_C(1:3),R8Ki) - real(u_local%AD%rotors(1)%HubMotion%Position(1:3,1), R8Ki)
      u_local%AD%rotors(1)%HubMotion%Orientation(1:3,1:3,1) = reshape( real(HubOri_C(1:9),R8Ki), (/3,3/) )
      u_local%AD%rotors(1)%HubMotion%TranslationVel(1:3,1)  = real(HubVel_C(1:3), ReKi)
      u_local%AD%rotors(1)%HubMotion%RotationVel(1:3,1)     = real(HubVel_C(4:6), ReKi)
      u_local%AD%rotors(1)%HubMotion%TranslationAcc(1:3,1)  = real(HubAcc_C(1:3), ReKi)
      call OrientRemap(u_local%AD%rotors(1)%HubMotion%Orientation(1:3,1:3,1))
      if (TransposeDCM) then
         u_local%AD%rotors(1)%HubMotion%Orientation(1:3,1:3,1) = transpose(u_local%AD%rotors(1)%HubMotion%Orientation(1:3,1:3,1))
      endif
   endif
   ! Nacelle -- NOTE: RotationalVel and RotationalAcc not present in the mesh
   if ( u_local%AD%rotors(1)%NacelleMotion%Committed ) then
      u_local%AD%rotors(1)%NacelleMotion%TranslationDisp(1:3,1) = real(NacPos_C(1:3),R8Ki) - real(u_local%AD%rotors(1)%NacelleMotion%Position(1:3,1), R8Ki)
      u_local%AD%rotors(1)%NacelleMotion%Orientation(1:3,1:3,1) = reshape( real(NacOri_C(1:9),R8Ki), (/3,3/) )
      u_local%AD%rotors(1)%NacelleMotion%TranslationVel(1:3,1)  = real(NacVel_C(1:3), ReKi)
      u_local%AD%rotors(1)%NacelleMotion%TranslationAcc(1:3,1)  = real(NacAcc_C(1:3), ReKi)
      call OrientRemap(u_local%AD%rotors(1)%NacelleMotion%Orientation(1:3,1:3,1))
      if (TransposeDCM) then
         u_local%AD%rotors(1)%NacelleMotion%Orientation(1:3,1:3,1) = transpose(u_local%AD%rotors(1)%NacelleMotion%Orientation(1:3,1:3,1))
      endif
   endif
   ! Blade root
   do i=0,numBlades-1
      if ( u_local%AD%rotors(1)%BladeRootMotion(i+1)%Committed ) then
         u_local%AD%rotors(1)%BladeRootMotion(i+1)%TranslationDisp(1:3,1) = real(BldRootPos_C(3*i+1:3*i+3),R8Ki) - real(u_local%AD%rotors(1)%BladeRootMotion(i+1)%Position(1:3,1), R8Ki)
         u_local%AD%rotors(1)%BladeRootMotion(i+1)%Orientation(1:3,1:3,1) = reshape( real(BldRootOri_C(9*i+1:9*i+9),R8Ki), (/3,3/) )
         u_local%AD%rotors(1)%BladeRootMotion(i+1)%TranslationVel(1:3,1)  = real(BldRootVel_C(6*i+1:6*i+3), ReKi)
         u_local%AD%rotors(1)%BladeRootMotion(i+1)%RotationVel(1:3,1)     = real(BldRootVel_C(6*i+4:6*i+6), ReKi)
         u_local%AD%rotors(1)%BladeRootMotion(i+1)%TranslationAcc(1:3,1)  = real(BldRootAcc_C(6*i+1:6*i+3), ReKi)
         u_local%AD%rotors(1)%BladeRootMotion(i+1)%RotationAcc(1:3,1)     = real(BldRootAcc_C(6*i+4:6*i+6), ReKi)
         call OrientRemap(u_local%AD%rotors(1)%BladeRootMotion(i+1)%Orientation(1:3,1:3,1))
         if (TransposeDCM) then
            u_local%AD%rotors(1)%BladeRootMotion(i+1)%Orientation(1:3,1:3,1) = transpose(u_local%AD%rotors(1)%BladeRootMotion(i+1)%Orientation(1:3,1:3,1))
         endif
      endif
   enddo

   ! Blade mesh
   do i=1,numBlades
      if ( u_local%AD%rotors(1)%BladeMotion(i)%Committed ) then
         call Transfer_Point_to_Line2( BldPtMotionMesh, u_local%AD%rotors(1)%BladeMotion(i), Map_BldPtMotion_2_AD_Blade(i), ErrStat, ErrMsg )
         if (ErrStat >= AbortErrLev)  return
      endif
   enddo
end subroutine AD_SetInputMotion

!> Map the loads of the output mesh to the intermediate output mesh.
!! This routine is operating on module level data, hence few inputs
subroutine AD_TransferLoads( u_local, y_local, ErrStat3, ErrMsg3 )
   type(ADI_InputType),    intent(in   )  :: u_local           ! Only one input (probably at T)
   type(ADI_OutputType),   intent(in   )  :: y_local     ! Only one input (probably at T)
   integer(IntKi),         intent(  out)  :: ErrStat3
   character(ErrMsgLen),   intent(  out)  :: ErrMsg3
   integer(IntKi)                         :: i
   BldPtLoadMesh%Force     = 0.0_ReKi
   BldPtLoadMesh%Moment    = 0.0_ReKi
   do i=1,NumBlades
      if ( y_local%AD%rotors(1)%BladeLoad(i)%Committed ) then
         if (debugverbose > 4)  call MeshPrintInfo( CU, y_local%AD%rotors(1)%BladeLoad(i), MeshName='AD%rotors('//trim(Num2LStr(1))//')%BladeLoad('//trim(Num2LStr(i))//')' )
         call Transfer_Line2_to_Point( y%AD%rotors(1)%BladeLoad(i), BldPtLoadMesh_tmp, Map_AD_BldLoad_P_2_BldPtLoad(i), &
                  ErrStat3, ErrMsg3, u_local%AD%rotors(1)%BladeMotion(i), BldPtMotionMesh )
         if (ErrStat3 >= AbortErrLev)  return
         BldPtLoadMesh%Force  = BldPtLoadMesh%Force  + BldPtLoadMesh_tmp%Force
         BldPtLoadMesh%Moment = BldPtLoadMesh%Moment + BldPtLoadMesh_tmp%Moment
      endif
   enddo
   if (debugverbose > 4)  call MeshPrintInfo( CU, BldPtLoadMesh, MeshName='BldPtLoadMesh' )
end subroutine AD_TransferLoads

!> Transfer the loads from the load mesh to the temporary array for output
!! This routine is operating on module level data, hence few inputs
subroutine Set_OutputLoadArray()
   integer(IntKi)                            :: iNode
   ! Set mesh corresponding to input motions
   do iNode=1,NumMeshPts
      tmpBldPtMeshFrc(1:3,iNode)   = BldPtLoadMesh%Force (1:3,iNode)
      tmpBldPtMeshFrc(4:6,iNode)   = BldPtLoadMesh%Moment(1:3,iNode)
   enddo
end subroutine Set_OutputLoadArray

!> take DCM passed in, do Euler angle extract, then Euler angle construct back to DCM.  Idea here is we can account
!! for minor accuracy issues in the passed DCM
subroutine OrientRemap(DCM)
   real(R8Ki), intent(inout)  :: DCM(3,3)
   real(R8Ki)                 :: theta(3)
!   real(R8Ki)                 :: logMap(3)
!   integer(IntKi)             :: TmpErrStat  ! DCM_logMapD requires this output, but doesn't use it at all
!   character(ErrMsgLen)       :: TmpErrMsg   ! DCM_logMapD requires this output, but doesn't use it at all
!write(200,*)   reshape(DCM,(/9/))
   theta = EulerExtract(DCM)
   DCM = EulerConstruct(theta)
!   call DCM_logMap(DCM,logMap,TmpErrStat,TmpErrMsg)
!   DCM = DCM_Exp(logMap)
!write(201,*)   reshape(DCM,(/9/))
end subroutine OrientRemap


!----------------------------------------------------------------------------------------------------------------------------------
!> This subroutine sets up the information needed for plotting VTK surfaces.
!! NOTE: this is a duplicate of some pieces in the AeroDyn_Driver_Subs.  This should
!!       eventually be combined into ADI
subroutine SetVTKParameters(OutRootName, rot_u, RefPoint, errStat, errMsg)
   character(IntfStrLen),  intent(in   )  :: OutRootName    !< Root name to use for echo files and other
   type(RotInputType),     intent(in   )  :: rot_u(:)
   real(SiKi),             intent(in   )  :: RefPoint(3)
   integer(IntKi),         intent(  out)  :: errStat        !< Error status of the operation
   character(*),           intent(  out)  :: errMsg         !< Error message if errStat /= ErrID_None
   real(SiKi)                             :: RefLengths(2)               
   real(SiKi)                             :: TwrLength
   integer(IntKi)                         :: iBld, nNodes
   integer(IntKi)                         :: iWT
   character(IntfStrLen)                  :: TmpFileName
   integer(IntKi)                         :: errStat2
   character(ErrMsgLen)                   :: errMsg2
   character(*), parameter                :: RoutineName = 'SetVTKParameters'
   real(SiKi) :: BladeLength, MaxBladeLength, MaxTwrLength, GroundRad, MaxLength
   real(SiKi) :: WorldBoxMax(3), WorldBoxMin(3) ! Extent of the turbines
   real(SiKi) :: BaseBoxDim
   errStat = ErrID_None
   errMsg  = ""
   
   ! get the name of the output directory for vtk files (in a subdirectory called "vtk" of the output directory), and
   ! create the VTK directory if it does not exist
   call GetPath ( OutRootName, VTK_OutFileRoot, TmpFileName ) ! the returned VTK_OutFileRoot includes a file separator character at the end
   VTK_OutFileRoot = trim(VTK_OutFileRoot) // 'vtk-ADI'
   call MKDIR( trim(VTK_OutFileRoot) )
   VTK_OutFileRoot = trim( VTK_OutFileRoot ) // PathSep // trim(TmpFileName)

   ! calculate the number of digits in 'y_FAST%NOutSteps' (Maximum number of output steps to be written)
   ! this will be used to pad the write-out step in the VTK filename with zeros in calls to MeshWrVTK()
   VTK_tWidth = CEILING( log10( TMax / dT_Global ) ) + 1

   if (allocated(VTK_Surface)) then
      return ! The surfaces were already computed (for combined cases)
   endif

   allocate(VTK_Surface(NumTurbines))
   ! --- Find dimensions for all objects to determine "Ground" and typical dimensions
   MaxBladeLength = 0
   MaxTwrLength   = 0
   MaxLength      = 0
   do iWT=1,numTurbines
      do iBld=1, NumBlades
         nNodes = rot_u(iWT)%BladeMotion(iBld)%nnodes
         BladeLength = TwoNorm(rot_u(iWT)%BladeMotion(iBld)%Position(:,nNodes)-rot_u(iWT)%BladeMotion(iBld)%Position(:,1))
         MaxBladeLength = max(MaxBladeLength, BladeLength)
      enddo
      if (rot_u(iWt)%TowerMotion%Committed) then
        if (rot_u(iWt)%TowerMotion%NNodes>0) then
            TwrLength = TwoNorm( rot_u(iWt)%TowerMotion%position(:,1) - rot_u(iWt)%TowerMotion%position(:,rot_u(iWt)%TowerMotion%NNodes) ) 
            MaxTwrLength = max(MaxTwrLength, TwrLength)
         endif
      endif
      MaxLength = max(MaxLength, MaxTwrLength, MaxBladeLength)

      ! Determine extent of the objects
      if (iWT==1) then
         WorldBoxMax(1) =  RefPoint(1)+MaxLength
         WorldBoxMax(2) =  RefPoint(2)+MaxLength
         WorldBoxMax(3) =  RefPoint(3)+MaxLength ! NOTE: not used
         WorldBoxMin(1) =  RefPoint(1)-MaxLength
         WorldBoxMin(2) =  RefPoint(2)-MaxLength
         WorldBoxMin(3) =  RefPoint(3)-MaxLength ! NOTE: not used
      else
         WorldBoxMax(1) = max(WorldBoxMax(1), RefPoint(1)+MaxLength)
         WorldBoxMax(2) = max(WorldBoxMax(2), RefPoint(2)+MaxLength)
         WorldBoxMax(3) = max(WorldBoxMax(3), RefPoint(3)+MaxLength) ! NOTE: not used
         WorldBoxMin(1) = min(WorldBoxMin(1), RefPoint(1)-MaxLength)
         WorldBoxMin(2) = min(WorldBoxMin(2), RefPoint(2)-MaxLength)
         WorldBoxMin(3) = min(WorldBoxMin(3), RefPoint(3)-MaxLength) ! NOTE: not used
      endif
   enddo ! Loop on turbine 

   ! Get radius for ground (blade length + hub radius):
   GroundRad = MaxBladeLength + MaxTwrLength+ VTKHubRad
   ! write the ground or seabed reference polygon:
   ! Averaging the center point of the ground:
   !RefPoint(1:2) = dvr%WT(1)%originInit(1:2)
   !do iWT=2,NumTurbines
   !   RefPoint(1:2) = RefPoint(1:2) + dvr%WT(iWT)%originInit(1:2)
   !end do
   !RefPoint(1:2) = RefPoint(1:2) / NumTurbines
   
   RefLengths  = GroundRad  + sqrt((WorldBoxMax(1)-WorldBoxMin(1))**2 + (WorldBoxMax(2)-WorldBoxMin(2))**2)
   call WrVTK_Ground (RefPoint, RefLengths, trim(VTK_OutFileRoot) // '.GroundSurface', errStat2, errMsg2 )         


   ! --- Create surfaces for Nacelle, Base, Tower, Blades
   do iWT=1,NumTurbines
      VTK_Surface(iWT)%NumSectors = 25   

      ! Create nacelle box
      VTK_Surface(iWT)%NacelleBox(:,1) = (/ VTKNacDim(1)             , VTKNacDim(2)+VTKNacDim(5), VTKNacDim(3) /)
      VTK_Surface(iWT)%NacelleBox(:,2) = (/ VTKNacDim(1)+VTKNacDim(4), VTKNacDim(2)+VTKNacDim(5), VTKNacDim(3) /) 
      VTK_Surface(iWT)%NacelleBox(:,3) = (/ VTKNacDim(1)+VTKNacDim(4), VTKNacDim(2)             , VTKNacDim(3) /)
      VTK_Surface(iWT)%NacelleBox(:,4) = (/ VTKNacDim(1)             , VTKNacDim(2)             , VTKNacDim(3) /) 
      VTK_Surface(iWT)%NacelleBox(:,5) = (/ VTKNacDim(1)             , VTKNacDim(2)             , VTKNacDim(3)+VTKNacDim(6) /)
      VTK_Surface(iWT)%NacelleBox(:,6) = (/ VTKNacDim(1)+VTKNacDim(4), VTKNacDim(2)             , VTKNacDim(3)+VTKNacDim(6) /) 
      VTK_Surface(iWT)%NacelleBox(:,7) = (/ VTKNacDim(1)+VTKNacDim(4), VTKNacDim(2)+VTKNacDim(5), VTKNacDim(3)+VTKNacDim(6) /)
      VTK_Surface(iWT)%NacelleBox(:,8) = (/ VTKNacDim(1)             , VTKNacDim(2)+VTKNacDim(5), VTKNacDim(3)+VTKNacDim(6) /) 

      ! Create base box (using towerbase or nacelle dim)
      BaseBoxDim = minval(VTKNacDim(4:6))/2
      if (size(m%VTK_Surfaces(iWT)%TowerRad)>0) then
         BaseBoxDim = m%VTK_Surfaces(iWT)%TowerRad(1)
      endif
      VTK_Surface(iWT)%BaseBox(:,1) = (/ -BaseBoxDim             , -BaseBoxDim+2*BaseBoxDim, -BaseBoxDim /)
      VTK_Surface(iWT)%BaseBox(:,2) = (/ -BaseBoxDim+2*BaseBoxDim, -BaseBoxDim+2*BaseBoxDim, -BaseBoxDim /) 
      VTK_Surface(iWT)%BaseBox(:,3) = (/ -BaseBoxDim+2*BaseBoxDim, -BaseBoxDim             , -BaseBoxDim /)
      VTK_Surface(iWT)%BaseBox(:,4) = (/ -BaseBoxDim             , -BaseBoxDim             , -BaseBoxDim /) 
      VTK_Surface(iWT)%BaseBox(:,5) = (/ -BaseBoxDim             , -BaseBoxDim             , -BaseBoxDim+2*BaseBoxDim /)
      VTK_Surface(iWT)%BaseBox(:,6) = (/ -BaseBoxDim+2*BaseBoxDim, -BaseBoxDim             , -BaseBoxDim+2*BaseBoxDim /) 
      VTK_Surface(iWT)%BaseBox(:,7) = (/ -BaseBoxDim+2*BaseBoxDim, -BaseBoxDim+2*BaseBoxDim, -BaseBoxDim+2*BaseBoxDim /)
      VTK_Surface(iWT)%BaseBox(:,8) = (/ -BaseBoxDim             , -BaseBoxDim+2*BaseBoxDim, -BaseBoxDim+2*BaseBoxDim /) 

   enddo ! iWT, turbines
end subroutine SetVTKParameters

!> Write VTK reference meshes and setup directory if needed.
!! NOTE: it is assumed that only an fatal error will be returned in the subroutines contained here
subroutine WrVTK_refMeshes(rot_u, RefPoint, ErrStat, ErrMsg)
   type(RotInputType),     intent(in   )  :: rot_u(:)       !< pointer to rotor input (for easier to read code)
   real(SiKi),             intent(in   )  :: RefPoint(3)
   integer(IntKi),         intent(  out)  :: ErrStat        !< error status
   character(ErrMsgLen),   intent(  out)  :: ErrMsg         !< error message
   integer(IntKi)                         :: nBlades
   integer(IntKi)                         :: iWT, k
   character(*), parameter                :: RoutineName = 'WrVTK_refMeshes'  !< for error handling
   integer(IntKi)                         :: ErrStat2       !< temporary error status
   character(ErrMsgLen)                   :: ErrMsg2        !< temporary error message
   character(10)                          :: sWT

   ErrStat =  0_IntKi
   ErrMsg  =  ''

   iWT = 1  !TODO: expand for multiple turbines

   ! Turbine identifier
   if (NumTurbines==1) then
      sWT = ''
   else
      sWT = '.T'//trim(num2lstr(iWT))
   endif

   select case (WrVTK_Type)
      case (1)    ! surfaces -- don't write any surface references
         call WrVTK_PointsRef(  ErrStat2,ErrMsg2); if (Failed()) return;
      case (2)    ! lines
         call WrVTK_PointsRef(  ErrStat2,ErrMsg2); if (Failed()) return;
         call WrVTK_LinesRef(   ErrStat2,ErrMsg2); if (Failed()) return;
      case (3)    ! both
         call WrVTK_PointsRef(  ErrStat2,ErrMsg2); if (Failed()) return;
         call WrVTK_LinesRef(   ErrStat2,ErrMsg2); if (Failed()) return;
   end select

contains
   logical function Failed()
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      Failed = ErrStat >= AbortErrLev
   end function Failed

   !> meshes rendered at all times (points, or lines for fvw)
   subroutine WrVTK_PointsRef(ErrStat3,ErrMsg3)
      integer(IntKi),         intent(  out)  :: ErrStat3       !< error status
      character(ErrMsgLen),   intent(  out)  :: ErrMsg3        !< error message
      ErrStat3 =  0_IntKi
      ErrMsg3  =  ''

      ! Blade point motion (structural mesh from driver)
      call MeshWrVTKreference(RefPoint, BldPtMotionMesh, trim(VTK_OutFileRoot)//trim(sWT)//'.BldPtMotionMesh', ErrStat3, ErrMsg3)
      if (ErrStat3 >= AbortErrLev) return

      ! Blade root motion (point only)
      if (allocated(rot_u(iWT)%BladeRootMotion)) then
         do k=1,NumBlades
            if (rot_u(iWT)%BladeRootMotion(k)%Committed) then
               call MeshWrVTKreference(RefPoint, rot_u(iWT)%BladeRootMotion(k), trim(VTK_OutFileRoot)//trim(sWT)//'.BladeRootMotion'//trim(num2lstr(k)), ErrStat3, ErrMsg3 )
                  if (ErrStat3 >= AbortErrLev) return
            endif
         enddo
      endif

      ! Nacelle (structural point input
      if ( rot_u(iWT)%NacelleMotion%Committed ) call MeshWrVTKreference(RefPoint, rot_u(iWT)%NacelleMotion, trim(VTK_OutFileRoot)//trim(sWT)//'.NacelleMotion', ErrStat3, ErrMsg3)
      if (ErrStat3 >= AbortErrLev) return
   end subroutine WrVTK_PointsRef

   !> meshes rendered with lines only
   subroutine WrVTK_LinesRef(ErrStat3,ErrMsg3)
      integer(IntKi),         intent(  out)  :: ErrStat3       !< error status
      character(ErrMsgLen),   intent(  out)  :: ErrMsg3        !< error message
      ErrStat3 =  0_IntKi
      ErrMsg3  =  ''

      ! Tower
      if (rot_u(iWT)%TowerMotion%Committed) call MeshWrVTKreference(RefPoint, rot_u(iWT)%TowerMotion, trim(VTK_OutFileRoot)//trim(sWT)//'.Tower', ErrStat3, ErrMsg3 )
      if (ErrStat3 >= AbortErrLev) return

      ! Nacelle meshes
      if (rot_u(iWT)%NacelleMotion%Committed) call MeshWrVTKreference(RefPoint, rot_u(iWT)%NacelleMotion, trim(VTK_OutFileRoot)//trim(sWT)//'.Nacelle', ErrStat3, ErrMsg3 )
      if (ErrStat3 >= AbortErrLev) return

      ! Hub
      if (rot_u(iWT)%HubMotion%Committed) call MeshWrVTKreference(RefPoint, rot_u(iWT)%HubMotion, trim(VTK_OutFileRoot)//trim(sWT)//'.Hub', ErrStat3, ErrMsg3 )
      if (ErrStat3 >= AbortErrLev) return

      ! Blades
      if (allocated(rot_u(iWT)%BladeMotion)) then
         do k=1,NumBlades
            if (rot_u(iWT)%BladeMotion(k)%Committed) then
               call MeshWrVTKreference(RefPoint, rot_u(iWT)%BladeMotion(k), trim(VTK_OutFileRoot)//trim(sWT)//'.Blade'//trim(num2lstr(k)), ErrStat3, ErrMsg3 )
                  if (ErrStat3 >= AbortErrLev) return
            endif
         enddo
      endif
   end subroutine WrVTK_LinesRef
end subroutine WrVTK_refMeshes

!> Write VTK meshes
!! NOTE: it is assumed that only an fatal error will be returned in the subroutines contained here
subroutine WrVTK_Meshes(rot_u, RefPoint, ErrStat, ErrMsg)
   type(RotInputType),     intent(in   )  :: rot_u(:)       !< pointer to rotor input (for easier to read code)
   real(SiKi),             intent(in   )  :: RefPoint(3)    !< turbine reference point
   integer(IntKi),         intent(  out)  :: ErrStat        !< error status
   character(ErrMsgLen),   intent(  out)  :: ErrMsg         !< error message
   integer(IntKi)                         :: nBlades
   integer(IntKi)                         :: iWT, k
   character(IntfStrLen)                  :: TmpFileName
   character(*), parameter                :: RoutineName = 'WrVTK_Meshes'  !< for error handling
   integer(IntKi)                         :: ErrStat2       !< temporary error status
   character(ErrMsgLen)                   :: ErrMsg2        !< temporary error message
   character(10)                          :: sWT

   ErrStat =  0_IntKi
   ErrMsg  =  ''

   iWT = 1  !TODO: expand for multiple turbines

   ! Turbine identifier
   if (NumTurbines==1) then
      sWT = ''
   else
      sWT = '.T'//trim(num2lstr(iWT))
   endif


   select case (WrVTK_Type)
      case (1)    ! surfaces
         call WrVTK_Points(  ErrStat2,ErrMsg2); if (Failed()) return;
         call WrVTK_Surfaces(ErrStat2,ErrMsg2); if (Failed()) return;
      case (2)    ! lines
         call WrVTK_Points(  ErrStat2,ErrMsg2); if (Failed()) return;
         call WrVTK_Lines(   ErrStat2,ErrMsg2); if (Failed()) return;
      case (3)    ! both
         call WrVTK_Points(  ErrStat2,ErrMsg2); if (Failed()) return;
         call WrVTK_Surfaces(ErrStat2,ErrMsg2); if (Failed()) return;
         call WrVTK_Lines(   ErrStat2,ErrMsg2); if (Failed()) return;
   end select

contains
   logical function Failed()
      CALL SetErrStat( ErrStat2, ErrMsg2, ErrStat, ErrMsg, RoutineName )
      Failed = ErrStat >= AbortErrLev
   end function Failed

   !> meshes rendered at all times (points, or lines for fvw)
   subroutine WrVTK_Points(ErrStat3,ErrMsg3)
      use FVW_IO, only: WrVTK_FVW
      integer(IntKi),         intent(  out)  :: ErrStat3       !< error status
      character(ErrMsgLen),   intent(  out)  :: ErrMsg3        !< error message
      ErrStat3 =  0_IntKi
      ErrMsg3  =  ''

      ! Blade point motion (structural mesh from driver)
      call MeshWrVTK(RefPoint, BldPtMotionMesh, trim(VTK_OutFileRoot)//trim(sWT)//'.BldPtMotionMesh', N_Global, .true., ErrStat3, ErrMsg3, VTK_tWidth)
      if (ErrStat3 >= AbortErrLev) return

      ! Blade root motion (point only)
      if (allocated(rot_u(iWT)%BladeRootMotion)) then
         do k=1,NumBlades
            if (rot_u(iWT)%BladeRootMotion(k)%Committed) then
               call MeshWrVTK(RefPoint, rot_u(iWT)%BladeRootMotion(k), trim(VTK_OutFileRoot)//trim(sWT)//'.BladeRootMotion'//trim(num2lstr(k)), N_Global, .true., ErrStat3, ErrMsg3, VTK_tWidth)
                  if (ErrStat3 >= AbortErrLev) return
            endif
         enddo
      endif

      ! Nacelle (structural point input
      if ( rot_u(iWT)%NacelleMotion%Committed ) call MeshWrVTK(RefPoint, rot_u(iWT)%NacelleMotion, trim(VTK_OutFileRoot)//trim(sWT)//'.NacelleMotion', N_Global, .true., ErrStat3, ErrMsg3, VTK_tWidth)
      if (ErrStat3 >= AbortErrLev) return

      ! Free wake
      if (allocated(m%AD%FVW_u) .and. iWT==1) then
         if (allocated(m%AD%FVW_u(1)%WingsMesh)) then
            call WrVTK_FVW(p%AD%FVW, x(STATE_CURR)%AD%FVW, z(STATE_CURR)%AD%FVW, m%AD%FVW, trim(VTK_OutFileRoot)//'.FVW', N_Global, VTK_tWidth, bladeFrame=.FALSE.)  ! bladeFrame==.FALSE. to output in global coords
         endif
      end if
   end subroutine WrVTK_Points

   !> meshes rendered with a shape or size
   subroutine WrVTK_Surfaces(ErrStat3,ErrMsg3)
      integer(IntKi),         intent(  out)  :: ErrStat3       !< error status
      character(ErrMsgLen),   intent(  out)  :: ErrMsg3        !< error message
      logical, parameter                     :: OutputFields = .FALSE.  ! due to confusion about what fields mean on a surface, we are going to just output the basic meshes if people ask for fields
      integer(IntKi), parameter              :: numSectors   = 25       ! Number of sectors for surface utput

      ErrStat3 =  0_IntKi
      ErrMsg3  =  ''

!TODO: use this routine when it is moved out of the driver and into ADI
!      call AD_WrVTK_Surfaces(u(1)%AD, y%AD, RefPoint, m%VTK_Surfaces, N_Global, VTK_OutFileRoot, VTK_tWidth, 25, VTKHubRad)

      ! Nacelle
      if ( rot_u(iWT)%NacelleMotion%Committed ) then
         call MeshWrVTK_PointSurface (RefPoint, rot_u(iWT)%NacelleMotion, trim(VTK_OutFileRoot)//trim(sWT)//'.NacelleSurface', N_Global, &
                                      OutputFields, errStat3, errMsg3, VTK_tWidth, verts=VTK_Surface(iWT)%NacelleBox)
         if (ErrStat3 >= AbortErrLev) return
      endif

      ! Tower
      if (rot_u(iWT)%TowerMotion%Committed) then
         call MeshWrVTK_Ln2Surface (RefPoint, rot_u(iWT)%TowerMotion, trim(VTK_OutFileRoot)//trim(sWT)//'.TowerSurface', &
                                    N_Global, OutputFields, errStat3, errMsg3, VTK_tWidth, numSectors, m%VTK_Surfaces(iWT)%TowerRad )
         if (ErrStat3 >= AbortErrLev) return
      endif

      ! Hub
      if (rot_u(iWT)%HubMotion%Committed) then
         call MeshWrVTK_PointSurface (RefPoint, rot_u(iWT)%HubMotion, trim(VTK_OutFileRoot)//trim(sWT)//'.HubSurface', &
                                      N_Global, OutputFields, errStat3, errMsg3, VTK_tWidth, &
                                      NumSegments=numSectors, radius=VTKHubRad)
         if (ErrStat3 >= AbortErrLev) return
      endif

      ! Blades
      if (allocated(rot_u(iWT)%BladeMotion)) then
         do k=1,NumBlades
            if (rot_u(iWT)%BladeMotion(k)%Committed) then
               call MeshWrVTK_Ln2Surface (RefPoint, rot_u(iWT)%BladeMotion(k), trim(VTK_OutFileRoot)//trim(sWT)//'.Blade'//trim(num2lstr(k))//'Surface', &
                                          N_Global, OutputFields, errStat3, errMsg3, VTK_tWidth , verts=m%VTK_Surfaces(iWT)%BladeShape(k)%AirfoilCoords, &
                                          Sib=y%AD%rotors(iWT)%BladeLoad(k) )
                  if (ErrStat3 >= AbortErrLev) return
            endif
         enddo
      endif
   end subroutine WrVTK_Surfaces

   !> meshes rendered with lines only
   subroutine WrVTK_Lines(ErrStat3,ErrMsg3)
      integer(IntKi),         intent(  out)  :: ErrStat3       !< error status
      character(ErrMsgLen),   intent(  out)  :: ErrMsg3        !< error message
      ErrStat3 =  0_IntKi
      ErrMsg3  =  ''

      ! Tower
      if (rot_u(iWT)%TowerMotion%Committed) call MeshWrVTK(RefPoint, rot_u(iWT)%TowerMotion, trim(VTK_OutFileRoot)//trim(sWT)//'.Tower', N_Global, .true., ErrStat3, ErrMsg3, VTK_tWidth)
      if (ErrStat3 >= AbortErrLev) return

      ! Nacelle meshes
      if (rot_u(iWT)%NacelleMotion%Committed) call MeshWrVTK(RefPoint, rot_u(iWT)%NacelleMotion, trim(VTK_OutFileRoot)//trim(sWT)//'.Nacelle', N_Global, .true., ErrStat3, ErrMsg3, VTK_tWidth)
      if (ErrStat3 >= AbortErrLev) return

      ! Hub
      if (rot_u(iWT)%HubMotion%Committed) call MeshWrVTK(RefPoint, rot_u(iWT)%HubMotion, trim(VTK_OutFileRoot)//trim(sWT)//'.Hub', N_Global, .true., ErrStat3, ErrMsg3, VTK_tWidth)
      if (ErrStat3 >= AbortErrLev) return

      ! Blades
      if (allocated(rot_u(iWT)%BladeMotion)) then
         do k=1,NumBlades
            if (rot_u(iWT)%BladeMotion(k)%Committed) then
               call MeshWrVTK(RefPoint, rot_u(iWT)%BladeMotion(k), trim(VTK_OutFileRoot)//trim(sWT)//'.Blade'//trim(num2lstr(k)), N_Global, .true., ErrStat3, ErrMsg3, VTK_tWidth)
                  if (ErrStat3 >= AbortErrLev) return
            endif
         enddo
      endif
   end subroutine WrVTK_Lines
end subroutine WrVTK_Meshes

!----------------------------------------------------------------------------------------------------------------------------------
!> This routine writes the ground or seabed reference surface information in VTK format.
!! see VTK file information format for XML, here: http://www.vtk.org/wp-content/uploads/2015/04/file-formats.pdf
!! TODO: this is a duplicate of the AeroDyn_Driver_Subs.f90 routine!!!!
subroutine WrVTK_Ground (RefPoint, HalfLengths, FileRootName, errStat, errMsg)
   REAL(SiKi),      INTENT(IN)           :: RefPoint(3)     !< reference point (plane will be created around it)
   REAL(SiKi),      INTENT(IN)           :: HalfLengths(2)  !< half of the X-Y lengths of plane surrounding RefPoint
   CHARACTER(*),    INTENT(IN)           :: FileRootName    !< Name of the file to write the output in (excluding extension)
   INTEGER(IntKi),  INTENT(OUT)          :: errStat         !< Indicates whether an error occurred (see NWTC_Library)
   CHARACTER(*),    INTENT(OUT)          :: errMsg          !< Error message associated with the errStat
   ! local variables
   INTEGER(IntKi)            :: Un            ! fortran unit number
   INTEGER(IntKi)            :: ix            ! loop counters
   CHARACTER(1024)           :: FileName
   INTEGER(IntKi), parameter :: NumberOfPoints = 4
   INTEGER(IntKi), parameter :: NumberOfLines = 0
   INTEGER(IntKi), parameter :: NumberOfPolys = 1
   INTEGER(IntKi)            :: errStat2
   CHARACTER(ErrMsgLen)      :: errMsg2
   errStat = ErrID_None
   errMsg  = ""
   FileName = TRIM(FileRootName)//'.vtp'
   call WrVTK_header( FileName, NumberOfPoints, NumberOfLines, NumberOfPolys, Un, errStat2, errMsg2 )    
   call SetErrStat(errStat2,errMsg2,errStat,errMsg,'WrVTK_Ground'); if (errStat >= AbortErrLev) return
   WRITE(Un,'(A)')         '      <Points>'
   WRITE(Un,'(A)')         '        <DataArray type="Float32" NumberOfComponents="3" format="ascii">'
   WRITE(Un,VTK_AryFmt) RefPoint(1) + HalfLengths(1) , RefPoint(2) + HalfLengths(2), RefPoint(3)
   WRITE(Un,VTK_AryFmt) RefPoint(1) + HalfLengths(1) , RefPoint(2) - HalfLengths(2), RefPoint(3)
   WRITE(Un,VTK_AryFmt) RefPoint(1) - HalfLengths(1) , RefPoint(2) - HalfLengths(2), RefPoint(3)
   WRITE(Un,VTK_AryFmt) RefPoint(1) - HalfLengths(1) , RefPoint(2) + HalfLengths(2), RefPoint(3)
   WRITE(Un,'(A)')         '        </DataArray>'
   WRITE(Un,'(A)')         '      </Points>'
   WRITE(Un,'(A)')         '      <Polys>'      
   WRITE(Un,'(A)')         '        <DataArray type="Int32" Name="connectivity" format="ascii">'         
   WRITE(Un,'('//trim(num2lstr(NumberOfPoints))//'(i7))') (ix, ix=0,NumberOfPoints-1)                   
   WRITE(Un,'(A)')         '        </DataArray>'      
   
   WRITE(Un,'(A)')         '        <DataArray type="Int32" Name="offsets" format="ascii">'            
   WRITE(Un,'(i7)') NumberOfPoints
   WRITE(Un,'(A)')         '        </DataArray>'
   WRITE(Un,'(A)')         '      </Polys>'      
   call WrVTK_footer( Un )       
end subroutine WrVTK_Ground



END MODULE AeroDyn_Inflow_C_BINDING
