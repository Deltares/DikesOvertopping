!> @file
!! This file contains a module with the type definitions for Dikes Overtopping
!<
!***********************************************************************************************************
!
!  Programmer: Bastiaan Kuijper, HKV consultants
!
!  Copyright (c) 2015, Deltares, HKV lijn in water, TNO
!  $Id$
!
!***********************************************************************************************************
   module typeDefinitionsRTOovertopping
!***********************************************************************************************************
   !
   use precision 
   
   !> tpGeometry: structure with geometry data
   type, public              :: tpGeometry
      real(wp)               :: psi                     !< dike normal (degrees)
      integer                :: nCoordinates            !< number of coordinates cross section 
      real(wp),    pointer   :: xCoordinates(:)         !< vector with x-coordinates cross section (m)
      real(wp),    pointer   :: yCoordinates(:)         !< vector with y-coordinates cross section (m+NAP)
      real(wp),    pointer   :: roughnessFactors(:)     !< vector with roughness factors cross section
      real(wp),    pointer   :: xCoordDiff(:)           !< vector with differences in x-coordinates (m)
      real(wp),    pointer   :: yCoordDiff(:)           !< vector with differences in y-coordinates (m)
      real(wp),    pointer   :: segmentSlopes(:)        !< vector with slopes dike segments
      integer, pointer       :: segmentTypes(:)         !< vector with segment types (1=slope,2=berm,3=other)
      integer                :: NbermSegments           !< number of berm segments
   end type tpGeometry

   !> tpLoad: structure with load parameters
   type, public              :: tpLoad                  
      real(wp)               :: h                       !< local water level (m+NAP)
      real(wp)               :: Hm0                     !< significant wave height (m)
      real(wp)               :: Tm_10                   !< spectral wave period (s)
      real(wp)               :: phi                     !< wave direction (degrees)
    end type tpLoad
   
   !> OvertoppingModelFactors: C-structure with model factors
   type, public, bind(C) :: tpOvertoppingInput
      real(kind=wp) :: factorDeterminationQ_b_f_n         !< model factor for non-breaking waves
      real(kind=wp) :: factorDeterminationQ_b_f_b         !< model factor for breaking waves
      real(kind=wp) :: m_z2                               !< model factor describing the uncertainty of 2% runup height
      real(kind=wp) :: frunup1                            !< model factor 1 for wave run-up  (for backwards compatability)
      real(kind=wp) :: frunup2                            !< model factor 2 for wave run-up  (idem)
      real(kind=wp) :: frunup3                            !< model factor 3 for wave run-up  (idem)
      real(kind=wp) :: fshallow                           !< model factor for shallow waves
      real(kind=wp) :: ComputedOvertopping                !< model factor computed overtopping
      real(kind=wp) :: CriticalOvertopping                !< model factor critical overtopping
      integer       :: typeRunup                          !< 0: fRunup1, 2 and 3 are given; 1: m_z2 is given
      real(kind=wp) :: relaxationFactor                   !< relaxation factor iteration procedure wave runup
      real(kind=wp) :: reductionFactorForeshore = 0.5_wp  !< reduction factor foreshore
   end type tpOvertoppingInput

   !> tpOvertopping: structure with overtopping results
   type, public             :: tpOvertopping
      real(wp)              :: z2                      !< 2% wave run-up (m)
      real(wp)              :: Qo                      !< wave overtopping discharge (m3/m per s)
   end type

   ! -------------------------------------
   !  Public parameters
   ! -------------------------------------
   real(wp),  parameter      :: xDiff_min     = 2.0d-2            !< minimal value distance between x-coordinates (m)
   real(wp),  parameter      :: marginDiff    = 1.0d-14           !< margin for minimal distance (m)
   real(wp),  parameter      :: berm_min      = 0.0d0             !< minimal value gradient berm segment
   real(wp),  parameter      :: berm_max      = 1.0d0/15          !< maximal value gradient berm segment
   real(wp),  parameter      :: slope_min     = 1.0d0/8           !< minimal value gradient slope segment
   real(wp),  parameter      :: slope_max     = 1.0d0             !< maximal value gradient slope segment
   real(wp),  parameter      :: marginGrad    = 0.0025d0          !< margin for minimal and maximal gradients
   real(wp),  parameter      :: rFactor_min   = 0.5d0             !< minimal value roughness factor dike segments
   real(wp),  parameter      :: rFactor_max   = 1.0d0             !< maximal value roughness factor dike segments
   real(wp),  parameter      :: mz2_min       = 0.0d0             !< minimal value model factor of 2% runup height
   real(wp),  parameter      :: mz2_max       = huge(mz2_max)     !< maximal value model factor of 2% runup height
   real(wp),  parameter      :: fRunup1_min   = 0.0d0             !< minimal value model factor 1 for wave run-up
   real(wp),  parameter      :: fRunup1_max   = huge(fRunup1_max) !< maximal value model factor 1 for wave run-up
   real(wp),  parameter      :: fRunup2_min   = 0.0d0             !< minimal value model factor 2 for wave run-up
   real(wp),  parameter      :: fRunup2_max   = huge(fRunup2_max) !< maximal value model factor 2 for wave run-up
   real(wp),  parameter      :: fRunup3_min   = 0.0d0             !< minimal value model factor 3 for wave run-up
   real(wp),  parameter      :: fRunup3_max   = huge(fRunup3_max) !< maximal value model factor 3 for wave run-up
   real(wp),  parameter      :: fB_min        = 0.0d0             !< minimal value model factor for breaking waves
   real(wp),  parameter      :: fB_max        = huge(fB_max)      !< maximal value model factor for breaking waves
   real(wp),  parameter      :: fN_min        = 0.0d0             !< minimal value model factor for non-breaking waves
   real(wp),  parameter      :: fN_max        = huge(fN_max)      !< maximal value model factor for non-breaking waves
   real(wp),  parameter      :: fS_min        = 0.0d0             !< minimal value model factor for shallow waves
   real(wp),  parameter      :: fS_max        = huge(fS_max)      !< maximal value model factor for shallow waves
   real(wp),  parameter      :: foreshore_min = 0.3d0             !< minimal value reduction factor foreshore
   real(wp),  parameter      :: foreshore_max = 1.0d0             !< maximal value reduction factor foreshore
   integer,   parameter      :: z2_iter_max   = 100               !< maximal number of iterations for calculation z2
   real(wp),  parameter      :: z2_margin     = 0.001d0           !< margin for convergence criterium calculation z2

!***********************************************************************************************************
   end module typeDefinitionsRTOovertopping
!***********************************************************************************************************
