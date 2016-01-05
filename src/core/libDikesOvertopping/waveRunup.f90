!> @file
!! This file contains a module with the core computations for Dikes Overtopping
!<
!***********************************************************************************************************
!
!  Programmer: Bastiaan Kuijper, HKV consultants
!
!  Copyright (c) 2015, Deltares, HKV lijn in water, TNO
!  $Id$
!
!***********************************************************************************************************
   module waveRunup
!***********************************************************************************************************

   use factorModuleRTOovertopping
   use typeDefinitionsRTOovertopping
   use formulaModuleRTOovertopping
   use geometryModuleRTOovertopping

   implicit none

   private
   
   public :: iterationWaveRunup

   contains

!> iterationWaveRunup:
!! iteration for the wave runup
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine iterationWaveRunup (geometry, h, Hm0, Tm_10, gammaBeta_z, modelFactors, z2, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),         intent(in)     :: geometry       !< structure with geometry data
   real(wp),                  intent(in)     :: h              !< local water level (m+NAP)
   real(wp),                  intent(in)     :: Hm0            !< significant wave height (m)
   real(wp),                  intent(in)     :: Tm_10          !< spectral wave period (s)
   real(wp),                  intent(inout)  :: gammaBeta_z    !< influence factor angle wave attack 2% run-up
   type (tpOvertoppingInput), intent(in)     :: modelFactors   !< structure with model factors
   real(wp),                  intent(out)    :: z2             !< 2% wave run-up (m)
   logical,                   intent(out)    :: succes         !< flag for succes
   character(len=*),          intent(out)    :: errorMessage   !< error message
!
!  Local parameters
!
   type (tpGeometry) :: geometryFlatBerms       !< structure with geometry data with horizontal berms
   real(kind=wp)     :: s0                      !< wave steepness
   integer           :: i                       !< counter iteration steps
   real(kind=wp)     :: z2_start  (z2_iter_max) !< starting value 2% wave run-up for each iteration step
   real(kind=wp)     :: tanAlpha  (z2_iter_max) !< representative slope angle    for each iteration step
   real(kind=wp)     :: ksi0      (z2_iter_max) !< breaker parameter             for each iteration step
   real(kind=wp)     :: ksi0Limit (z2_iter_max) !< limit value breaker parameter for each iteration step
   real(kind=wp)     :: z2_smooth (z2_iter_max) !< 2% wave run-up (no roughness) for each iteration step
   real(kind=wp)     :: gammaF    (z2_iter_max) !< influence factor roughness    for each iteration step
   real(kind=wp)     :: z2_rough  (z2_iter_max) !< 2% wave run-up (no berms)     for each iteration step
   real(kind=wp)     :: gammaB    (z2_iter_max) !< influence factor berms        for each iteration step
   real(kind=wp)     :: z2_end    (z2_iter_max) !< ending value 2% wave run-up   for each iteration step
   integer           :: Niterations             !< number of iterations before convergence
   logical           :: convergence             !< flag for convergence iteration procedure
   
! ==========================================================================================================

   ! calculate wave steepness
   call calculateWaveSteepness (Hm0, Tm_10, s0, succes, errorMessage)
   
   ! if applicable adjust non-horizontal berms
   if (succes) then
      call adjustNonHorizontalBerms (geometry, geometryFlatBerms, succes, errorMessage)
   endif

   ! initialize influence factor berms and roughness (for each iteration step)
   gammaB     = 1.0d0
   gammaF     = 1.0d0

   ! initialize number of iterations and flag for convergence
   Niterations = 0
   convergence = .false.

   ! -------------------------
   ! start iteration procedure
   ! -------------------------

   ! iteration procedure for calculating 2% wave run-up
   do i=1, z2_iter_max

      ! edit number of iterations
      Niterations = Niterations + 1
      
      z2_start(i) = determineStartingValue(i, modelFactors%relaxationFactor, z2_start, z2_end, Hm0)

      ! calculate representative slope angle
      if (succes) then
         if (z2_start(i) > 0.0d0) then
            call calculateTanAlpha (h, Hm0, z2_start(i), geometryFlatBerms, tanAlpha(i), &
                                    succes, errorMessage)
         endif
      endif

      ! calculate breaker parameter
      if (succes) then
         if (z2_start(i) > 0.0d0) then
            call calculateBreakerParameter (tanAlpha(i), s0, ksi0(i), succes, errorMessage)
         endif
      endif

      ! calculate limit value breaker parameter
      if (succes) then
         if (z2_start(i) > 0.0d0) then
            call calculateBreakerLimit (modelFactors, gammaB(i), ksi0Limit(i), succes, errorMessage)
         endif
      endif

      ! calculate z2% smooth (gammaB=1, gammaF=1)
      if (succes) then
         if (z2_start(i) > 0.0d0) then
            call calculateWaveRunup (Hm0, ksi0(i), ksi0Limit(i), gammaB(i), gammaF(i), &
                                     gammaBeta_z, modelFactors, z2_smooth(i), succes, errorMessage)
         else
            z2_smooth(i) = 0.0d0
         endif
      endif

      ! calculate influence factor roughness (gammaF)
      if (succes) then
         if (z2_smooth(i) > 0.0d0) then
            call calculateGammaF (h, ksi0(i), ksi0Limit(i), gammaB(i), z2_smooth(i), geometry, &
                                  gammaF(i), succes, errorMessage)
         endif
      endif

      ! calculate z2% rough (gammaB=1)
      if (succes) then
         if (z2_smooth(i) > 0.0d0) then
            call calculateWaveRunup (Hm0, ksi0(i), ksi0Limit(i), gammaB(i), gammaF(i), &
                                     gammaBeta_z, modelFactors, z2_rough(i), succes, errorMessage)
         else
            z2_rough(i) = 0.0d0
         endif
      endif

      ! if cross section contains one or more berms
      if (geometry%NbermSegments > 0) then
      
         ! calculate influence factor berms (gammaB)
         if (succes) then
            if (z2_rough(i) > 0.0d0) then
               call calculateGammaB (h, Hm0, z2_rough(i), geometryFlatBerms, &
                                     geometryFlatBerms%NbermSegments, gammaB(i), succes, errorMessage)
            endif
         endif

         ! calculate limit value breaker parameter
         if (succes) then
            if (z2_rough(i) > 0.0d0) then
               call calculateBreakerLimit (modelFactors, gammaB(i), ksi0Limit(i), succes, errorMessage)
            endif
         endif

         ! calculate influence factor roughness (gammaF)
         if (succes) then
            if (z2_rough(i) > 0.0d0) then
               call calculateGammaF (h, ksi0(i), ksi0Limit(i), gammaB(i), z2_rough(i), geometry, &
                                     gammaF(i), succes, errorMessage)
            endif
         endif
   
         ! calculate z2%
         if (succes) then
            if (z2_rough(i) > 0.0d0) then
               call calculateWaveRunup (Hm0, ksi0(i), ksi0Limit(i), gammaB(i), gammaF(i), &
                                        gammaBeta_z, modelFactors, z2_end(i), succes, errorMessage)
            else
               z2_end(i) = 0.0d0
            endif
         endif
      
      else

         ! no berms present
         z2_end(i) = z2_rough(i)

      endif

      ! calculate convergence criterium
      convergence = (abs(z2_start(i) - z2_end(i)) < z2_margin)

      ! exit loop when an error occurs or convergence is reached
      if ((.not. succes) .or. (convergence)) then
          exit
      endif
      
      if (convergedWithResidu(i, z2_start, z2_end)) then
          convergence = .true.
          exit
      endif

   enddo

   call deallocateGeometry( geometryFlatBerms )

   ! -----------------------
   ! end iteration procedure
   ! -----------------------

   ! check if iteration procedure converged
   if (succes) then
      
      if (convergence) then
     
         ! when convergence is reached 2% wave run-up equals value in last iteration step
         z2 = z2_end(Niterations)
   
      else

         ! determine error message when no convergence is reached
         succes = .false.
         errorMessage = 'no convergence in iteration procedure 2% wave run-up'

      endif

   endif

   end subroutine iterationWaveRunup

   function determineStartingValue(i, relaxationFactor, z2_start, z2_end, Hm0) result (startValue)
   integer, intent(in)        :: i
   real(kind=wp), intent(in)  :: relaxationFactor, z2_start(:), z2_end(:), Hm0
   real(kind=wp)              :: startValue
   
   integer       :: k
   real(kind=wp) :: relaxation

   ! determine starting value 2% wave run-up for this iteration step
   if (i == 1) then
      startValue = 1.5d0*Hm0
   else if (i <= 5) then
      startValue = z2_end(i-1)
   else if (i >= 50) then
      !
      ! emulate numerical integration
      !
      k = findSmallestResidu(z2_start(1:49), z2_end(1:49))
      startValue = z2_start(k) + 2.0_wp * z2_margin * (real(i,wp)-60.25_wp)
   else
      if (i <= 25) then 
          relaxation = relaxationFactor
      else
          relaxation = min(0.5_wp, relaxationFactor)
      endif
      startValue = z2_end(i-1) * relaxation + ( 1.0_wp - relaxation) * z2_start(i-1)
   endif
   end function determineStartingValue

   integer function findSmallestResidu(z2_start, z2_end)
   real(kind=wp), intent(in)  :: z2_start(:), z2_end(:)
   !
   ! locals
   !
   real(kind=wp), allocatable :: residu(:)
   integer :: j, k, maxi
   real(kind=wp) :: min_res
   
   maxi = size(z2_start)
   allocate(residu(maxi))
   min_res = huge(min_res)
   do j = 1, maxi
       residu(j) = z2_end(j) - z2_start(j)
       if (abs(residu(j)) < min_res) then
           k = j
           min_res = abs(residu(j))
       endif
   enddo
   deallocate(residu)
   findSmallestResidu = k

   end function findSmallestResidu

   logical function convergedWithResidu(i, z2_start, z2_end)
   use utilities
   use ModuleLogging
   integer, intent(in) :: i
   real(kind=wp), intent(in) :: z2_start(:)
   real(kind=wp), intent(inout) :: z2_end(:)
   
   integer :: iunit, k
   
   if (i > 80) then
       k = findSmallestResidu(z2_start(1:i), z2_end(1:i))
       z2_end(i) = (z2_end(k) + z2_start(k)) * 0.5_wp
       if (currentLogging%fileName /= ' ') then
           call getFreeLuNumber(iunit)
           open(iunit, file = currentLogging%fileName, position = 'append')
           write(iunit,'(a,f8.4)') 'residu in 2% wave run-up:', abs(z2_start(k) - z2_end(k))
           close(iunit)
       endif
       convergedWithResidu = .true.
   else
       convergedWithResidu = .false.
   endif

   end function convergedWithResidu

!***********************************************************************************************************
   end module waveRunup
!***********************************************************************************************************
