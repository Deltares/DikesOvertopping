! Copyright (C) Stichting Deltares and State of the Netherlands 2025. All rights reserved.
!
! This file is part of the Dikes Overtopping Kernel.
!
! The Dikes Overtopping Kernel is free software: you can redistribute it and/or modify
! it under the terms of the GNU Affero General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
! GNU Affero General Public License for more details.
!
! You should have received a copy of the GNU Affero General Public License
! along with this program. If not, see <http://www.gnu.org/licenses/>.
!
! All names, logos, and references to "Deltares" are registered trademarks of
! Stichting Deltares and remain full property of Stichting Deltares at all times.
! All rights reserved.
!

submodule (formulaModuleOvertopping) submCalcWaveRunup
   use parametersOvertopping
contains

!> calculateWaveRunup:
!! calculate wave runup
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module subroutine calculateWaveRunup(Hm0, ksi0, ksi0Limit, gamma, modelFactors, z2, error)
   implicit none
   real(kind=wp),             intent(in)     :: Hm0            !< significant wave height (m)
   real(kind=wp),             intent(in)     :: ksi0           !< breaker parameter
   real(kind=wp),             intent(in)     :: ksi0Limit      !< limit value breaker parameter
   type(tpInfluencefactors),  intent(inout)  :: gamma          !< influence factors
   type (tpOvertoppingInput), intent(in)     :: modelFactors   !< structure with model factors
   real(kind=wp),             intent(out)    :: z2             !< 2% wave run-up (m)
   type(tMessage),            intent(inout)  :: error          !< error struct
!***********************************************************************************************************

   ! if applicable adjust influence factors
   call adjustInfluenceFactors (gamma, 1, ksi0, ksi0Limit, error)

   if (error%errorCode == 0) then

      ! calculate 2% wave run-up for small breaker parameters
      if (ksi0 < ksi0Limit) then
         z2 = Hm0 * fRunup1 * gamma%gammaB * gamma%gammaF * gamma%gammaBeta * ksi0

      ! calculate 2% wave run-up for large breaker parameters
      else
         z2 = Hm0 * gamma%gammaF * gamma%gammaBeta * (fRunup2 - fRunup3/sqrt(ksi0))
         z2 = max(z2, 0.0d0)
      endif
      z2 = z2 * modelFactors%m_z2

   endif

end subroutine calculateWaveRunup

end submodule submCalcWaveRunup
