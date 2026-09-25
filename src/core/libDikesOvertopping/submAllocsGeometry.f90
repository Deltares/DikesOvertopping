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

submodule (geometryModuleOvertopping) submAllocGeometry
   use OvertoppingMessages
contains

module subroutine copyCoordinates(coordIn, coordOut)
    type(tpCoordinatePair), intent(in   ) :: coordIn
    type(tpCoordinatePair), intent(inout) :: coordOut

    integer :: i
    integer :: length

    length = min(size(coordIn%x), size(coordOut%x))
    do i = 1, length
        coordOut%x(i) = coordIn%x(i)
        coordOut%y(i) = coordIn%y(i)
    end do
end subroutine copyCoordinates

module subroutine allocCoordinatePair(xy, n, ierr)
   type(tpCoordinatePair), intent(inout) :: xy
   integer               , intent(in   ) :: n
   integer               , intent(  out) :: ierr

                  allocate(xy%x(n), stat=ierr)
   if (ierr == 0) allocate(xy%y(n), stat=ierr)
end subroutine allocCoordinatePair

module subroutine cleanupCoordinatePair(xy)
   type(tpCoordinatePair), intent(inout) :: xy

   if (allocated(xy%x)) deallocate(xy%x)
   if (allocated(xy%y)) deallocate(xy%y)
end subroutine cleanupCoordinatePair

!> deallocateGeometry:
!! deallocate the geometry vectors
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module subroutine deallocateGeometry(geometry)
    type (tpGeometry), intent(inout) :: geometry     !< structure with geometry data
!***********************************************************************************************************
    call cleanupCoordinatePair(geometry%Coordinates)
    call cleanupCoordinatePair(geometry%CoordDiff)
    if (allocated(geometry%roughnessFactors)) deallocate(geometry%roughnessFactors)
    if (allocated(geometry%segmentSlopes))    deallocate(geometry%segmentSlopes)
    if (allocated(geometry%segmentTypes))     deallocate(geometry%segmentTypes)

end subroutine deallocateGeometry

end submodule submAllocGeometry
