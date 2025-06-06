! Copyright (C) Stichting Deltares 2019. All rights reserved.
!
! This file is part of the Hydra Ring Application.
!
! The Hydra Ring Application is free software: you can redistribute it and/or modify
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

!> @file
!! Module for retrieving file version info for a .dll or .exe 
!<
!
!!   @ingroup General
module versionInfo

    implicit none

    private

    public :: getFileVersion

contains

!> Get file version info
!!
!! @ingroup General
function getFileVersion() result(version)
    character(len=20) :: version   !< The resulting file version

    version = "25.1.1.0"

end function getFileVersion

end module versionInfo

