!> @file
!! This file contains the parameters and types (structs)
!! as part of the interface to and from dllOvertopping
!<
!
! Copyright (c) 2016, Deltares, HKV lijn in water, TNO
! $Id$
!
!>
!! Module for the interface of dllOvertopping
!! @ingroup LibOvertopping
!<
module overtoppingInterface
    use precision, only : wp
    
    integer, parameter :: varModelFactorCriticalOvertopping =   8   !< Model factor critical overtopping

    type :: tpProfileCoordinate
        real(kind=wp)               :: xCoordinate                  !< X-coordinate foreland profile
        real(kind=wp)               :: zCoordinate                  !< Z-coordinate foreland profile
        real(kind=wp)               :: roughness                    !< Roughness of the area between two points
    end type
    
    type :: OvertoppingGeometryTypeF
        real(kind=wp)          :: normal
        integer                :: nPoints
        real(kind=wp), pointer :: xCoords(:)
        real(kind=wp), pointer :: yCoords(:)
        real(kind=wp), pointer :: roughness(:)
    end type OvertoppingGeometryTypeF

    private
    
    public :: tpProfileCoordinate, OvertoppingGeometryTypeF, varModelFactorCriticalOvertopping

end module overtoppingInterface
