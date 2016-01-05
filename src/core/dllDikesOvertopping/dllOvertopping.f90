!> @file dllOvertopping.f90
!!  Main entry for the dll DikesOvertopping
!!  FUNCTIONS/SUBROUTINES exported from dllOvertopping.dll:
!!  - zFuncOvertopping
!!  - calculateQo
!!  - calculateQoF
!!  - versionNumber
!
! Copyright (c) 2015, Deltares, HKV lijn in water, TNO
! $Id$
!
!
!> Calculate one type of overtopping
!
module dllOvertopping
    use zFunctionsWTIOvertopping,      only : calculateQoRTO, zFuncLogRatios
    use geometryModuleRTOovertopping,  only : deallocateGeometry
    use precision,                     only : wp
    use typeDefinitionsRTOovertopping, only : tpGeometry, tpLoad, OvertoppingModelFactors
    use overtoppingInterface,          only : OvertoppingGeometryType, OvertoppingGeometryTypeF
    use, intrinsic :: iso_c_binding

    implicit none

    private

    !  FUNCTIONS/SUBROUTINES exported from dllOvertoppping.dll:
    public :: calculateQo, calculateQoF, calcZValue, versionNumber
    
contains

!>
!! Subroutine that calculates the discharge needed for the Z-function DikesOvertopping
!! Wrapper for calculateQoF: covert C-like input structures to Fortran input structures
!!
!! @ingroup LSF
subroutine calculateQo ( load, geometryInput, dikeHeight, modelFactors, overtopping, success, errorText)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"calculateQo" :: calculateQo
    use geometryModuleRTOovertopping
    use typeDefinitionsRTOovertopping
    type(OvertoppingGeometryType), intent(in) :: geometryInput  !< struct with geometry and roughness as c-pointers
    type(tpLoad), intent(in)                  :: load           !< struct with waterlevel and wave parameters
    real(kind=wp), intent(in)                 :: dikeHeight     !< dikeHeight
    type(OvertoppingModelFactors), intent(in) :: modelFactors   !< struct with modelfactors
    type (tpOvertopping), intent(out)         :: overtopping    !< structure with overtopping results
    logical, intent(out)                      :: success        !< flag for success
    character(len=*), intent(out)             :: errorText      !< error message (only set if not successful)

    integer                                   :: n(1)           !< dimension definition in c_f_pointer call
    type(OvertoppingGeometryTypeF)            :: geometry       !< fortran struct with geometru and roughness

    n(1) = geometryInput%nPoints

    call c_f_pointer(geometryInput%xCoords, geometry%xcoords, n)
    call c_f_pointer(geometryInput%ycoords, geometry%ycoords, n)
    n(1) = geometryInput%nPoints - 1
    call c_f_pointer(geometryInput%roughness, geometry%roughness, n)
    geometry%npoints = geometryInput%nPoints
    geometry%normal = geometryInput%normal

    call calculateQoF ( load, geometry, dikeHeight, modelFactors, overtopping, success, errorText)

end subroutine calculateQo

!>
!! Subroutine that calculates the discharge needed for the Z-function DikesOvertopping
!!
!! @ingroup LSF
subroutine calculateQoF ( load, geometryF, dikeHeight, modelFactors, overtopping, success, errorText)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"calculateQoF" :: calculateQoF
    use geometryModuleRTOovertopping
    use typeDefinitionsRTOovertopping
    type(OvertoppingGeometryTypeF), intent(in) :: geometryF      !< struct with geometry and roughness
    type(tpLoad), intent(in)                   :: load           !< struct with waterlevel and wave parameters
    real(kind=wp), intent(in)                  :: dikeHeight     !< dikeHeight
    type(OvertoppingModelFactors), intent(in)  :: modelFactors   !< struct with modelFactors
    type (tpOvertopping), intent(out)          :: overtopping    !< structure with overtopping results
    logical, intent(out)                       :: success        !< flag for success
    character(len=*), intent(out)              :: errorText      !< error message (only set if not successful)
!
    type (tpGeometry)                          :: geometry       !< structure with geometry data
!
    call initializeGeometry (geometryF%normal, geometryF%npoints, geometryF%xcoords, geometryF%ycoords, geometryF%roughness, geometry, success, errorText)

    if (success) then
        call calculateQoRTO ( dikeHeight, modelFactors, overtopping, load, geometry, success, errorText )
    endif

    call deallocateGeometry( geometry )
end subroutine calculateQoF

!>
!! Subroutine that calculates the Z-function DikesOvertopping
!! based on the discharge calculated with calculateQoF
!!
!! @ingroup LSF
subroutine calcZValue ( criticalOvertoppingRate, modelFactors, Qo, z, success, errorMessage)
! use alias, otherwise the name will be dllOvertopping_mp_calcZValue
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"calcZValue" :: calcZValue
    real(kind=wp), intent(in)     :: criticalOvertoppingRate    !< critical overtoppingrate
    type(OvertoppingModelFactors), intent(in)  :: modelFactors  !< struct with modelfactors
    real(kind=wp), intent(in)     :: Qo                         !< calculated discharge
    real(kind=wp), intent(out)    :: z                          !< z value
    character(len=*), intent(out) :: errorMessage               !< error message (only if not successful)
    logical, intent(out)          :: success                    !< flag for success

    !==============================================================================
    z = zFuncLogRatios(qo, criticalOvertoppingRate, modelFactors%ComputedOvertopping, modelFactors%CriticalOvertopping, success, errorMessage)

end subroutine calcZValue

subroutine versionNumber(version)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"versionNumber" :: versionNumber
    character(len=*), intent(out) :: version !< version number
    !
    ! locals
    !
    character(len=*), parameter :: cversion = "15.1.0.0"
    !
    !==============================================================================
    !
    if (len(version) >= len(cversion)) then
        version = cversion
    else
        version = repeat("*", len(version))
    endif

end subroutine versionNumber

end module dllOvertopping