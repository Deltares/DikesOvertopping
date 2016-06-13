!> @file dllOvertopping.f90
!!  Main entry for the dll DikesOvertopping
!!  FUNCTIONS/SUBROUTINES exported from dllOvertopping.dll:
!!  - calcZValue
!!  - calculateQoF
!!  - ValidateInputF
!!  - omkeerVariantF
!!  - SetLanguage
!!  - GetLanguage
!!  - versionNumber
!
! Copyright (c) 2016, Deltares, HKV lijn in water, TNO
! $Id$
!
!
!> Main entry for the dll DikesOvertopping
!
module dllOvertopping
    use zFunctionsWTIOvertopping,      only : calculateQoRTO, zFuncLogRatios
    use geometryModuleRTOovertopping,  only : deallocateGeometry
    use precision,                     only : wp
    use typeDefinitionsRTOovertopping, only : tpGeometry, tpLoad, tpOvertoppingInput
    use overtoppingInterface,          only : OvertoppingGeometryTypeF

    implicit none

    private

    !  FUNCTIONS/SUBROUTINES exported from dllOvertoppping.dll:
    public :: calculateQoF, calcZValue, versionNumber, ValidateInputF, &
              omkeerVariantF, setLanguage, getLanguage

contains

!>
!! Subroutine that calculates the discharge needed for the Z-function DikesOvertopping
!!
!! @ingroup dllDikesOvertopping
subroutine calculateQoF(load, geometryF, dikeHeight, modelFactors, overtopping, success, errorText, logging)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"calculateQoF" :: calculateQoF
    use geometryModuleRTOovertopping
    use typeDefinitionsRTOovertopping
    use ModuleLogging
    type(OvertoppingGeometryTypeF), intent(in) :: geometryF      !< struct with geometry and roughness
    type(tpLoad), intent(in)                   :: load           !< struct with waterlevel and wave parameters
    real(kind=wp), intent(in)                  :: dikeHeight     !< dike height
    type(tpOvertoppingInput), intent(inout)    :: modelFactors   !< struct with modelFactors
    type (tpOvertopping), intent(out)          :: overtopping    !< structure with overtopping results
    logical, intent(out)                       :: success        !< flag for success
    character(len=*), intent(out)              :: errorText      !< error message (only set if not successful)
    type(tLogging), intent(in)                 :: logging        !< logging struct
!
    type (tpGeometry)                          :: geometry       !< structure with geometry data
!
    currentLogging = logging

    call initializeGeometry(geometryF%normal, geometryF%npoints, geometryF%xcoords, geometryF%ycoords, &
                             geometryF%roughness, geometry, success, errorText)

    if (success) then
        call calculateQoRTO(dikeHeight, modelFactors, overtopping, load, geometry, success, errorText)
    endif

    call deallocateGeometry(geometry)
end subroutine calculateQoF

!>
!! Subroutine that calculates the Z-function DikesOvertopping
!! based on the discharge calculated with calculateQoF
!!
!! @ingroup dllDikesOvertopping
subroutine calcZValue(criticalOvertoppingRate, modelFactors, Qo, z, success, errorMessage)
! use alias, otherwise the name will be dllOvertopping_mp_calcZValue
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"calcZValue" :: calcZValue
    real(kind=wp), intent(in)               :: criticalOvertoppingRate    !< critical overtoppingrate
    type(tpOvertoppingInput), intent(inout) :: modelFactors               !< struct with modelfactors
    real(kind=wp), intent(in)               :: Qo                         !< calculated discharge
    real(kind=wp), intent(out)              :: z                          !< z value
    character(len=*), intent(out)           :: errorMessage               !< error message (only if not successful)
    logical, intent(out)                    :: success                    !< flag for success

    !==============================================================================
    z = zFuncLogRatios(qo, criticalOvertoppingRate, modelFactors%ComputedOvertopping, &
                       modelFactors%CriticalOvertopping, success, errorMessage)

end subroutine calcZValue

!>
!! Subroutine that validates the geometry
!!
!! @ingroup dllDikesOvertopping
subroutine ValidateInputF(geometryF, dikeHeight, modelFactors, errorStruct)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"ValidateInputF" :: ValidateInputF
    use geometryModuleRTOovertopping
    use typeDefinitionsRTOovertopping
    use zFunctionsWTIOvertopping
    use mainModuleRTOovertopping, only : checkModelFactors
    use errorMessages
    type(OvertoppingGeometryTypeF), intent(in) :: geometryF           !< struct with geometry and roughness
    real(kind=wp), intent(in)                  :: dikeHeight          !< dike height
    type(tpOvertoppingInput), intent(inout)    :: modelFactors        !< struct with modelFactors
    type(TErrorMessages), intent(inout)        :: errorStruct         !< error message (only set if not successful)
!
!   locals
!
    type (tpGeometry)                          :: geometry            !< structure with geometry data
    integer                                    :: nrCoordsAdjusted    !< number of coordinates of the adjusted profile
    real(kind=wp), pointer                     :: xCoordsAdjusted(:)  !< vector with x-coordinates of the adjusted profile
    real(kind=wp), pointer                     :: zCoordsAdjusted(:)  !< vector with y-coordinates of the adjusted profile
    type (tpGeometry)                          :: geometryAdjusted    !< structure for the adjusted profile
    character(len=StrLenMessages)              :: errorText           !< local error or validation message
    integer, parameter                         :: maxErr = 32         !< max. number of validation messages
    character(len=StrLenMessages)              :: errorTexts(maxErr)  !< local error or validation messages
    logical                                    :: success             !< local error flag
    integer                                    :: ierr                !< actual number of validation messages
    type (tMessage)                            :: msgStruct           !< struct for one local error or validation message
    integer                                    :: i                   !< loop counter
!
    success = .true.
    errorText = ' '

    nullify(xCoordsAdjusted)
    nullify(zCoordsAdjusted)

    if (success) then
        call basicGeometryTest(geometryF, success, errorStruct)
    endif

    if (success) then
        call initializeGeometry (geometryF%normal, geometryF%npoints, geometryF%xcoords, geometryF%ycoords, &
                             geometryF%roughness, geometry, success, errorText)
    endif

    if (success) then

        call profileInStructure(geometry%nCoordinates, geometry%xcoordinates, geometry%ycoordinates, dikeHeight, &
                            nrCoordsAdjusted, xCoordsAdjusted, zCoordsAdjusted, success, errorText)
    endif

    if (success) then
        call initializeGeometry(geometry%psi, nrCoordsAdjusted, xCoordsAdjusted, zCoordsAdjusted, &
                                geometry%roughnessFactors, geometryAdjusted, success, errorText)
        call deallocateGeometry(geometryAdjusted)
        call deallocateGeometry(geometry)
    endif

    if (.not. success) then
        if (errorText /= ' ') then
            msgStruct%errorCode = 1
            msgStruct%severity  = severityError
            msgStruct%message   = errorText
            call addMessage(errorStruct, msgStruct)
        endif
    endif

    call checkModelFactors (modelFactors, maxErr, errorTexts, ierr)
    do i = 1, ierr
       msgStruct%errorCode = 2
       msgStruct%severity  = severityError
       msgStruct%message   = errorTexts(i)
       call addMessage(errorStruct, msgStruct)
    enddo

    if (associated(xCoordsAdjusted)) deallocate(xCoordsAdjusted)
    if (associated(zCoordsAdjusted)) deallocate(zCoordsAdjusted)
end subroutine ValidateInputF

!>
!! Subroutine with omkeerVariant
!!
!! @ingroup dllDikesOvertopping
subroutine omkeerVariantF(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, success, errorText, logging)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"omkeerVariantF" :: omkeerVariantF
    use geometryModuleRTOovertopping
    use typeDefinitionsRTOovertopping
    use ModuleLogging
    use omkeerVariantModule
    type(OvertoppingGeometryTypeF), intent(in) :: geometryF      !< struct with geometry and roughness
    type(tpLoad), intent(in)                   :: load           !< struct with waterlevel and wave parameters
    real(kind=wp), intent(in)                  :: givenDischarge !< discharge to iterate to
    real(kind=wp), intent(out)                 :: dikeHeight     !< dike height
    type(tpOvertoppingInput), intent(inout)    :: modelFactors   !< struct with modelFactors
    type (tpOvertopping), intent(inout)        :: overtopping    !< structure with overtopping results
    logical, intent(out)                       :: success        !< flag for success
    character(len=*), intent(out)              :: errorText      !< error message (only set if not successful)
    type(tLogging), intent(in)                 :: logging        !< logging struct
!
    type (tpGeometry)                          :: geometry       !< structure with geometry data
!
    call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, &
                                 overtopping, success, errorText, logging)
end subroutine omkeerVariantF

!>
!! Subroutine that sets the language for error and validation messages
!!
!! @ingroup dllDikesOvertopping
subroutine SetLanguage(lang)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"SetLanguage" :: SetLanguage
use OvertoppingMessages, only : SetLanguageCore => SetLanguage
character(len=*), intent(in) :: lang

call SetLanguageCore(lang)

end subroutine SetLanguage

!>
!! Subroutine that gets the language for error and validation messages
!!
!! @ingroup dllDikesOvertopping
subroutine GetLanguage(lang)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"GetLanguage" :: GetLanguage
use OvertoppingMessages, only : GetLanguageCore => GetLanguage
character(len=*), intent(out) :: lang

call GetLanguageCore(lang)
end subroutine GetLanguage

!>
!! Subroutine that delivers the version number
!!
!! @ingroup dllDikesOvertopping
subroutine versionNumber(version)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"versionNumber" :: versionNumber
    character(len=*), intent(out) :: version !< version number
    !
    ! locals
    !
    character(len=*), parameter :: cversion = "16.1.1.0"
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
