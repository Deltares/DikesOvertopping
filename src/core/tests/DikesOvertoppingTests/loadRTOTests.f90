!> @file
!! Contains the module loadRTOTests of the RTO overtopping dll
!! RTO stands for Risk, Assessment and Design instruments (in dutch: Risico, Toets en Ontwerpinstrumentarium)
!
! Copyright (c) 2015, Deltares, HKV lijn in water, TNO
! $Id$
!
!> 
!! Module, holding test series in which the load parameters are varied. These are the basic test series.
!!
!! @ingroup FailureMechanismsTests
module loadRTOTests

    use equalReals
    use ftnunit
    use utilities
    use angleUtilities
    use waveParametersUtilities
    use typeDefinitionsRTOovertopping
    use mainModuleRTOovertopping
    use geometryModuleRTOovertopping
    use readCrossSectionForTests
    use overtoppingInterface, only : OvertoppingGeometryTypeF

    implicit none
    private

    type (tpModelFactors)  :: modelFactors            ! structure with model factors
    character(len=90)      :: crossSectionFile        ! file with cross section coordinates
    character(len=90)      :: testSerieFile           ! file with values of the test serie
    character(len=90)      :: outputFile              ! file for the output of the testserie
    logical, parameter     :: probContext = .true.    ! flag for using the overtopping module in a probabilistic context

    public :: allLoadRTOTests, overtoppingDllTest

contains

!> Call all the basic overtopping test series for the RTO overtopping dll. In these test series the load parameters are varied.
!!
!! @ingroup FailureMechanismsTests
subroutine allLoadRTOTests( nCrossSections, nBasicTestSeries )
!
!   input/output parameters
!
    integer, intent(in)     :: nCrossSections       !< number of cross sections
    integer, intent(in)     :: nBasicTestSeries     !< number of basic test series
!
!   local parameters
!
    character(len=120)      :: frozenFile           ! frozen copy of the output file of the testserie
    character(len=90)       :: errorMessage         ! error message
    character(len=1)        :: crossSectionNumber   ! number of the cross section
    character(len=2)        :: testSerieNumber      ! number of the test serie
    integer                 :: i, j                 ! do-loop counters
!
!   Source
!
    ! fill the structure with model factors for the RTO overtopping dll
    call fillModelFactorsRTO (modelFactors, 2.3d0, 4.3d0, 1.75d0, 4.3d0, 1.6d0, 0.21d0)
    !
    ! execute all test series for all cross sections
    do i = 1, nCrossSections
        write (crossSectionFile,'(a,i1,a)') '../DikesOvertoppingTests/InputRTOovertopping/Cross_section', i, '.txt'
        do j = 1, nBasicTestSeries
            write (testSerieFile,'(a,i1,a)')   '../DikesOvertoppingTests/InputRTOovertopping/Basic_test', j, '.txt'
            write (outputFile,'(a,i1,a,i2.2,a)') 'output_section', i, '_test', j, '.txt'
            
            write (crossSectionNumber,'(I1)') i
            write (testSerieNumber,   '(I2)') j
            call testWithLevel( testSeriesLoadRTO, "Calculations with the RTO overtopping dll in the test serie " // &
                                           trim(testSerieNumber) // " and cross section " // crossSectionNumber, 1)
            
            write (frozenFile,'(a,i1,a,i2.2,a)') '../DikesOvertoppingTests/OutputRTOovertopping/output_section', i, '_test', j, '.txt'
            errorMessage = 'The file "' // trim(outputFile) // '" differs with the same file computed before.'
            call assert_files_comparable( outputFile, frozenFile, trim(errorMessage) )
        enddo
    enddo

end subroutine allLoadRTOTests

!> One test serie with varying one load parameter.
!!
!! @ingroup FailureMechanismsTests
subroutine testSeriesLoadRTO
!
!   Local parameters
!
    integer              :: tunit                ! unit-number for the test serie file
    integer              :: ounit                ! unit-number for the output file
    integer              :: ios                  ! input/output-status

    integer              :: nstep                ! number of computations in the test serie
    integer              :: istep                ! do-loop counter in the test serie

    real(wp)             :: waveSteepness        ! wave steepness
    real(wp)             :: beta                 ! wave direction w.r.t. the dike normal
    real(wp)             :: var                  ! value of the variable in the test serie
    real(wp)             :: varmin               ! minimum value of the variable in the test serie
    real(wp)             :: varmax               ! maximum value of the variable in the test serie
    real(wp)             :: varstep              ! step size of the variable in the test serie
    real(wp)             :: trig                 ! trigger for the variable in the test serie

    character(len=1)     :: comment              ! comment character
    character(len=2)     :: free                 ! symbol for the parameter to vary in the test serie 

    type (tpGeometry)    :: geometry             ! structure with geometry data
    type (tpLoad)        :: load                 ! structure with load data
    type (tpOvertopping) :: overtopping          ! structure with overtopping results
    logical              :: succes               ! flag for succes
    character(len=250)   :: errorMessage         ! error message

    real(wp), parameter  :: margin = 1.0d-6      ! relative value for the margin
!
!   source
!
    !
    !   read the cross section
    call readCrossSection(crossSectionFile, geometry, succes, errorMessage)
    !
    ! Read the test serie
    !
    ! Open the file with the test serie
    call getFreeLuNumber( tunit )
    open (unit=tunit, file=trim(testseriefile), status='old', iostat=ios)
    if (ios /= 0) then
        call fatalError ('Unable to open the file: ' // trim(testseriefile) )
    endif
    !
    ! Skip comment lines in test serie file 
    comment='#'
    do while (comment == '#')
        read (tunit,'(a)', iostat=ios) comment
        if (ios /= 0) then
            call fatalError ('Read error from the file: ' // trim(testseriefile) )
        endif
    enddo
    backspace(unit=tunit)

    ! read values for test serie in test serie file
    read(tunit,*,iostat=ios) varmin,varmax,varstep
    if (ios /= 0) call fatalError ('Read error from the file: ' // trim(testseriefile) )
    ! read water level (h)
    read(tunit,*,iostat=ios) load%h
    if (ios /= 0) call fatalError ('Read error from the file: ' // trim(testseriefile) )
    ! read wave height (Hm0)
    read(tunit,*,iostat=ios) load%Hm0
    if (ios /= 0) call fatalError ('Read error from the file: ' // trim(testseriefile) )
    ! read wave steepness
    read(tunit,*,iostat=ios) waveSteepness
    if (ios /= 0) call fatalError ('Read error from the file: ' // trim(testseriefile) )
    ! read wave direction w.r.t. North
    read(tunit,*,iostat=ios) load%phi
    if (ios /= 0) call fatalError ('Read error from the file: ' // trim(testseriefile) )

    close( tunit )

    ! The parameter, which is varied in the test serie, has the value -999.99
    trig = -999.99d0
    if     (equalRealsRelative(load%h,        trig, margin)) then
        free='wl'
    elseif (equalRealsRelative(load%Hm0,      trig, margin)) then
        free='hs'
    elseif (equalRealsRelative(waveSteepness, trig, margin)) then
        free='ws' 
    elseif (equalRealsRelative(load%phi,      trig, margin)) then
        free='th'
    endif
    !
    ! open the output file
    call getFreeLuNumber( ounit )
    open (unit=ounit, file=trim(outputFile), status='unknown', iostat=ios)
    if (ios /= 0) then
        call fatalError ('Unable to open the file: ' // trim(outputFile) )
    endif
    !
    ! write headers to output file
    write (ounit,'(a)') '# Input and results test serie RTO overtopping dll'
    write (ounit,'(a)') '#  1       2       3       4       5       6       7'
    write (ounit,'(a)') '#  WL      HS      WS      BETA    TSPEC   Z2%     Qo'

    nstep = nint( (varmax - varmin) / varstep)
    do istep = 0, nstep
        var = varmin + dble(istep) * varstep
 
        if     (free == 'wl') then
            load%h        = var
        elseif (free == 'hs') then
            load%Hm0      = var
        elseif (free == 'ws') then
            waveSteepness = var
        elseif (free == 'th') then
            load%phi      = var
        endif

        !
        ! compute the wave period
        load%Tm_10 = computeWavePeriod( load%Hm0, waveSteepness )
        !
        ! compute the angle of wave attack
        beta = angleBetween2Directions( load%phi, geometry%psi )
        !
        ! Compute the wave runup and the wave overtopping discharge with the RTO-overtopping module
        call calculateOvertopping (geometry, load, modelFactors, probContext, overtopping, succes, errorMessage)
        !
        ! Write the results to the output file
        write (ounit,'(f8.2,2f8.3,f8.1,f8.2,f8.3,f15.10)') load%h, load%Hm0, waveSteepness, beta, load%Tm_10, &
                                                           load%h + overtopping%z2, overtopping%Qo
    enddo
    close( ounit )

    call deallocateGeometry( geometry )

end subroutine testSeriesLoadRTO

!> Test the functions in dllOvertopping.dll.
!!     these functions are:
!!     - calcZValue
!!     - calculateQoF
!!     - versionNumber
!!
!!     - test overflow ( waterlevel > dike heigth)
!!     - test with and without waves
!!
!! @ingroup FailureMechanismsTests
subroutine overtoppingDllTest
    use user32
    use kernel32

    real(kind=wp)      :: z                 !< z value
    real(kind=wp), parameter :: zExpected1a =   29.88985_wp
    real(kind=wp), parameter :: zExpected1b =  701.48866_wp
    real(kind=wp), parameter :: zExpected2 = -6.43985_wp
    real(kind=wp), parameter :: margin     =  0.0000100_wp

    integer                        :: p
    character(len=12)              :: version
    external                       :: calcZValue
    external                       :: calculateQoF
    external                       :: versionNumber
    integer                        :: i
    logical                        :: succes
    integer, parameter             :: npoints = 3
    real(kind=wp)                  :: normal, waveSteepness, z_overflow
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(OvertoppingModelFactors)  :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate

    pointer            (qz, calcZValue)
    pointer            (qv, versionNumber)
    pointer            (qc, calculateQoF)

    p = loadlibrary    ("dllOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qv = getprocaddress (p, "versionNumber"C)
    qc = getprocaddress (p, "calculateQoF"C)
    qz = getprocaddress (p, "calcZValue"C)
    !
    ! initializations
    !
    dikeHeight  = 9.1_wp
    modelFactors%factorDeterminationQ_n_f_n = 2.3_wp
    modelFactors%factorDeterminationQ_b_f_b = 4.3_wp
    modelFactors%frunup1  = 1.75_wp
    modelFactors%frunup2  = 4.30_wp
    modelFactors%frunup3  = 1.60_wp
    modelFactors%fshallow = 0.21_wp
    modelFactors%ComputedOvertopping = 1.0_wp
    modelFactors%CriticalOvertopping = 1.0_wp
    criticalOvertoppingRate        = 1.0d-3

    call versionNumber(version)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    do i = 1, npoints
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * i
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    !
    load%h        =  5.50_wp
    load%phi      = 50.00_wp
    load%Hm0      =  1.00_wp
    waveSteepness =  0.04_wp
    load%Tm_10    = computeWavePeriod( load%Hm0, waveSteepness )
    !
    ! test actual computions in calculateQo and zFuncOvertopping for waterlevel < dikeheigth
    !
    call calculateQoF( load, geometryF, dikeHeight, modelFactors, overtopping, succes, errorMessage )
    call assert_true ( succes, errorMessage )
    call assert_comparable ( overtopping%Qo, 0.1044725180580d-15, margin, 'Qo from dllOvertopping.dll')
    call assert_comparable ( overtopping%z2, 0.85575d0, margin, 'z2 from dllOvertopping.dll')
    call calcZValue ( criticalOvertoppingRate, modelFactors, overtopping%Qo, z, succes, errorMessage)
    call assert_true ( succes, errorMessage )
    call assert_comparable( z, zExpected1a, margin, "Z value from dllOvertopping.dll; overflow" )
    !
    ! test actual computions in calculateQo and zFuncOvertopping for waterlevel > dikeheigth (overflow)
    !
    load%h        =  9.50_wp
    call calculateQoF( load, geometryF, dikeHeight, modelFactors, overtopping, succes, errorMessage )
    call assert_true ( succes, errorMessage )
    call assert_comparable ( overtopping%Qo,  0.626311424133d0, margin, 'Qo from dllOvertopping.dll')
    call assert_comparable ( overtopping%z2, 3.0989d0, margin, 'z2 from dllOvertopping.dll')
    call calcZValue ( criticalOvertoppingRate, modelFactors, overtopping%Qo, z, succes, errorMessage)
    call assert_true ( succes, errorMessage )
    call assert_comparable( z, zExpected2, margin, "Z value from dllOvertopping.dll; overflow" )
    !
    ! test actual computions in calculateQo and zFuncOvertopping for waterlevel < dikeheigth, without waves
    !
    load%h        =  5.50_wp
    load%Hm0      =  0.00_wp
    call calculateQoF( load, geometryF, dikeHeight, modelFactors, overtopping, succes, errorMessage )
    call calcZValue ( criticalOvertoppingRate, modelFactors, overtopping%Qo, z, succes, errorMessage)
    call assert_comparable( z, zExpected1b, margin, "Z value from dllOvertopping.dll; no waves test" )

    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine overtoppingDllTest

end module loadRTOTests