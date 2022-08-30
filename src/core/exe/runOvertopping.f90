program runOvertopping
    use mainModuleOvertopping
    use typeForLoad
    use overtoppingInterface
    use typeDefinitionsOvertopping
    use ModuleLogging
    use errorMessages
    use geometryModuleOvertopping
    implicit none
    character(len=256) :: infile, errorText
    integer :: iunit, nPoints, ierr
    double precision:: dikeHeight, psi
    double precision, allocatable :: roughnessFactors(:)
    type(tpCoordinatePair) :: coordinates
    type(tpLoad) :: load           !< struct with waterlevel and wave parameters
    type(tpGeometry) :: geometry       !< fortran struct with geometry and roughness
    type(tpOvertoppingInput) :: modelFactors   !< struct with modelfactors
    type(tpOvertopping) :: overtopping
    type(tLogging)      :: logging
    logical             :: success
    type(tMessage)      :: error

    if (command_argument_count() < 1) then
        write(*,*) "need name of input file as first argument"
        stop -1
    end if

    call get_command_argument( 1, infile, status = ierr )

    open(newunit = iunit, file = infile, status='old', action='read', iostat=ierr)
    if (ierr == 0) read(iunit,*, iostat=ierr) nPoints
    if (ierr == 0) allocate(coordinates%x(nPoints), coordinates%y(nPoints), roughnessFactors(nPoints-1), stat=ierr)
    if (ierr == 0) read(iunit,*,iostat=ierr) coordinates%x
    if (ierr == 0) read(iunit,*,iostat=ierr) coordinates%y
    if (ierr == 0) read(iunit,*,iostat=ierr) roughnessFactors
    if (ierr == 0) read(iunit,*,iostat=ierr) psi
    if (ierr == 0) read(iunit,*,iostat=ierr) dikeHeight
    if (ierr == 0) read(iunit,*,iostat=ierr) load
    if (ierr /= 0) then
        write(*,*) "error at opening or reading file: ", infile
        stop -1
    end if
    close(iunit)

    coordinates%y(nPoints) = dikeHeight
    coordinates%n = nPoints
    call initializeGeometry (psi, coordinates, roughnessFactors(1:coordinates%N-1), geometry, error)

    modelFactors%factorDeterminationQ_b_f_n = 1.0d0
    modelFactors%factorDeterminationQ_b_f_b = 1.0d0
    modelFactors%m_z2                       = 1.0d0
    modelFactors%fshallow                   = 1.0d0
    modelFactors%ComputedOvertopping        = 1.0d0
    modelFactors%CriticalOvertopping        = 1.0d0
    modelFactors%relaxationFactor           = 1.0d0
    modelFactors%reductionFactorForeshore   = 0.5d0

    if (error%errorCode == 0) call calculateOvertopping (geometry, load, modelFactors, overtopping, error)
    if (error%errorCode == 0) then
        write(*,*) 'z-2% = ', overtopping%z2
    else
        write(*,*) 'error: ', error%message
    endif

    end program runOvertopping

! format input file
! nPoints  # number of coordinates
! x1 .. xN # all xcoordinates
! y1 .. yN # all ycoordinates
! r1 .. rN-1 # all roughness
! psi        # dikenormal
! dikeheight
! load
