!> @file
!! Main program for running the unit tests for dikes overtopping
!<
!
! Copyright (c) 2016, Deltares, HKV lijn in water, TNO
!
! $Id$
!
program unitTestsProgram

    use ftnunit
    use utilities
    use overtoppingRTOTests

    implicit none

    call prepareTests

    call runtests_init
    call setRunTestLevel(3) ! should be 3

    call runtests(allovertoppingRTOTests)

    call runtests_final
    call showResult

contains

!> Routine to start the testing
!! Note:
!!     This routine merely takes care that the unit tests
!!     are indeed run
subroutine prepareTests
    implicit none

    integer  :: lun   !< LU-number
    
    call getFreeLuNumber( lun )
    open( lun, file = 'ftnunit.run' )
    write( lun, '(a)' ) 'ALL'
    close( lun )

end subroutine prepareTests

!> Start the browser to show the result
!!
subroutine showResult
    implicit none

    call system( 'ftnunit.html' )

end subroutine showResult

end program unitTestsProgram
