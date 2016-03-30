program main

    use, intrinsic :: iso_fortran_env, only: &
        stdout => OUTPUT_UNIT

    use type_Stack, only: &
        Stack

    ! Explicit typing only
    implicit none

    !--------------------------------------------------------------------------
    ! Dictionary
    !--------------------------------------------------------------------------
    type (Stack) :: s
    integer      :: i !! Counter
    !--------------------------------------------------------------------------

    call s%show()

    call s%push(3)
    call s%push(4)
    call s%show()

    call s%push(5)
    call s%show()

    call s%pop()
    call s%show()

    call s%push(6)
    call s%show()

    do i = 1, 3
        call s%pop()
        call s%show()
    end do

end program main
