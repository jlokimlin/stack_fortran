module type_Stack

    use, intrinsic :: iso_fortran_env, only: &
        stdout => OUTPUT_UNIT

    use type_Node, only: &
        Node

    ! Explicit typing only
    implicit none

    ! Everything is private unless stated otherwise
    private
    public :: Stack

    ! Declare the derived data type
    type, public :: Stack
        !----------------------------------------------------------------------
        ! Class variables
        !----------------------------------------------------------------------
        integer,              public :: size_of_stack = 0
        type (Node), pointer, public :: top => null()
        !----------------------------------------------------------------------
    contains
        !----------------------------------------------------------------------
        ! Class methods
        !----------------------------------------------------------------------
        procedure, public :: push !! Insert element
        procedure, public :: pop !! Remove top element
        procedure, public :: show !! Print stack contents to console
        procedure, public :: is_empty !! Test whether stack is empty
        procedure, public :: size !! Returns the number of elements in the stack
        final             :: finalize_stack !! Destructor
        !----------------------------------------------------------------------
    end type Stack


contains


    subroutine push(this, n)
        !----------------------------------------------------------------------
        ! Dictionary: calling arguments
        !----------------------------------------------------------------------
        class (Stack), intent (in out) :: this
        integer ,      intent (in)     :: n
        !----------------------------------------------------------------------
        ! Dictionary: local variables
        !----------------------------------------------------------------------
        class (Node), pointer, save :: ptr => null()
        !----------------------------------------------------------------------

        ! Create memory leak
        allocate ( ptr, source=Node(data=n, next=null()) )

        ! Push
        if (this%is_empty() .eqv. .false.) then
            ptr%next=> this%top
        end if

        ! Assign pointer
        this%top => ptr

        ! Increment size of stack
        associate( s => this%size_of_stack )
            s = s + 1
        end associate

    end subroutine push



    subroutine pop(this)
        !----------------------------------------------------------------------
        ! Dictionary: calling arguments
        !----------------------------------------------------------------------
        class (Stack), intent (in out) :: this
        !----------------------------------------------------------------------
        ! Dictionary: local variables
        !----------------------------------------------------------------------
        class (Node), allocatable :: temp
        !----------------------------------------------------------------------

        ! Address the case when the stack is empty
        if (this%is_empty() .eqv. .true.) then
            write( stdout, '(A)') 'The stack is empty'
            return
        end if

        ! Make a copy
        allocate ( temp, source=this%top)

        ! Remove top element
        this%top => this%top%next

        ! Print action to console
        write( stdout, '(A,I3)') 'Pop operation..............'
        write( stdout, '(A,I3)') 'Poped value is ', temp%data

        ! Decrement size of stack
        associate( s => this%size_of_stack )
            s = s - 1
        end associate

        ! Release memory
        deallocate ( temp )

    end subroutine pop



    subroutine show(this)
        !----------------------------------------------------------------------
        ! Dictionary: calling arguments
        !----------------------------------------------------------------------
        class (Stack), intent (in out) :: this
        !----------------------------------------------------------------------
        ! Dictionary: local variables
        !----------------------------------------------------------------------
        type (Node), pointer :: ptr => null()
        !----------------------------------------------------------------------

        ! Address the case when the stack is empty
        if (this%is_empty() .eqv. .true.) then
            write( stdout, '(A)') 'The stack is empty'
            return
        else
            write( stdout, '(A,I3)') 'size of stack = ', this%size()
            write( stdout, '(A)', advance = 'no') 'The stack is: '
            ptr => this%top !! Assign pointer
            do while ( associated( ptr ) )
                write( stdout, '(I3,A)', advance = 'no' ) ptr%data, ' ->'
                ptr => ptr%next !! Re-assign pointer
            end do
            write( stdout, '(A)' ) ' null()'
        end if

    end subroutine show



    function is_empty(this) result (return_value)
        !----------------------------------------------------------------------
        ! Dictionary: calling arguments
        !----------------------------------------------------------------------
        class (Stack), intent (in out) :: this
        logical                        :: return_value
        !----------------------------------------------------------------------

        return_value = ( .not.associated(this%top) )

    end function is_empty



    function size(this) result (return_value)
        !----------------------------------------------------------------------
        ! Dictionary: calling arguments
        !----------------------------------------------------------------------
        class (Stack), intent (in out) :: this
        integer                        :: return_value
        !----------------------------------------------------------------------

        return_value = this%size_of_stack

    end function size



    recursive subroutine finalize_stack(this)
        !----------------------------------------------------------------------
        ! Dictionary: calling arguments
        !----------------------------------------------------------------------
        type (Stack), intent (in out) :: this
        !----------------------------------------------------------------------

        if (this%is_empty() .eqv. .false.) deallocate (this%top)

    end subroutine finalize_stack


end module type_Stack
