module type_Node

    ! Explicit typing only
    implicit none

    ! Everything is private unless stated otherwise
    private
    public :: Node


    ! Declare the derived data type
    type, public :: Node
        !----------------------------------------------------------------------
        ! Class variables
        !----------------------------------------------------------------------
        integer,              public :: data = 0
        type (Node), pointer, public :: next => null()
        !----------------------------------------------------------------------
    end type Node


end module type_Node
