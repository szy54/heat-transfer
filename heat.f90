#if !defined(PR)
#define PR 4
#endif

program main
    use gaussian_e
    implicit none

    !vars
    integer :: i, N
    character(len=8):: arg

    !get arg which program was called with
    call get_command_argument(1, arg)
    write(*,*) arg


end program main