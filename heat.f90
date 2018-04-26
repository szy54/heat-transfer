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
    read(arg(1:len_trim(arg)), '(i8)') N
    write(*,*) N

end program main