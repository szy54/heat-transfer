#if !defined(PR)
#define PR 4
#endif

program main
    use gaussian_e
    implicit none

    !vars
    integer :: i, N
    character(len=8):: arg
    real(kind=PR), allocatable :: A(:, :), X(:)
    real(kind=PR) :: h, P13, P2
    ! P13== P1 == P3


    !get arg which program was called with
    call get_command_argument(1, arg)
    !write(*,*) arg
    read(arg(1:len_trim(arg)), '(i8)') N
    !write(*,*) N

    h = 1./N
    P13=1/(h*h)
    P2=-2/(h*h)

    allocate(A(N,N))
    allocate(X(N))

    A(:,:)=0
    X(:)=0
    X(N)=1

    !fill in the matrix
    A(1, 3)=P13
    A(1,2)=P2

    do I=2,N-2
        A(I, 1)=P13
        A(I,2)=P2
        A(I,3)=P13
    end do
    A(N-1,2)=P2
    A(N-1,1)=P13

    write(*,*) A

    deallocate(A)
    deallocate(X)

end program main