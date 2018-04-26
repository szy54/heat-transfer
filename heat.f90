#if !defined(PR)
#define PR 4
#endif

program main
    use gauss
    implicit none

    !vars
    integer(kind=8) :: i, N
    character(len=8):: arg
    real(kind=PR), allocatable :: A(:, :), X(:)
    real(kind=PR) :: h, P13, P2
    ! P13== P1 == P3
    real(kind=16) :: eps

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

    do I=1,N
        if( I .NE. N) then
            A(I, I+1) = P13
        end if
        if(I .NE. 1) then
            A(I,I-1) = P13
        end if
        A(I,I)=P2
    end do

    eps=0

    call eliminate(A,X,N)

    write(*,*) A

    deallocate(A)
    deallocate(X)

end program main