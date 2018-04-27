! link do gita jako wynik projektu
! kompilacja z flagą -std 2008
! używać make lub rake lub cmake
! moduły
! wykorzystać jak najwięcej rzeczy z wykładu

! policzyć błąd numeryczny rozwiązania za pomocą MRS w porównaniu do wartości dokładnej
! w rozwiązaniu wykresik błędu w zależności od gęstości
! można używać różnych kajndów

#if !defined(PR)
#define PR 4
#endif

module gauss
    implicit none

contains
    subroutine eliminate(A,X,N)
        implicit none
        integer(kind=8), intent(in):: N
        real(kind=PR), intent(inout):: A(N-1,3)
        real(kind=PR), intent(inout)::X(N-1)
        real(kind=PR) ::C
        integer(kind=8) ::I,J

        X(1)=X(1)/A(1,2)
        A(1,:)=A(1,:)/A(1,2)

        do I=2,N-1
            C=A(I,1)/A(I-1,2)
            A(I,1) = A(I,1) - C * A(I-1,2);
            A(I,2) = A(I,2) - C * A(I-1,3);
            X(I) = X(I) - C * X(I-1);
            X(I) = X(I)/A(I,2);
            A(I,:) = A(I,:)/A(I,2);
        end do
    end subroutine eliminate
end module gauss