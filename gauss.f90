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
            real(kind=PR), intent(inout):: A(N,N)
            real(kind=PR), intent(inout)::X(N)
            real(kind=PR) ::c
            integer(kind=8) ::I,J

            do i=1,N
                do J=1,N
                    if( I .NE. J) THEN
                        C=A(I,J)/A(I,I)
                        A(:,J)=A(:,J)-C*A(:,I)
                        X(J) = X(J) - C*X(I)
                        X(I) = X(I) / A(I,I)
                    end if
                end do
            end do
        end subroutine eliminate
end module gauss