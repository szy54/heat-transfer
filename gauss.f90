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

module gaussian_e
    implicit none

contains
        subroutine eliminate(A,X,N)
            implicit none
            real(kind=PR), intent(inout):: A(N,N)
            integer(kind=8), intent(in):: N
            real(kind=PR), intent(inout)::X(N)
            real(kind=PR) ::c
            integer ::I,J

            do i=1,N
                do J=0,N
                    if( I .NE. J) THEN
                        C=A(I,J+1)/A(I,I+1)
                        A(:,J+1)=A(:,J+1)-C*A(:,I+1)

                        X(J+1)=X(J+1)-C*X(I+1)
                    end if
                end do
            end do
        end subroutine eliminate
end module gaussian_e