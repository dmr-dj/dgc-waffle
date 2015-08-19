!-----|--1--------2---------3---------4---------5---------6---------7-|
!      Ce sous-programme calcule la moyenne annuelle d'une variable
!      non cumulative, d'apres calcmean piratee de (A,G)ISM
!
!      Auteur : P. Huybrecht, Didier M. Roche
!      Date   : Inconnue
!      Derniere modification : 11 Juin 2008, Didier M. Roche
!-----|--1--------2---------3---------4---------5---------6---------7-|

      SUBROUTINE calcmean(nx,ny,n,M1,M2)
        integer :: nx,ny,n,i,j,k
        real(KIND=8) :: M1(nx,ny,n),M2(nx,ny),help
        do i=1,nx
          do j=1,ny
            help=0.0
            do k=1,n
              help=help+M1(i,j,k)
            end do
            M2(i,j)=help/n
          end do
        end do
      END SUBROUTINE calcmean
