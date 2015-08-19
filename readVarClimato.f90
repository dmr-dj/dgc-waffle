!-----|--1--------2---------3---------4---------5---------6---------7-|
!      Ce sous-programme lit une variable d'entree sur grille ECBIlt
!      ou CLIO dans un fichier externe. Typiquement, t2m ou snow pour  
!      le modele de calotte GRISLI ou SST pour les shelves
!
!      Auteur : Didier M. Roche
!      Date   : 15 Juin 2008
!      Derniere modification : 18 Juin 2008, Didier M. Roche
!-----|--1--------2---------3---------4---------5---------6---------7-|

       SUBROUTINE ReadVarClimato(nx,ny,nt,tableau,fich,num)

!-----|--1--------2---------3---------4---------5---------6---------7-|
!       Variables d'entree  : 
!          nx, ny, nz : taille du tableau a remplir par la routine
!          fich : nom du fichier a lire
!          num : numero d'unite pour l'ouverture du fichier
!       Variables de sortie : 
!          tableau(nx,ny) : contient la variable lue
!-----|--1--------2---------3---------4---------5---------6---------7-|

       IMPLICIT NONE

       INTEGER :: nx, ny, nt, num
       REAL(KIND=8), DIMENSION(nx,ny,nt) :: tableau
       CHARACTER(*) :: fich

!-----|--1--------2---------3---------4---------5---------6---------7-|
!       Lecture du fichier externe, grille ECBilt ou CLIO
!-----|--1--------2---------3---------4---------5---------6---------7-|

       open(num,file=fich, form="formatted")
       read(num,*) tableau
       close(num)

       END SUBROUTINE ReadVarClimato
