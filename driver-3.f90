       PROGRAM driver_dgc

!-----|--1--------2---------3---------4---------5---------6---------7-|
!          This program shows and example usage of the dgc module
!
!     This is version 1.1, created June, 3rd, 2013
!     Last modification: June, 13th, 2013
!
! Copyright 2013, Didier M. Roche a.k.a. dmr
! Didier M. Roche: Didier.roche@lsce.ipsl.fr
! Release under GPLv3 license.
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
!-----|--1--------2---------3---------4---------5---------6---------7-|

       USE interpolate_mod, only: interpolate_init, interpolate
       USE ncio, only: nc_create, nc_write_attr, nc_write_dim, nc_write


       IMPLICIT NONE

!-----|--1--------2---------3---------4---------5---------6---------7-|
!      Tweaking FLAGS to change the program's behaviour
!-----|--1--------2---------3---------4---------5---------6---------7-|

!       DEBUG >  0 triggers additional printing for debug
#define DEBUG 1

!       PLOT == 0 defines the mode for real data output
!       PLOT >  0 defines the mode(s) for GMT plotting
#define PLOT 0

!-----|--1--------2---------3---------4---------5---------6---------7-|
!       GENERAL DEFINITION VARIABLES
!       nx, ny    :: ISM dimensions
!       nlat,nlon :: ATM model dimensions
!       nw        :: ranks for the distance
!                    1: is lat index i
!                    2: is lon index j
!                    3: is distance
!       nz        :: Number of neighbouring cells to interpolate with
!                    It has currently been tested with 4, 9, 25 and 49
!       ex        :: array expansion number
!       nbmois    :: number of months per year
!                    (only for climate data, if yearly data, set to 1)
!-----|--1--------2---------3---------4---------5---------6---------7-|

       INTEGER, PARAMETER :: nx = 360, ny = 180, nlat = 32, nlon = 64   &
              , nbmois = 12
       INTEGER, PARAMETER :: nw = 3, nz = 9, ex = 2

!-----|--1--------2---------3---------4---------5---------6---------7-|
!       Datafile containing climatic data to interpolate
!-----|--1--------2---------3---------4---------5---------6---------7-|
       CHARACTER(LEN = 41), PARAMETER ::                                &
             fclimSN="inputdata/PP_climato_lh5.txt"

!-----|--1--------2---------3---------4---------5---------6---------7-|
!       XCC, YCC, XLONG and YLAT are the values for the ISM grid
!       XCC and YCC are not used here
!       XLONG and YLAT are coordinates of the ISM grid
!-----|--1--------2---------3---------4---------5---------6---------7-|

!       REAL(KIND=8), DIMENSION(nx,ny) :: XCC, YCC, XLONG, YLAT
       REAL(KIND=8), DIMENSION(nx,ny) :: XLONG, YLAT

!-----|--1--------2---------3---------4---------5---------6---------7-|
!       pfyear is the final output: interpolated data on the ISM grid
!              as a mean annual
!       pfGi gives the same, but with the 12 months
!-----|--1--------2---------3---------4---------5---------6---------7-|
       REAL(KIND=8), DIMENSION(nx,ny) ::  pfyear
       REAL(KIND=8), DIMENSION(nx,ny, nbmois) :: pfGi

!-----|--1--------2---------3---------4---------5---------6---------7-|
!
!-----|--1--------2---------3---------4---------5---------6---------7-|
       REAL(KIND=8), DIMENSION(nx,ny,nw,nz) :: tab_dat

!-----|--1--------2---------3---------4---------5---------6---------7-|
!       latEcb, lonEcb are coordinates of the atmospheric grid
!-----|--1--------2---------3---------4---------5---------6---------7-|
       REAL(KIND=8), DIMENSION(nlat) :: latEcb
       REAL(KIND=8), DIMENSION(nlon) :: lonEcb

!-----|--1--------2---------3---------4---------5---------6---------7-|
!       Useful integers for loops mainly + file name placeholder
!-----|--1--------2---------3---------4---------5---------6---------7-|
!       INTEGER :: k, ios, i, j, i_close, j_close, corner, ii, jj, ll
       INTEGER :: i, j, ios
#if ( PLOT == 1 )
       INTEGER :: ii,jj
#endif
       CHARACTER*23 :: filin

!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2----------|
! cdmr  Adding the current date to the netCDF file
!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2----------|

       CHARACTER(8)   :: date
       CHARACTER(11)  :: date_fmttd

!-----|--1--------2---------3---------4---------5---------6---------7-|
!       sxsnowG is the reading in climatological variable
!-----|--1--------2---------3---------4---------5---------6---------7-|
       REAL(KIND=8), DIMENSION(nlon,nlat,nbmois) :: sxsnowG

       LOGICAL :: results



!-----|--1--------2---------3---------4---------5---------6---------7-|
!       Added for output with NCIO
!-----|--1--------2---------3---------4---------5---------6---------7-|
       character(len=256) :: filename
       real(kind=8), dimension(nx) :: lonlist
       real(kind=8), dimension(ny) :: latlist

!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2----------|
! cdmr  Initialization of the date
!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2----------|

        call date_and_time(DATE=date)
        date_fmttd = date(1:4)//"-"//date(5:6)//"-"//date(7:8)

!-----|--1--------2---------3---------4---------5---------6---------7-|
!       Reading in geographical coordinates of both models
!-----|--1--------2---------3---------4---------5---------6---------7-|

!       1.1: highres grid

!       Definition d'une grille de 1x1 degres, reguliere.
!       Besoin de définir les tableaux XLONG, YLAT qui contiennent
!        les longitudes et latitudes respectivement

!       Point le plus au sud est à -90 + 0.5 = -89.5
!       Point de démarrage à l'ouest est à 0.5


!       DO i=1,nx/2
!         XLONG(i,:) = 0.5+(i-1)*1.0
!       ENDDO
!       DO i=nx/2+1,nx
!         XLONG(i,:) = 0.5+(i-1)*1.0 - 360.
!       ENDDO

       DO i=1,nx
         XLONG(i,:) = 0.5+(i-1)*1.0
       ENDDO

       lonlist(:) = XLONG(:,1)

       DO j=1,ny
         YLAT(:,j) = -89.5+(j-1)*1.0
       ENDDO

       latlist(:) = YLAT(1,:)

!       1.2: Atmospheric model

       filin='inputdata/coord-T21.dat'

! lecture des coordonnees d'ECBilt dans un fichier externe
       open(unit=2004,file=filin,iostat=ios)
           read(2004,*) (latEcb(i),i=nlat,1,-1)
           read(2004,*) (lonEcb(j),j=1,nlon, 1)
       close(2004)


       results = interpolate_init(nx,ny,nlat,nlon,latEcb,lonEcb,YLAT    &
                                 ,XLONG,nw,nz,ex,tab_dat)

!-----|--1--------2---------3---------4---------5---------6---------7-|
!       Interpolation test from a file
!-----|--1--------2---------3---------4---------5---------6---------7-|

!       4.1: Reading climatic variables (12 months climatological)

       CALL ReadVarClimato(nlon,nlat,nbmois,sxsnowG,TRIM(fclimSN),111)


       results = interpolate(tab_dat,sxsnowG,pfGi,nx,ny,nw,nz,nlon      &
                                  ,nlat,nbmois)

!-----|--1--------2---------3---------4---------5---------6---------7-|
!       Yearly mean from the monthly ones
!-----|--1--------2---------3---------4---------5---------6---------7-|
      call calcmean(nx,ny,nbmois,pfGi,pfyear)

!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2----------|
!       Data output for GMT
!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2----------|
#if ( PLOT == 1 )
       DO jj = 1, ny
         DO ii = 1, nx
        WRITE(*,*) YLAT(ii,jj), XLONG(ii,jj), pfyear(ii,jj)
         ENDDO
      ENDDO
#endif

!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2----------|
! cdmr --- try writing out the results in a netCDF file ...
!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2----------|


      filename = "out_highres.nc"
      ! Create the netcdf file, write global attributes
      call nc_create(filename)
      call nc_write_attr(filename,"title", "highres output from dgc interpolate test")
      call nc_write_attr(filename,"institution",                        &
        "CNRS/Laboratoire des Sciences du Climat et de l'Environnement | Vrije Universiteit Amsterdam")
      call nc_write_attr(filename,"creation date",             date_fmttd)

      ! Write the dimensions (x, y), defined inline
      call nc_write_dim(filename,"x",x=lonlist,units="degrees",axis="longitude")
      call nc_write_dim(filename,"y",x=latlist,units="degrees",axis="latitude")
      call nc_write(filename,"snow",pfyear(:,:),dim1="x",dim2="y")

       write(*,*) "test value", MAXVAL(tab_dat(:,:,3,4))

!      DO i=1,nx
!      DO j=1,ny
!        if (NINT(tab_dat(i,j,3,4)).EQ.NINT(MAXVAL(tab_dat(:,:,3,4)))) then
!          write(*,*) "found max in: ", i,j, tab_dat(i,j,3,4)
!        endif
!      ENDDO
!      ENDDO

       END PROGRAM driver_dgc

!-dmr The End of All Things (op. cit.)
