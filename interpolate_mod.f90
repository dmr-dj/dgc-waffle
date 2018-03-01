       module interpolate_mod

!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
!         This module implement a simple 2D interpolation routine
!
!     This is version 1.1, created June, 3rd, 2013
!     Updated version 1.2, created August, 1st, 2015
!     Updated version 1.3, created February, 15th, 2018
!     Last modification: February, 16th, 2018

!
! Copyright 2013,2015,2018 Didier M. Roche a.k.a. dmr
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
!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|

!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
!      Tweaking FLAGS to change the interpolation's behaviour
!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|

!       DEBUG >  0 triggers additional printing for debug
#define DEBUG 0
!       DEBUG > 0 compatibility flag for use with lens-grid type objects
#define USE_SUBG_OBJ 0

!       INT_MODEL defines the type of interpolation model you wish
!       INT_MODEL 0 is no distance weight (equal contribution of all cells)
!       INT_MODEL 1 depends in cell distance quadratrically
!       INT_MODEL 2 depends in cell distance with exponential model
#define INT_MODEL 1

      USE global_constants_mod, only: dp, ip

      IMPLICIT NONE

      CONTAINS

      logical function interpolate_init(nx,ny,nlat,nlon,latEcb,lonEcb   &
                ,YLAT,XLONG,nw,nz,ex,int_coord_tab,weights, sum_weights)

!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
!       distance_great_circle module: main routines for computation
!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
       USE dgc, ONLY: compute_bounds, find_closest_EC_cell, which_corner, create_interp_data, expand_tab, circular_coord

!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
!       GENERAL DEFINITION VARIABLES
!       nx, ny    :: highres grid
!       nlat,nlon :: lowres grid
!       nw        :: ranks for the distance
!                    1: is lat index i
!                    2: is lon index j
!                    3: is distance
!       nz        :: Number of neighbouring cells to interpolate with
!                    It has currently been tested with 4, 9, 25 and 49
!       ex        :: array expansion number
!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|

       INTEGER(ip), INTENT(IN) :: nx, ny, nlat, nlon, nw, nz, ex


!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
!       XLONG and YLAT are the two dimentionsal coordinates of the highres grid
!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|

       REAL(dp), DIMENSION(nx,ny) :: XLONG, YLAT

!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
!      Output of this routine == tab of weights for the computation
!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|

       REAL(dp),   DIMENSION(nz,nx,ny)     ,INTENT(out) :: weights
       REAL(dp),   DIMENSION(nx,ny)        ,INTENT(out) :: sum_weights
       INTEGER(ip),DIMENSION(nw-1,nz,nx,ny),INTENT(out) :: int_coord_tab

!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
!       latEcb, lonEcb are coordinates of the atmospheric grid
!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
       REAL(dp), DIMENSION(nlat), INTENT(in) :: latEcb
       REAL(dp), DIMENSION(nlon), INTENT(in) :: lonEcb

!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
!       Variables subscripted with "ex" are extended grids
!        latexEcb, lonexEcb are the coordinates of the atm. model
!        latex_bEcb, lonex_bEcb are the boundaries of the atm. grid
!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
       REAL(dp), DIMENSION(-ex+1:nlat+ex) :: latexEcb, latex_bEcb
       REAL(dp), DIMENSION(-ex+1:nlon+ex) :: lonexEcb, lonex_bEcb

!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
!       Useful integers for loops mainly + file name placeholder
!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
       INTEGER(ip) :: k, i_close, j_close, corner, ii, jj, ll, i, j

#if ( INT_MODEL == 1 || INT_MODEL == 2 )
       REAL(dp) :: valmax, valmin
#endif

       REAL(dp), DIMENSION(nx,ny,nw,nz) :: tab_dat


!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
!       sumw is the sum of weights from the neighbouring points
!       valmax is the maximum distance over the neighbouring cells
!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
       REAL(dp) :: sumw

!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
!       Preparing arrays for computations
!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|

!       2.1: expansion of arrays (longitude IS circular, latitude NOT)

       CALL expand_tab(latEcb,nlat,ex,latexEcb,0)
       CALL expand_tab(lonEcb,nlon,ex,lonexEcb,1)

!dmr   We need to fix manually the northest and southest bound ; if not
!       the last point will be the last grid point center.

       latexEcb(nlat+1:nlat+ex) = 90.0_dp
       latexEcb(-ex+1:0) = -90.0_dp

#if ( DEBUG >= 2 )
       write(*,*) latEcb
       write(*,*) latexEcb
       read(*,*)
#endif

#if ( USE_SUBG_OBJ == 0 )
!       2.2: computing cells bounds on the basis of the cells center values

       CALL compute_bounds(latexEcb,nlat+2*ex,latex_bEcb)
       CALL compute_bounds(lonexEcb,nlon+2*ex,lonex_bEcb)

!       2.3: quick fixing of the extrema: one cannot expect the last value to be
!            correct since there is initially no +90°N,S bound

       latex_bEcb(-ex+1:-ex+2) = -90.0_dp
       latex_bEcb(nlat+ex-2:nlat+ex) = 90.0_dp
       lonex_bEcb(nlon+ex) = lonex_bEcb(circular_coord(nlon+ex,1,nlon)) + 360.0_dp

#if ( DEBUG >= 2 )
       DO i=nlat+ex,-ex+1,-1
          WRITE(*,*) latexEcb(i), latex_bEcb(i)
       ENDDO

       DO i=nlon+ex,-ex+1,-1
          WRITE(*,*) lonexEcb(i), lonex_bEcb(i)
       ENDDO

       READ(*,*)
#endif


#else /* when USE_SUBG_OBJ, the bounds area read in the outer file */

!       2.2 Initialize the bound table from the one existing in sub_grid
!           NOTA: in subgrid the bounds table have dimension (nx,2)
!                 whereas here they are only nx+ex

!       2.3 Compute the expanded table(s) that are still absent

#endif

         weights(:,:,:)=0.0
         sum_weights(:,:)=0.0

!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
!       Main loop over the cells of the ISM
!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|

       DO ii = 1, nx
         DO jj = 1, ny

       k = 0

#if ( USE_SUBG_OBJ == 0 )
!       3.1: lookup the closest atmospheric cell of ISM(ii,jj)

       CALL find_closest_EC_cell(XLONG(ii,jj), YLAT(ii,jj),latexEcb     &
                    ,latex_bEcb,nlat,nlon, lonexEcb,lonex_bEcb, i_close &
                    ,j_close,ex)

!       3.2: in case we are out the range [1,nlon] of coordinates, reset

        j_close = circular_coord(j_close,1,nlon)

#if ( DEBUG >= 0 )
       if (ii.EQ.(nx-1)) then
!       WRITE(*,*) "Obtained coords:", i_close, j_close
!       WRITE(*,*) latexEcb(i_close-2:i_close+2)
!       WRITE(*,*) "Obtained : LAT", YLAT(ii,jj), latex_bEcb(i_close-1)  &
!                    , latexEcb(i_close), latex_bEcb(i_close)
!
!       WRITE(*,*) "Obtained : LONG", XLONG(ii,jj),lonex_bEcb(j_close-1) &
!                    , lonexEcb(j_close), lonex_bEcb(j_close)
!       READ(*,*)
       endif
#endif

#else

!       3.1 the closest EC_cell has been already found in sub_grid aggreg
!           use the clustered points table to this end ...

#endif /* on USE_SUBG_OBJ */

!       3.3: need to find in which corner of the atmospheric cell we are
!            to make sure that we interpolate correctly with neighbouting
!            cells

       call which_corner(XLONG(ii,jj), YLAT(ii,jj),latEcb               &
                    ,nlat,nlon, lonEcb,i_close,j_close, corner)

!       3.4: create interpolation data with distance ...
       call create_interp_data(XLONG(ii,jj), YLAT(ii,jj), latEcb, lonEcb&
                    ,nlat,nlon,i_close,j_close, corner,ii,jj,nx,ny,nw,nz&
                    ,tab_dat)

!       3.5: spanning the grid to check whether each original point
!             is taken into account in the cornered table version

        DO ll = 1, nz
          IF ((NINT(tab_dat(ii,jj,1,ll)).EQ.i_close).AND.               &
          (NINT(tab_dat(ii,jj,2,ll)).EQ.j_close)) THEN
            k = 1
          ENDIF
        ENDDO ! on nz

        interpolate_init = .true.
         IF ( k.EQ.0) THEN
             WRITE(*,*) "PROBLEM !!!! ", ii,jj
             READ(*,*)
             interpolate_init = .false.
         ENDIF

#if ( INT_MODEL == 1 )
!dmr    valmax is the maximum distance from the given cells around
         valmax  = MAXVAL(tab_dat(ii,jj,nw,:))
         valmin  = MINVAL(tab_dat(ii,jj,nw,:))

#elif ( INT_MODEL == 2 )
!dmr    in model 2, valmax should be the e-fold  distance (a valmin in fact)
           ! valmax  = MINVAL(tab_dat(ii,jj,nw,:))
         valmax  = 400000.0 ! in meters
#endif
         sumw = 0.0d0

!dmr    nz is the number of neighbouring cells you want to interpolate with
         DO ll = 1, nz

           i = NINT(tab_dat(ii,jj,1,ll))
           j = NINT(tab_dat(ii,jj,2,ll))

           int_coord_tab(1,ll,ii,jj)=i
           int_coord_tab(2,ll,ii,jj)=j

           weights(ll,ii,jj) =                                          &
#if ( INT_MODEL == 1 )
!~            (1-tab_dat(ii,jj,nw,ll)**2/valmax**2)
            (valmax-tab_dat(ii,jj, nw, ll))**2/(valmax-valmin)**2
#elif ( INT_MODEL == 2 )
           EXP(1-tab_dat(ii,jj,nw,ll)/valmax)
#elif ( INT_MODEL == 0 )
           1.0d0
#endif
           sumw = sumw + weights(ll,ii,jj)

           ENDDO ! on nz

           sum_weights(ii,jj) = sumw

        ENDDO ! on ny
      ENDDO   ! on nx

!~       WRITE(*,*) "Finalized interpolate_init"

      return
      end function interpolate_init



      logical function interpolate(int_coord_tab,weights,sum_weights,sxsnowG,pfGi,nx,ny,nw,nz,nlon,nlat)

      INTEGER(ip)                          ,INTENT(in) :: nx,ny,nw,nz,nlon,nlat

      INTEGER(ip), DIMENSION(nw-1,nz,nx,ny),INTENT(in) :: int_coord_tab
      REAL(dp),    DIMENSION(nz,nx,ny)     ,INTENT(in) :: weights
      REAL(dp),    DIMENSION(nx,ny)        ,INTENT(in) :: sum_weights
!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
!       sxsnowG is the reading in climatological variable
!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
      REAL(dp), DIMENSION(nlon,nlat), INTENT(in) :: sxsnowG

      REAL(dp), DIMENSION(nx,ny), INTENT(out) :: pfGi

!~       REAL(dp) :: sumw
!~ #if ( INT_MODEL == 1 || INT_MODEL == 2 )
!~        REAL(dp) :: valmax
!~ #endif

       INTEGER :: i,j,ii,jj,ll

!       4.2: Actual interpolation

       pfGi(:,:) = 0.0d0

       DO jj = 1, ny
         DO ii = 1, nx

!dmr    nz is the number of neighbouring cells you want to interpolate with
           DO ll = 1, nz

             i = int_coord_tab(1,ll,ii,jj)
             j = int_coord_tab(2,ll,ii,jj)

             pfGi(ii,jj) = pfGi(ii,jj) + sxsnowG(j,i)                   &
             * weights(ll,ii,jj)

           ENDDO ! on nz

           pfGi(ii,jj) = pfGi(ii,jj) / sum_weights(ii,jj)

#if ( DEBUG > 1 )
      DO i=1,NINT(SQRT(REAL(nz,KIND=4)))
        WRITE(*,'(3F18.8)') (tab_dat(ii,jj,nw,j)/1000.0,j=(i-1)*3+1,i*3)
      ENDDO
      DO i=1,NINT(SQRT(REAL(nz,KIND=4)))
        WRITE(*,'(3F10.5)') (weights(ii,jj,j)/sumw,j=(i-1)*3+1,i*3)
      ENDDO
      READ(*,*)
#endif

         ENDDO
      ENDDO

      interpolate = .true.

      end function interpolate

      end module interpolate_mod

!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
!-dmr The End of All Things (op. cit.)
!-----|--1---------2---------3---------4---------5---------6---------7---------8---------9---------0---------1---------2---------3-|
