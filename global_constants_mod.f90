!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!
!>      VUA and IPSL/LSCE by the iLOVECLIM / LUDUS coding group / Within the LUDUS code environement
!
!       LICENSING TERMS:
!>      \copyright
!!      This file is part of the iLOVECLIM coupled climate model under the LUDUS framework.
!!      global_constants_mod is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
!!      License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
!!      version.
!!
!!      global_constants_mod is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
!!      warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!!
!!      You should have received a copy of the GNU General Public License along with Foobar.
!!      If not, see <http://www.gnu.org/licenses/>.
!
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!      MODULE: global_constants_mod
!
!>     @author  Didier M. Roche (dmr)
!
!
!>     @brief This module global_constants_mod is storing the global physical constants needed for the computation within
!>              the different modules in LUDUS
!
!>     @date Creation date: March, 1st, 2016
!>     @date Last modification: $LastChangedDate$
!>     @author Last modified by : dmr
!
!>     @version This is svn version: $LastChangedRevision$
!
!>     Here add the long_description of the module ...
!!     The constants in the different modules of the LOVECLIM and thus iLOVECLIM and LUDUS framework have been used without internal
!!       consistency between the different components. While this is probably fine in common applications, it may complicated the
!!       checkings of mass, energy etc. conservations between the different components. It also hampers the possibility of updating
!!       the value of the different constants easily.
!!     Thus the current module is developped with explicit relation to the physical constants, their units and the reference that
!!        have been used. The module will be slowly expanded to be included in as much components of the framework as possible.
!!     It will not be included in external svn resources to avoid conflict at external sub-component level.
!
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

      module global_constants_mod

       implicit none
       public

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!   Selected double precision as taken from:
!   Metcalf, M., J. Reid, and M. Cohen (2004). Fortran 95/2003 Explained. Oxford University Press.
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

       integer, parameter  :: sp = 4      ! kind(1.0) does not work if compilation is forced to double as in e.g. -fdefault-real-8
       integer, parameter  :: dp = selected_real_kind(2*precision(1.0_sp))
       integer, parameter  :: qp = selected_real_kind(2*precision(1.0_dp))

       integer, parameter  :: ip = 4

       integer, parameter  :: str_len =256

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
! dmr   Physical constants from http://arxiv.org/abs/1507.07956v1
!       CODATA group for physical constants
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

!       speed_lightspeed of light in vacuum c, c0 299 792 458 m s−1 exact
       real(dp), parameter :: ligth_speed = 299792458.0_dp

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
! dmr   Other Physical constants
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

! dmr  Temperature of 0°C in K                   (in K)
       real(dp), parameter :: tK_zero_C  = 273.15_dp

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
! dmr  Time constants (taken in part from https://www.cfa.harvard.edu/~dfabricant/huchra/ay145/constants.html)
!      Given in seconds (SI)
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

       real(dp), parameter :: second_s     = 1.0_dp
       real(dp), parameter :: minute_s     = 60.0_dp * second_s
       real(dp), parameter :: hour_s       = 60.0_dp * minute_s
       real(dp), parameter :: solar_day    = 24.0_dp * hour_s
       real(dp), parameter :: sidereal_day = 23.0_dp * hour_s + 56.0d0 * minute_s + 04.09054d0 * second_s

       real(dp), parameter :: months_year  = 12.0_dp    ! in days
       real(dp), parameter :: days_year360d= 360.0_dp   ! in days
       real(dp), parameter :: days_year365d= 365.0_dp   ! in days
       
       integer(ip), parameter :: months_year_i = nint(months_year)
       integer(ip), parameter :: days_year360d_i = nint(days_year360d)
       integer(ip), parameter :: seconds_month30d_i = solar_day * nint(days_year360d/months_year)

       real(dp), parameter :: sidereal_yr  = 3.155815E7_dp                 ! in seconds
       real(dp), parameter :: sidereal_yr_d= 3.155815E7_dp / sidereal_day  ! in sidereal days

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
! dmr  Physical constants for Earth (planet)
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

! dmr  Radius of the Earth ...              (in meters)
       real(dp), parameter :: radius_earth = 6.37E6_dp

! dmr  Solar Constant (as seen from the Earth s upper atmosphere) ...    (in W.m^-2)
       real(dp), parameter :: solar_constant = 1365.0_dp ! should be 1368 from ECBilt value / afq, back to pre-rev 638 value

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
! dmr  Physical constants for water (H2O)
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

! dmr  Volumetric mass density of water ... (in kg.m-3)
       real(dp), parameter :: vol_mass_dens_wat= 1000.0_dp
! dmr  Latent heat of vaporization ...      (in J.kg-1) ! ams value = 2.501+06
       real(dp), parameter :: latheat_vap_wat  = 2.5E6_dp
! dmr  Latent heat of sublimation ...       (in J.kg-1) ! ams value = 2.834+06
       real(dp), parameter :: latheat_sub_wat  =2.8E6_dp
! dmr  Latent heat of fusion ...            (in J.kg-1) ! ams value = 0.337+06
       real(dp), parameter :: latheat_fus_wat  =0.3E6_dp

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
! dmr  Mathematical constants
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

! dmr  PI, the number ...                              (no unit)
       real(dp), parameter :: pi_dp      = 4.0_dp*datan(1.0_dp)
! dmr  radian
       real(dp), parameter :: rad_to_deg = 180.0_dp/pi_dp
! dmr  degrees
       real(dp), parameter :: deg_to_rad = pi_dp/180.0_dp

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
! dmr   Other constants
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

! dmr  Altitude of Olympus Mons, on Planet Mars. Can be used as a big constant ...
       real(dp), parameter :: alt_olympus_mons = 21229.0_dp

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!      End of the module here
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|

end module global_constants_mod

!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
!      The End of All Things (op. cit.)
!-----|--1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2----+----3-|
