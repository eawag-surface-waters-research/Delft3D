!!  Copyright (C)  Stichting Deltares, 2012-2019.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

!!  *********************************************************************
!!  *    Module containing all the shared data of BLOOM II              *
!!  *********************************************************************

!!   contains the following modules:
!!    - bloom_data_dim    dimensions and sizes
!!    - bloom_data_arran      light curve data
!!    - bloom_data_mass_balance     mass balance communication
!!    - bloom_data_caldynam   dynamic running
!!    - bloom_data_io     io info
!!    - bloom_data_matrix     a matrix for solver
!!    - bloom_data_phyt       pythoplankton data
!!    - bloom_data_putin      input data
!!    - bloom_data_sumou      summary output
!!    - bloom_data_xvect      x vectors

module bloom_data_dim
   integer, parameter :: mt = 30                  ! maximum number of phytoplankton types
   integer, parameter :: ms = 15                  ! maximum number of phytoplankton species (groups)
   integer, parameter :: mn =  8                  ! maximum number of nutrients
   integer, parameter :: mg =  1                  ! maximum number of grazers
   integer, parameter :: ia = mn + 2 + 1 + 2 * ms ! maxmimum number of rows in A-matrix. This is equal to the sum of
                                                  ! mn + 2 (energy constraints) + 1 (exclusion row) + 2 * ms
                                                  ! mortality and growth constraints of species)
   integer, parameter :: mx = ia + mt + 1         ! maximum dimension of X vector. This is equal to maximum number
                                                  ! of constraints ia + mt + 1 (optimum)
   integer            :: iperm
end module bloom_data_dim

module bloom_data_size
   use bloom_data_dim
   real(8)            :: pmax1(mt)
   real(8)            :: pmax2(mt)
   real(8)            :: pmax(mt)
   real(8)            :: rmort1(mt)
   real(8)            :: rmort2(mt)
   real(8)            :: rmort(mt)
   real(8)            :: res1(mt)
   real(8)            :: res2(mt)
   real(8)            :: resp(mt)
   real(8)            :: chltoc(mt)
   real(8)            :: ctodry(mt)
   real(8)            :: sdmix(mt)
   real(8)            :: zoopr(mt,0:mg)
   real(8)            :: surf(mt)
   real(8)            :: toplev
   real(8)            :: biobas
   real(8)            :: temlim
   real(8)            :: basmor
   real(8)            :: rmort3(mt)
   real(8)            :: aveffi(mt)

   integer            :: lpmax(mt)
   integer            :: nprodu
   integer            :: lprodu
   integer            :: ldiel
   integer            :: lpools
   integer            :: loxout
   integer            :: ldydea
   integer            :: ldyext
   integer            :: lgroch
   integer            :: lmorch
   integer            :: lpmort
   integer            :: ltlim
   integer            :: lobfun
end module bloom_data_size

module bloom_data_arran
   use bloom_data_dim
   real(8)            :: fun(51,ms)
   real(8)            :: der(51,ms)
   real(8)            :: zvec(51)
   real(8)            :: daymul(24,ms)
   real(8)            :: dl(24)
   real(8)            :: verfrm
   real(8)            :: aroot(2*mt)
   real(8)            :: euligh
   real(8)            :: tefcur
   real(8)            :: power(51)
   real(8)            :: effic(51,ms)
   integer            :: nz
   integer            :: npoint
   integer            :: ldayeu
end module bloom_data_arran

module bloom_data_mass_balance
   integer, parameter :: ntypm2=30                ! dimensies arrays = max aantal algen , koppeling gebruikt 30 , bloom 30 dus maar 30 genomen
   integer            :: ntypa2=0                 ! actueel aantal algen = copy NTYP_A
   integer            :: iblsub(ntypm2)           ! stofnummer bloomalgen
   real(8)            :: ncralg(ntypm2)           ! N-C ratio voor de algen = copy van ALGTYP(4,IALG)
   real(8)            :: pcralg(ntypm2)           ! P-C ratio voor de algen = copy van ALGTYP(5,IALG)
end module bloom_data_mass_balance

module bloom_data_caldynam
   use bloom_data_dim
   real(8)            :: detemp
   real(8)            :: decon
   real(8)            :: demult
   real(8)            :: pmfrac
   real(8)            :: flush
   integer            :: lcal
   real(8)            :: xflush(mx)
   real(8)            :: spexde
   real(8)            :: tstep
   integer            :: inexde
   integer            :: ldyn
end module bloom_data_caldynam

module bloom_data_io
   use bloom_data_dim
   character(1)       :: string(48)
   character(8)       :: line(10)
   character(16)      :: cnames(ia)
   integer            :: iou(99)
   integer            :: posit
   integer            :: inuni
   integer            :: ouuni
   integer            :: opl
   integer            :: ioflag
   integer            :: ipl1
   integer            :: ipl2
   integer            :: ipl3
   integer            :: lplot
   integer            :: lscr
   integer            :: lenstr
end module bloom_data_io

module bloom_data_matrix
   use bloom_data_dim
   real(8)            :: a(ia,mt)
   real(8)            :: b(ia)
   real(8)            :: c(mt)
   real(8)            :: aco(mt,mt)
   real(8)            :: bgro(ms)
   integer            :: isplim(mt)
end module bloom_data_matrix

module bloom_data_phyt
   use bloom_data_dim
   real(8)            :: aa(mn,mt)
   real(8)            :: ekx(mt)
   real(8)            :: dmix(mt)
   real(8)            :: chlr(mt)
   real(8)            :: rnut(2,mn)
   real(8)            :: reminu(mn)
   real(8)            :: remexp(mn)
   real(8)            :: detrit(mn)
   real(8)            :: concen(mn)
   real(8)            :: remili(2)
   real(8)            :: availn(mt)
   real(8)            :: sedrat
   real(8)            :: remior
   real(8)            :: qmrem
   character(10)      :: grname(ms)
   character(10)      :: spname(mt)
   character(10)      :: cstra(mn+2)
   integer            :: it2(ms,2)
   integer            :: nspf(mt)
   integer            :: nsf
   integer            :: nrep
   integer            :: nuspec
   integer            :: nuecog
   integer            :: nunuco
   integer            :: nucols
   integer            :: nufili
   integer            :: nuabco
   integer            :: nuexro
   integer            :: nurows
   integer            :: nuspe1
   integer            :: idump
end module bloom_data_phyt

module bloom_data_putin
   use bloom_data_dim
   real(8)            :: death(52)
   real(8)            :: phyt(52)
   real(8)            :: bnut(mn)
   real(8)            :: dnut(mn)
   real(8)            :: backmu
   real(8)            :: backad
   real(8)            :: tempmu
   real(8)            :: tempad
   real(8)            :: solamu
   real(8)            :: solaco
   real(8)            :: solaad
   real(8)            :: deptmu
   real(8)            :: deptad
   real(8)            :: dlgtmu
   real(8)            :: dlgtad
   integer            :: nper(10,3)
   integer            :: nrun
   integer            :: imu
   integer            :: ml
   integer            :: mu
   integer            :: mi
   integer            :: lrun
   integer            :: lstop
   integer            :: iyear
   character(8)       :: date(52)
   character(8)       :: com(18)
   character(8)       :: case(13)
   character(8)       :: contro(20)
end module bloom_data_putin

module bloom_data_sumou
   use bloom_data_dim
   real(8)            :: pardis(12)
   real(8)            :: xst(mx)
   real(8)            :: biost
   real(8)            :: xsum(mt)
   real(8)            :: chlsum
   real(8)            :: obssum
   real(8)            :: chi2
   character(18)      :: limit
   character(3)       :: limnam(mn+3)
   integer            :: nobs
   integer            :: nunu2
   integer            :: nunu4
   integer            :: nts6
   integer            :: nts7
   integer            :: nts14
   integer            :: intst
   integer            :: lst
   integer            :: nprint
   integer            :: ldom
   integer            :: lgbase
   integer            :: lprint
   integer            :: isdump
   integer            :: isdper(2)
end module bloom_data_sumou

module bloom_data_xvect
   use bloom_data_dim
   real(8)            :: xdef(mx+1)
   real(8)            :: xinit(ms)
   real(8)            :: xeco(mt)
end module bloom_data_xvect
