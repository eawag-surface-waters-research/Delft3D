!!  Copyright(C) Stichting Deltares, 2012-2013.
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

!!  Note: The "part" engine is not yet Open Source, but still under
!!  development. This package serves as a temporary dummy interface for
!!  the references in the "waq" engine to the "part" engine.

module partmem

      use precision
      use typos

      integer(ip)  , parameter          :: nfilesp =  23
      integer(ip)                       :: lunitp(nfilesp) = 0
      character(len=256)                :: fnamep(nfilesp) = ' '
      character(len=20) , dimension(2)  :: ftypep          = ' '
      logical                           :: alone
      integer(ip)   :: itrakc  , itraki  , npwndn  , npwndw  , nstep
      real   (sp)   :: defang  , hmin    , ptlay   , accrjv
      logical       :: oil     , oil2dh  , oil3d   , ltrack  , acomp  , fout

      integer  ( ip)           :: bufsize
      integer  ( ip)           :: nosub_max
      integer  ( ip)           :: nmaxp
      integer  ( ip)           :: mmaxp
      integer  ( ip)           :: mnmax2
      integer  ( ip)           :: layt
      integer  ( ip)           :: mnmaxk
      integer  ( ip)           :: nflow
      integer  ( ip)           :: noseglp
      integer  ( ip)           :: nosegp
      integer  ( ip)           :: noqp
      integer  ( ip)           :: ihdel
      character( 40)           :: title(4)
      integer  ( ip)           :: modtyp
      integer  ( ip)           :: notrak
      logical  ( ip)           :: lsettl
      integer  ( ip)           :: nolayp
      integer  ( ip)           :: noslay
      integer  ( ip)           :: idelt
      integer  ( ip)           :: ipc
      logical  ( ip)           :: lcorr
      integer  ( ip)           :: ioptdv
      real     ( rp)           :: alpha
      real     ( rp)           :: cdisp
      real     ( rp)           :: dminim
      logical  ( ip)           :: ldiffz
      integer  ( ip)           :: nosubs
      integer  ( ip)           :: nosubc
      integer  ( ip)           :: nfract
      real     ( rp)           :: pblay
      integer  ( ip)           :: itrack
      integer  ( ip)           :: ntrack
      integer  ( ip)           :: nstick
      character(256)           :: finnh4
      character(256)           :: finno3
      integer  ( ip)           :: nopart
      integer  ( ip)           :: npmax
      integer  ( ip)           :: npolmax
      real     ( rp)           :: rough
      real     ( rp)           :: drand  (3)
      logical  ( ip)           :: spawnd
      integer  ( ip)           :: nowind
      integer  ( ip)           :: noconsp
      integer  ( ip)           :: itstrtp
      integer  ( ip)           :: itstopp
      integer  ( ip)           :: iddtim
      integer  ( ip)           :: icwsta
      integer  ( ip)           :: icwsto
      integer  ( ip)           :: icwste
      integer  ( ip)           :: ihstrtp
      integer  ( ip)           :: ihstopp
      integer  ( ip)           :: ihstepp
      integer  ( ip)           :: iyear
      integer  ( ip)           :: imonth
      integer  ( ip)           :: iofset
      logical  ( ip)           :: ldiffh
      real     ( rp)           :: rhow
      integer  ( ip)           :: stickdf
      integer  ( ip)           :: ini_opt
      character(256)           :: ini_file
      integer  ( ip)           :: nosta
      integer  ( ip)           :: iptset
      real     ( rp)           :: window(4)
      integer  ( ip)           :: mmap
      integer  ( ip)           :: nmap
      integer  ( ip)           :: nodye
      integer  ( ip)           :: nocont
      integer  ( ip)           :: noudef
      integer  ( ip)           :: idtset
      real     ( rp)           :: anfac
      integer  ( ip)           :: irfac
      integer  ( ip)           :: nrowsmax
      integer  ( ip)           :: ivtset
      real     ( rp)           :: chezy
      real     ( rp)           :: taucs
      real     ( rp)           :: tauce
      logical                  :: caltau

      integer  ( ip), pointer  :: lgrid (:,:)
      integer  ( ip), pointer  :: lgrid2(:,:)
      integer  ( ip), pointer  :: lgrid3(:,:)
      real     ( rp), pointer  :: tcktot (:)
      integer  ( ip), pointer  :: cellpntp(:)
      integer  ( ip), pointer  :: flowpntp(:,:)
      real     ( rp), pointer  :: angle  (:)
      real     ( rp), pointer  :: area   (:)
      real     ( rp), pointer  :: depth  (:)
      real     ( rp), pointer  :: dpsp   (:)
      real     ( rp), pointer  :: dx     (:)
      real     ( rp), pointer  :: dy     (:)
      real     ( rp), pointer  :: flow   (:)
      real     ( rp), pointer  :: flow1  (:)
      integer  ( ip), pointer  :: ipntp  (:)
      integer  ( ip), pointer  :: nplay  (:)
      real     ( rp), pointer  :: vdiff  (:)
      real     ( rp), pointer  :: vdiff1 (:)
      real     ( rp), pointer  :: tau    (:)
      real     ( rp), pointer  :: tau1   (:)
      real     ( rp), pointer  :: salin  (:)
      real     ( rp), pointer  :: salin1 (:)
      real     ( rp), pointer  :: temper (:)
      real     ( rp), pointer  :: temper1(:)
      real     ( rp), pointer  :: velo   (:)
      real     ( rp), pointer  :: vol1   (:)
      real     ( rp), pointer  :: vol2   (:)
      real     ( rp), pointer  :: volumep(:)
      real     ( rp), pointer  :: xb     (:)
      real     ( rp), pointer  :: yb     (:)
      real     ( rp), pointer  :: zlevel (:)
      real     ( rp), pointer  :: locdep(:,:)
      character( 20), pointer  :: substi (:)
      integer  ( ip), pointer  :: mapsub (:)
      integer  ( ip), pointer  :: nplot  (:)
      integer  ( ip), pointer  :: mstick (:)
      character( 20), pointer  :: subst  (:)
      character( 20), pointer  :: subst2 (:)
      real     ( rp), pointer  :: wveloa (:)
      real     ( rp), pointer  :: wdira  (:)
      real     ( dp), pointer  :: wvelo  (:)
      real     ( dp), pointer  :: wdir   (:)
      integer  ( ip), pointer  :: iwndtm (:)
      real     ( rp), pointer  :: const  (:)
      character( 20), pointer  :: nmstat (:)
      real     ( rp), pointer  :: xstat  (:)
      real     ( rp), pointer  :: ystat  (:)
      integer  ( ip), pointer  :: ipset  (:)
      real     ( rp), pointer  :: recovr (:)
      character( 20), pointer  :: nmdyer (:)
      integer  ( ip), pointer  :: iwtime (:)
      real     ( rp), pointer  :: xwaste (:)
      real     ( rp), pointer  :: ywaste (:)
      real     ( rp), pointer  :: zwaste (:)
      integer  ( ip), pointer  :: kwaste (:)
      integer  ( ip), pointer  :: ioptrad(:)
      real     ( rp), pointer  :: radius (:)
      real     ( rp), pointer  :: wparm  (:)
      integer  ( ip), pointer  :: ndprt  (:)
      real     ( rp), pointer  :: amassd(:,:)
      character( 20), pointer  :: nmconr (:)
      integer  ( ip), pointer  :: linear (:)
      real     ( rp), pointer  :: stoch (:,:)
      integer  ( ip), pointer  :: ictmax (:)
      integer  ( ip), pointer  :: ictime(:,:)
      real     ( rp), pointer  :: amassc(:,:,:)
      real     ( rp), pointer  :: ftime (:,:)
      real     ( rp), pointer  :: uscal  (:)
      integer  ( ip), pointer  :: isubud (:)
      integer  ( ip), pointer  :: iutime (:)
      integer  ( ip), pointer  :: ifopt  (:)
      character(256), pointer  :: finud  (:)
      integer  ( ip), pointer  :: iftime (:)
      integer  ( ip), pointer  :: nosud  (:)
      integer  ( ip), pointer  :: isfud  (:)
      integer  ( ip), pointer  :: idtime (:)
      real     ( rp), pointer  :: decay (:,:)
      real     ( rp), pointer  :: decays (:)
      integer  ( ip), pointer  :: ivtime (:)
      real     ( rp), pointer  :: wpart (:,:)
      real     ( rp), pointer  :: vsfour(:,:,:)
      real     ( rp), pointer  :: wsettl (:)
      integer  ( ip)              nbmax
      integer  ( ip)              ndoms
      type  (domain), pointer  :: doms   (:)
      integer  ( ip)              nbnds
      type  (boundp), pointer  :: bnds   (:)
      integer  ( ip)              nconn
      type  (pnt   ), pointer  :: conn   (:)
      integer  ( ip)              npgrid
      type  (PlotGrid),pointer :: pg     (:)

      real     ( rp), pointer  :: t0cf   (:)
      real     ( rp), pointer  :: tmassu (:)
      real     ( rp), pointer  :: acf    (:)
      integer  ( ip), pointer  :: ncheck (:)
      real     ( rp), pointer  :: rem    (:)
      real     ( rp), pointer  :: tmassc(:,:)
      real     ( rp), pointer  :: aconc (:,:)
      character     (len=20   ) ,  pointer, dimension(:       ) :: cbuff
      character     (len=20   ) ,  pointer, dimension(:       ) :: subsud
      integer       (sp       ) ,  pointer, dimension(:       ) :: floil
      integer       (sp       ) ,  pointer, dimension(:       ) :: ihplot
      integer       (sp       ) ,  pointer, dimension(:       ) :: iptime
      integer       (sp       ) ,  pointer, dimension(:       ) :: isfile
      integer       (sp       ) ,  pointer, dimension(:       ) :: kpart
      integer       (sp       ) ,  pointer, dimension(:       ) :: mpart
      integer       (sp       ) ,  pointer, dimension(:       ) :: mpart0
      integer       (sp       ) ,  pointer, dimension(:       ) :: mplsta
      integer       (sp       ) ,  pointer, dimension(:       ) :: mstat
      integer       (sp       ) ,  pointer, dimension(:       ) :: mwaste
      integer       (sp       ) ,  pointer, dimension(:       ) :: npart
      integer       (sp       ) ,  pointer, dimension(:       ) :: npart0
      integer       (sp       ) ,  pointer, dimension(:       ) :: nplsta
      integer       (sp       ) ,  pointer, dimension(:       ) :: nstat
      integer       (sp       ) ,  pointer, dimension(:       ) :: nwaste
      integer       (sp       ) ,  pointer, dimension(:,:     ) :: imap
      integer       (sp       ) ,  pointer, dimension(:,:     ) :: imask
      integer       (sp       ) ,  pointer, dimension(:,:     ) :: ibuff
      integer       (sp       ) ,  pointer, dimension(:       ) :: isub
      integer       (sp       ) ,  pointer, dimension(:,:     ) :: mcell
      integer       (sp       ) ,  pointer, dimension(:,:     ) :: ncell
      integer       (sp       ) ,  pointer, dimension(:,:,:   ) :: nbin
      integer       (sp       ) ,  pointer, dimension(:       ) :: nosyss
      real          (sp       ) ,  pointer, dimension(:       ) :: abuoy
      real          (sp       ) ,  pointer, dimension(:       ) :: dfact
      real          (sp       ) ,  pointer, dimension(:       ) :: fstick
      real          (sp       ) ,  pointer, dimension(:       ) :: t0buoy
      real          (sp       ) ,  pointer, dimension(:       ) :: tmass
      real          (sp       ) ,  pointer, dimension(:       ) :: xa
      real          (sp       ) ,  pointer, dimension(:       ) :: xa0
      real          (sp       ) ,  pointer, dimension(:       ) :: xpart
      real          (sp       ) ,  pointer, dimension(:       ) :: xpart0
      real          (sp       ) ,  pointer, dimension(:       ) :: ya
      real          (sp       ) ,  pointer, dimension(:,:     ) :: tmasud
      real          (sp       ) ,  pointer, dimension(:       ) :: ya0
      real          (sp       ) ,  pointer, dimension(:       ) :: ypart
      real          (sp       ) ,  pointer, dimension(:       ) :: ypart0
      real          (sp       ) ,  pointer, dimension(:       ) :: za
      real          (sp       ) ,  pointer, dimension(:       ) :: zpart
      real          (sp       ) ,  pointer, dimension(:,:     ) :: aconud
      real          (sp       ) ,  pointer, dimension(:,:     ) :: adepth
      real          (sp       ) ,  pointer, dimension(:,:     ) :: apeak
      real          (sp       ) ,  pointer, dimension(:,:     ) :: atotal
      real          (sp       ) ,  pointer, dimension(:,:     ) :: rbuff
      real          (sp       ) ,  pointer, dimension(:,:     ) :: track
      real          (sp       ) ,  pointer, dimension(:,:     ) :: vrtdsp
      real          (sp       ) ,  pointer, dimension(:,:     ) :: vsfact
      real          (sp       ) ,  pointer, dimension(:,:     ) :: xyztrk
      real          (sp       ) ,  pointer, dimension(:,:,:   ) :: atrack
      real          (sp       ) ,  pointer, dimension(:,:,:   ) :: chismp
      real          (sp       ) ,  pointer, dimension(:,:,:   ) :: chispl
      real          (sp       ) ,  pointer, dimension(:,:,:,: ) :: amap
      real          (sp       ) ,  pointer, dimension(:,:,:,: ) :: amapsett
      real          (sp       ) ,  pointer, dimension(:       ) :: xpol
      real          (sp       ) ,  pointer, dimension(:       ) :: ypol
      character     (len=16   ) ,  pointer, dimension(:       ) :: elt_names
      character     (len=16   ) ,  pointer, dimension(:       ) :: elt_types
      integer       (sp       ) ,  pointer, dimension(:       ) :: elt_bytes
      integer       (sp       ) ,  pointer, dimension(:,:     ) :: elt_dims
      real          (sp       ) ,  pointer, dimension(:       ) :: rbuffr
      real          (sp       ) ,  pointer, dimension(:,:     ) :: concp
      real          (sp       ) ,  pointer, dimension(:,:     ) :: flres

end module partmem
