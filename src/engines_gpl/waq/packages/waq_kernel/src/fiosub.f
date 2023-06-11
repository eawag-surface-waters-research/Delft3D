!!  Copyright (C)  Stichting Deltares, 2012-2023.
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

      subroutine fiosub (outval, iopoin, nrvar , nocons, nopa  ,
     +                   nofun , nosfun, notot , conc  , segfun,
     +                   func  , param , cons  , idt   , itime ,
     +                   volume, noseg , nosys , ndmpar, ipdmp ,
     +                   bound , noloc , proloc, nodef , defaul,
     +                   ncout , ntdmpq, paname, sfname, funame,
     +                   danam )
      use m_zoek
      use m_zero
      use timers

      implicit none
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:            : may 1993 by Jan van Beek
!
!     FUNCTION            : Fills output buffer OUTVAL on sub grid.
!
!     SUBROUTINES CALLED  : ZERO  , zero's a real array
!
!     FILES               : -
!
!     PARAMETERS          : 27
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     OUTVAL  REAL  NCOUT+NRVAR,* OUTPUT  Values for vars on output grid
!     IOPOIN  INTEGER       *     INPUT   Pointers to arrays for vars
!     NRVAR   INTEGER       1     INPUT   Number of extra output vars
!     NOCONS  INTEGER       1     INPUT   Number of constants used
!     NOPA    INTEGER       1     INPUT   Number of parameters
!     NOFUN   INTEGER       1     INPUT   Number of functions ( user )
!     NOSFUN  INTEGER       1     INPUT   Number of segment functions
!     NOTOT   INTEGER       1     INPUT   Total number of substances
!     CONC    REAL   NOTOT,NOSEG  INPUT   Model concentrations
!     SEGFUN  REAL   NOSEG,NOSFUN INPUT   Segment functions at ITIME
!     FUNC    REAL          *     INPUT   Model functions at ITIME
!     PARAM   REAL    NOPA,NOSEG  INPUT   Model parameters
!     CONS    REAL          *     INPUT   Model constants
!     IDT     INTEGER       1     INPUT   Simulation timestep
!     ITIME   INTEGER       1     INPUT   Time in system clock units
!     VOLUME  REAL      NOSEG     INPUT   Segment volumes
!     NOSEG   INTEGER       1     INPUT   Nr. of computational elements
!     NOSYS   INTEGER       1     INPUT   Number of active substances
!     NDMPAR  INTEGER       1     INPUT   number of dump locations
!     IPDMP   INTEGER       *     INPUT   pointer structure dump area's
!     BOUND   REAL     NOTOT*?    INPUT   boundary      values
!     NOLOC   INTEGER       1     INPUT   Number of variables in PROLOC
!     PARAM   REAL   NOLOC,NOSEG  INPUT   Parameters local in PROCES system
!     NODEF   INTEGER       1     INPUT   Number of used defaults
!     DEFAUL  REAL          *     INPUT   Default proces parameters
!     NCOUT   INTEGER       1     INPUT   number of conc in output
!     NTDMPQ  INTEGER       1     INPUT   total number exchanges in dump area
!
!     Declaration of arguments
!
      integer    nrvar , nocons, nopa  , nofun , nosfun,
     +           notot , idt   , itime , noseg , nosys ,
     +           ndmpar, noloc , nodef , ncout , ntdmpq
      integer    iopoin(*)      , ipdmp(*)
      real       outval(*)      , conc(notot,*),
     +           segfun(noseg,nosfun), func(*) ,
     +           param (nopa ,noseg ), cons(*) ,
     +           volume(*)      , bound(*)     ,
     +           proloc(*)      , defaul(*)
      character(20) paname(*) , sfname(*)
      character(len=20), intent(in   ) :: funame(*) ! function names
      character(len=20), intent(in   ) :: danam(*)  ! dump area names
!
!     Local
!
      real, parameter    :: rmiss = -999.
      integer, parameter :: nopred= 6     
      integer     iopa  , iofunc, iosfun, ioconc, ioloc ,
     +            iodef , ip    , ip1   , ip2   , itel2 ,
     +            isys  , ivar  , idump , isc   , iseg  ,
     +            nsc   , iofdmp, iocons, iip   , iidump,
     +            indx 
      integer  :: ifun   ! index in function arrays
      real        hlpvar, hlpcum, valcum, valvar, srf, cumsrf
      logical     parm
      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "fiosub", ithandl )
!
!     Pointer offsets
!
      iocons = nopred + 1
      iopa   = iocons + nocons
      iofunc = iopa   + nopa
      iosfun = iofunc + nofun
      ioconc = iosfun + nosfun
      ioloc  = ioconc + notot
      iodef  = ioloc  + noloc
!
!     Zero the output buffer for it is used as accumulation variable.
!
      call zero(outval,(ncout+nrvar)*ndmpar)
!
!     Fill the output buffer OUTVAL
!
      ip1   = ndmpar + ntdmpq
      itel2 = ndmpar + ntdmpq + ndmpar
!
!     Loop over the dump area's
!
      do idump = 1 , ndmpar
!
!        the segment contributes
!
         nsc = ipdmp(ip1+idump)
         iofdmp = (idump-1)*(ncout+nrvar)
!
!        If one segment don't bother to calculate mean value
!
         if ( nsc .eq. 1 ) then
            itel2 = itel2 + 1
            iseg  = ipdmp(itel2)
            if ( danam(idump)(1:6) .eq. 'MOVING' ) then
               do ifun = 1, nofun
                  if ( danam(idump) .eq. funame(ifun) ) then
                     iseg = nint(func(ifun))
                  endif
               enddo
            endif
!
!           The substances
!
            if ( ncout .gt. 0 ) then
               if ( iseg .lt. 0 ) then
                  do isys = 1 , nosys
                     iidump = iofdmp+isys
                     iip = (-iseg-1)*nosys + isys
                     outval(iidump) = bound(iip)
                  enddo
                  do isys = nosys+1 , notot
                     iidump = iofdmp+isys
                     outval(iidump) = rmiss
                  enddo
               elseif ( iseg .eq. 0 ) then
                  do isys = 1 , notot
                     iidump = iofdmp+isys
                     outval(iidump) = rmiss
                  enddo
               else
                  do isys = 1 , notot
                     iidump = iofdmp+isys
                     outval(iidump) = conc(isys,iseg)
                  enddo
               endif
            endif
!
!           The extra variables
!
            do ivar = 1 , nrvar
               ip = iopoin(ivar)
               iidump = iofdmp+ncout+ivar
               if ( iseg .lt. 0 ) then
                  if ( ip .ge. ioconc .and. ip .lt. ioconc+nosys ) then
                     iip = (-iseg-1)*nosys + ip-ioconc+1
                     outval(iidump) = bound(iip)
                  else
                     outval(iidump) = rmiss
                  endif
               elseif ( iseg .eq. 0 ) then
                  outval(iidump) = rmiss
               else
                  if ( ip .ge. iodef  ) then
                     outval(iidump) = defaul(ip-iodef+1)
                  elseif ( ip .ge. ioloc  ) then
                     iip = (iseg-1)*noloc + ip-ioloc+1
                     outval(iidump) = proloc(iip)
                  elseif ( ip .ge. ioconc ) then
                     outval(iidump) = conc(ip-ioconc+1,iseg)
                  elseif ( ip .ge. iosfun ) then
                     outval(iidump) = segfun(iseg,ip-iosfun+1)
                  elseif ( ip .ge. iofunc ) then
                     outval(iidump) = func(ip-iofunc+1)
                  elseif ( ip .ge. iopa ) then
                     outval(iidump) = param(ip-iopa+1,iseg)
                  elseif ( ip .ge. iocons ) then
                     outval(iidump) = cons(ip-iocons+1)
                  elseif ( ip .eq. 3 ) then
                     outval(iidump) = real(idt)
                  elseif ( ip .eq. 2 ) then
                     outval(iidump) = real(itime)
                  elseif ( ip .eq. 1 ) then
                     outval(iidump) = volume(iseg)
                  elseif ( ip .le. 0 ) then
                     outval(iidump) = rmiss
                  endif
               endif
!
            enddo
!
         else
!
!           The substances ( if asked ) in one loop
!
            if ( ncout .gt. 0 ) then
!
!              Zero the accummulative variables, OUTVAL already done.
!
               hlpcum = 0.0
               cumsrf = 0.0
               if ( nosys .ne. notot ) then
                  cumsrf = 1.0
                  call zoek20 ( 'SURF      ', nopa, paname, 10, indx )
                  if  ( indx .gt. 0 ) then
                     cumsrf = 0.0
                     parm = .true.
                  else
                     call zoek20 ( 'SURF      ', nosfun, sfname, 10, indx )
                     if  ( indx .gt. 0 ) then
                        cumsrf = 0.0
                        parm = .false.
                     endif
                  endif
               endif
!
               do isc = 1 , nsc
                  iseg  = ipdmp(itel2+isc)
!
!                 Accumulate VOLUME, substances in OUTVAL
!
                  if ( iseg .gt. 0 ) then
                     hlpvar = volume(iseg)
                     hlpcum = hlpcum + hlpvar
!
!                    All active substances weighted by volume
!
                     do isys = 1 , nosys
                        iidump = iofdmp+isys
                        outval(iidump) = outval(iidump) +
     +                                   conc(isys,iseg)*hlpvar
                     enddo

!       take care of mass/m2 for inactive substances if possible

                     if ( nosys .ne. notot ) then
                        if  ( indx .gt. 0 ) then
                           if ( parm ) then
                              srf = param (indx,iseg)
                           else
                              srf = segfun(iseg,indx)
                           endif
                           cumsrf = cumsrf + srf
                           do isys = nosys+1, notot
                              iidump = iofdmp+isys
                              outval(iidump) = outval(iidump)+conc(isys,iseg)*srf
                           enddo
                        else
                           do isys = nosys+1, notot
                              iidump = iofdmp+isys
                              outval(iidump) = outval(iidump)+conc(isys,iseg)
                           enddo
                        endif
                     endif
                  endif
!
               enddo
!
!              Calculate mean for then active and inactive substances
!
               if ( abs(hlpcum) .gt. 1.0e-20 ) then
                  do isys = 1 , nosys
                     iidump = iofdmp+isys
                     outval(iidump) = outval(iidump) / hlpcum
                  enddo
               endif
               if ( abs(cumsrf) .gt. 1.0e-20 ) then
                  do isys = nosys+1, notot
                     iidump = iofdmp+isys
                     outval(iidump) = outval(iidump) / cumsrf
                  enddo
               endif
!
            endif
!
!           The extra output variables each in a seperate loop
!
            do ivar = 1 , nrvar
!
!              Accumulate
!
               ip  = iopoin(ivar)
               ip2 = iopoin(nrvar+ivar)
               valcum = 0.0
               hlpcum = 0.0
               do isc = 1 , nsc
                  iseg  = ipdmp(itel2+isc)
!
!                 The output variable
!
                  if ( iseg .gt. 0 ) then
                     ip = iopoin(ivar)
                     if ( iseg .lt. 0 ) then
                        if ( ip.ge.ioconc .and. ip.lt.ioconc+nosys ) then
                           iip = (-iseg-1)*nosys + ip-ioconc+1
                           valvar = bound(iip)
                        else
                           valvar = 0.0
                        endif
                     elseif ( iseg .eq. 0 ) then
                        valvar = 0.0
                     else
                        if ( ip .ge. iodef  ) then
                           valvar = defaul(ip-iodef+1)
                        elseif ( ip .ge. ioloc  ) then
                           iip = (iseg-1)*noloc + ip-ioloc+1
                           valvar = proloc(iip)
                        elseif ( ip .ge. ioconc ) then
                           valvar = conc(ip-ioconc+1,iseg)
                        elseif ( ip .ge. iosfun ) then
                           valvar = segfun(iseg,ip-iosfun+1)
                        elseif ( ip .ge. iofunc ) then
                           valvar = func(ip-iofunc+1)
                        elseif ( ip .ge. iopa ) then
                           valvar = param(ip -iopa+1,iseg)
                        elseif ( ip .ge. iocons ) then
                           valvar = cons(ip-iocons+1)
                        elseif ( ip .eq. 3 ) then
                           valvar = real(idt)
                        elseif ( ip .eq. 2 ) then
                           valvar = real(itime)
                        elseif ( ip .eq. 1 ) then
                           valvar = volume(iseg)
                        elseif ( ip .le. 0 ) then
                           valvar = rmiss
                        endif
                     endif
!
!                    The weigth variable
!
                     if ( iseg .lt. 0 ) then
                        hlpvar = 1.0
                     elseif ( iseg .eq. 0 ) then
                        hlpvar = 0.0
                     else
                        if ( ip2 .ge. iodef  ) then
                           hlpvar = defaul(ip2-iodef+1)
                        elseif ( ip2 .ge. ioloc  ) then
                           iip = (iseg-1)*noloc + ip2-ioloc+1
                           hlpvar = proloc(iip)
                        elseif ( ip2 .ge. ioconc ) then
                           hlpvar = conc(ip2-ioconc+1,iseg)
                        elseif ( ip2 .ge. iosfun ) then
                           hlpvar = segfun(iseg,ip2-iosfun+1)
                        elseif ( ip2 .ge. iofunc ) then
                           hlpvar = func(ip2-iofunc+1)
                        elseif ( ip2 .ge. iopa ) then
                           hlpvar = param(ip2-iopa+1,iseg)
                        elseif ( ip2 .ge. iocons ) then
                           hlpvar = cons(ip2-iocons+1)
                        elseif ( ip2 .eq. 3 ) then
                           hlpvar = real(idt)
                        elseif ( ip2 .eq. 2 ) then
                           hlpvar = real(itime)
                        elseif ( ip2 .eq. 1 ) then
                           hlpvar = volume(iseg)
                        elseif ( ip2 .eq. 0 ) then
                           hlpvar = 0.0
                        elseif ( ip2 .lt. 0 ) then
                           hlpvar = 1.
                        endif
                     endif
!
                     if ( ip2 .eq. 0 ) then
                        valcum = valcum + valvar
                     else
                        valcum = valcum + valvar * hlpvar
                        hlpcum = hlpcum + hlpvar
                     endif
                  endif
!
               enddo
!
!              Calculate mean , HLPCUM = 0.0 has a double meaning
!              1. only accumulation, 2. no divide by zero HLPCUM
!
               iidump = iofdmp+ncout+ivar
!
               if ( abs(hlpcum) .gt. 1.0e-20 ) then
                  outval(iidump) = valcum / hlpcum
               else
                  outval(iidump) = valcum
               endif
!
            enddo
!
            itel2 = itel2 + nsc
         endif
!
      enddo
!
      if ( timon ) call timstop ( ithandl )
      return
      end
