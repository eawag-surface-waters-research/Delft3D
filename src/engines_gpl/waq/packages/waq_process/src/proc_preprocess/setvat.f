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

      SUBROUTINE SETVAT ( LUREP , NOCONS, NOPA  , NOFUN , NOSFUN,
     +                    NOSYS , NOTOT , NODISP, NOVELO, NODEF ,
     +                    NOLOC , NDSPX , NVELX , NLOCX , NFLUX ,
     +                    NOPRED, NOVAR , VARARR, VARIDX, VARTDA,
     +                    VARDAG, VARTAG, VARAGG, NOGRID, CONAME,
     +                    PANAME, FUNAME, SFNAME, DENAME, SYNAME,
     +                    LOCNAM, VARNAM)
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:            : Jan van Beek
!
!     FUNCTION            : Set variable atributes
!
!     SUBROUTINES CALLED  :
!
!     FILES               :
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!
!     Declaration of arguments
!
      use m_zoek
      use m_srstop
      use m_monsys
      use timers       !   performance timers

      INTEGER             LUREP , NOCONS, NOPA  , NOFUN , NOSFUN,
     +                    NOSYS , NOTOT , NODISP, NOVELO, NODEF ,
     +                    NOLOC , NDSPX , NVELX , NLOCX , NFLUX ,
     +                    NOPRED, NOVAR
      INTEGER             VARARR(NOVAR) , VARIDX(NOVAR) ,
     +                    VARTDA(NOVAR) , VARDAG(NOVAR) ,
     +                    VARTAG(NOVAR) , VARAGG(NOVAR)
      CHARACTER*20        VARNAM(NOVAR)
!
!     Locals
!
      PARAMETER ( MAXLOC = 2000 )
      INTEGER      VATTAG(MAXLOC), VATTDA(MAXLOC)
      CHARACTER*20 VATNAM(MAXLOC), VATNAG(MAXLOC),
     +             VATNDA(MAXLOC)
      CHARACTER*20 CONAME(*)     , PANAME(*)     ,
     +             FUNAME(*)     , SFNAME(*)     ,
     +             SYNAME(*)     , LOCNAM(*)     ,
     +             DENAME(*)
      CHARACTER*79 LINE, NAME
      LOGICAL      LEXI
      INTEGER      LUN
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "setvat", ithndl )
!
!     Just take the used array's in the right order
!
      IIVOL  =  1
      IIAREA =  2
      IIFLOW =  3
      IILENG =  4
      IIDISP =  5
      IICONC =  6
      IIMASS =  7
      IIDERV =  8
      IIBOUN =  9
      IIBSET = 10
      IIBSAV = 11
      IIWSTE = 12
      IICONS = 13
      IIPARM = 14
      IIFUNC = 15
      IISFUN = 16
      IIDNEW = 17
      IIDIFF = 18
      IIVNEW = 19
      IIVELO = 20
      IIHARM = 21
      IIFARR = 22
      IIMAS2 = 23
      IITIMR = 24
      IIVOL2 = 25
      IITRAC = 26
      IIGWRK = 27
      IIGHES = 28
      IIGSOL = 29
      IIGDIA = 30
      IIGTRI = 31
      IISMAS = 32
      IIPLOC = 33
      IIDEFA = 34
      IIFLUX = 35
      IISTOC = 36
      IIFLXD = 37
      IIFLXI = 38
      IIRIOB = 39
      IIDSPX = 40
      IIVELX = 41
      IILOCX = 42
      IIDSTO = 43
      IIVSTO = 44
      IIDMPQ = 45
      IIDMPS = 46
      IITRRA = 47
      IINRSP = 48
      IIVOLL = 49
      IIVOL3 = 50
      IIR1   = 51
      IIQXK  = 52
      IIQYK  = 53
      IIQZK  = 54
      IIDIFX = 55
      IIDIFY = 56
      IIDIFZ = 57
      IIVOLA = 58
      IIVOLB = 59
      IIGUV  = 60
      IIGVU  = 61
      IIGZZ  = 62
      IIAAK  = 63
      IIBBK  = 64
      IICCK  = 65
      IIBD3X = 66
      IIBDDX = 67
      IIBDX  = 68
      IIBU3X = 69
      IIBUUX = 70
      IIBUX  = 71
      IIWRK1 = 72
      IIWRK2 = 73
      IIAAKL = 74
      IIBBKL = 75
      IICCKL = 76
      IIDDKL = 77
!
      IVVOL = 1
      IVARE = IVVOL + 1
      IVFLO = IVARE + 1
      IVLEN = IVFLO + 1
      IVCNS = IVLEN + 2
      IVPAR = IVCNS + NOCONS
      IVFUN = IVPAR + NOPA
      IVSFU = IVFUN + NOFUN
      IVCNC = IVSFU + NOSFUN
      IVMAS = IVCNC + NOTOT
      IVDER = IVMAS + NOTOT
      IVDSP = IVDER + NOTOT
      IVVEL = IVDSP + NODISP
      IVDEF = IVVEL + NOVELO
      IVLOC = IVDEF + NODEF
      IVDSX = IVLOC + NOLOC
      IVVLX = IVDSX + NDSPX
      IVLCX = IVVLX + NVELX
      IVFLX = IVLCX + NLOCX
!
!
!
!
!     Volume
!
      IVAR = 1
      VARNAM(IVAR) = 'VOLUME'
      VARARR(IVAR) = IIVOL
      VARIDX(IVAR) = 1
      VARTDA(IVAR) = 0
      VARDAG(IVAR) = 0
      VARTAG(IVAR) = 1
      VARAGG(IVAR) = 0
!
!     Area
!
      IVAR = IVAR + 1
      VARNAM(IVAR) = 'XAREA'
      VARARR(IVAR) = IIAREA
      VARIDX(IVAR) = 1
      VARTDA(IVAR) = 0
      VARDAG(IVAR) = 0
      VARTAG(IVAR) = 0
      VARAGG(IVAR) = 0
!
!     Flow
!
      IVAR = IVAR + 1
      VARNAM(IVAR) = 'FLOW'
      VARARR(IVAR) = IIFLOW
      VARIDX(IVAR) = 1
      VARTDA(IVAR) = 0
      VARDAG(IVAR) = 0
      VARTAG(IVAR) = 0
      VARAGG(IVAR) = 0
!
!     Length , two length
!
      IVAR = IVAR + 1
      VARNAM(IVAR) = 'XLENFROM'
      VARARR(IVAR) = IILENG
      VARIDX(IVAR) = 1
      VARTDA(IVAR) = 0
      VARDAG(IVAR) = 0
      VARTAG(IVAR) = 0
      VARAGG(IVAR) = 0
      IVAR = IVAR + 1
      VARNAM(IVAR) = 'XLENTO'
      VARARR(IVAR) = IILENG
      VARIDX(IVAR) = 2
      VARTDA(IVAR) = 0
      VARDAG(IVAR) = 0
      VARTAG(IVAR) = 0
      VARAGG(IVAR) = 0
!
!     Cons
!
      DO ICONS = 1 , NOCONS
         IVAR = IVAR + 1
         VARNAM(IVAR) = CONAME(ICONS)
         VARARR(IVAR) = IICONS
         VARIDX(IVAR) = ICONS
         VARTDA(IVAR) = 0
         VARDAG(IVAR) = 0
         VARTAG(IVAR) = 0
         VARAGG(IVAR) = 0
      ENDDO
!
!     Param
!
      DO IPA = 1 , NOPA
         IVAR = IVAR + 1
         VARNAM(IVAR) = PANAME(IPA)
         VARARR(IVAR) = IIPARM
         VARIDX(IVAR) = IPA
         VARTDA(IVAR) = 0
         VARDAG(IVAR) = 0
         VARTAG(IVAR) = 3
         VARAGG(IVAR) = 1
         VARTDA(IVAR) = 0
         VARDAG(IVAR) = 0
         VARTAG(IVAR) = 3
         VARAGG(IVAR) = 1
      ENDDO
!
!     Func
!
      DO IFUN = 1 , NOFUN
         IVAR = IVAR + 1
         VARNAM(IVAR) = FUNAME(IFUN)
         VARARR(IVAR) = IIFUNC
         VARIDX(IVAR) = IFUN
         VARTDA(IVAR) = 0
         VARDAG(IVAR) = 0
         VARTAG(IVAR) = 0
         VARAGG(IVAR) = 0
      ENDDO
!
!     Seg Func
!
      DO ISFUN = 1 , NOSFUN
         IVAR = IVAR + 1
         VARNAM(IVAR) = SFNAME(ISFUN)
         VARARR(IVAR) = IISFUN
         VARIDX(IVAR) = ISFUN
         VARTDA(IVAR) = 0
         VARDAG(IVAR) = 0
         VARTAG(IVAR) = 3
         VARAGG(IVAR) = 1
      ENDDO
!
!     Conc
!
      DO ISYS = 1 , NOSYS
         IVAR = IVAR + 1
         VARNAM(IVAR) = SYNAME(ISYS)
         VARARR(IVAR) = IICONC
         VARIDX(IVAR) = ISYS
         VARTDA(IVAR) = 0
         VARDAG(IVAR) = 0
         VARTAG(IVAR) = 3
         VARAGG(IVAR) = 1
      ENDDO
      DO ISYS = NOSYS + 1 , NOTOT
         IVAR = IVAR + 1
         VARNAM(IVAR) = SYNAME(ISYS)
         VARARR(IVAR) = IICONC
         VARIDX(IVAR) = ISYS
         VARTDA(IVAR) = 0
         VARDAG(IVAR) = 0
         VARTAG(IVAR) = 1
         VARAGG(IVAR) = 0
      ENDDO
!
!     Mass
!
      DO ISYS = 1 , NOTOT
         IVAR = IVAR + 1
         VARNAM(IVAR) = 'MASS_'//SYNAME(ISYS)
         VARARR(IVAR) = IIMASS
         VARIDX(IVAR) = ISYS
         VARTDA(IVAR) = 0
         VARDAG(IVAR) = 0
         VARTAG(IVAR) = 1
         VARAGG(IVAR) = 0
      ENDDO
!
!     Deriv
!
      DO ISYS = 1 , NOTOT
         IVAR = IVAR + 1
         VARNAM(IVAR) = 'DERIV_'//SYNAME(ISYS)
         VARARR(IVAR) = IIDERV
         VARIDX(IVAR) = ISYS
         VARTDA(IVAR) = 2
         VARDAG(IVAR) = IVMAS + ISYS - 1
         VARTAG(IVAR) = 0
         VARAGG(IVAR) = 0
      ENDDO
!
!     Disp
!
      DO IDSP = 1 , NODISP
         IVAR = IVAR + 1
         NAME = ' '
         WRITE (NAME, '("DISP_ARRAY_",I0)') IDSP
         VARNAM(IVAR) = NAME
         VARARR(IVAR) = IIDISP
         VARIDX(IVAR) = IDSP
         VARTDA(IVAR) = 0
         VARDAG(IVAR) = 0
         VARTAG(IVAR) = 0
         VARAGG(IVAR) = 0
      ENDDO
!
!     Velo
!
      DO IVEL = 1 , NOVELO
         IVAR = IVAR + 1
         NAME = ' '
         WRITE (NAME, '("VELO_ARRAY_",I0)') IVEL
         VARNAM(IVAR) = NAME
         VARARR(IVAR) = IIVELO
         VARIDX(IVAR) = IVEL
         VARTDA(IVAR) = 0
         VARDAG(IVAR) = 0
         VARTAG(IVAR) = 0
         VARAGG(IVAR) = 0
      ENDDO
!
!     Default
!
      DO IDEF = 1 , NODEF
         IVAR = IVAR + 1
         VARNAM(IVAR) = DENAME(IDEF)
         VARARR(IVAR) = IIDEFA
         VARIDX(IVAR) = IDEF
         VARTDA(IVAR) = 0
         VARDAG(IVAR) = 0
         VARTAG(IVAR) = 0
         VARAGG(IVAR) = 0
      ENDDO
!
!     Local
!
      DO ILOC = 1 , NOLOC
         IVAR = IVAR + 1
         VARNAM(IVAR) = LOCNAM(ILOC)
         VARARR(IVAR) = IIPLOC
         VARIDX(IVAR) = ILOC
         VARTDA(IVAR) = 1
         VARDAG(IVAR) = 0
         VARTAG(IVAR) = 3
         VARAGG(IVAR) = 1
      ENDDO
!
!     DSPX
!
      DO IDSX = 1 , NDSPX
         IVAR = IVAR + 1
         VARARR(IVAR) = IIDSPX
         VARIDX(IVAR) = IDSX
         VARTDA(IVAR) = 0
         VARDAG(IVAR) = 0
         VARTAG(IVAR) = 0
         VARAGG(IVAR) = 0
      ENDDO
!
!     VELX
!
      DO IVLX = 1 , NVELX
         IVAR = IVAR + 1
         NAME = ' '
         WRITE (NAME, '("VELX_ARRAY_",I0)') IVLX
         VARNAM(IVAR) = NAME
         VARARR(IVAR) = IIVELX
         VARIDX(IVAR) = IVLX
         VARTDA(IVAR) = 0
         VARDAG(IVAR) = 0
         VARTAG(IVAR) = 0
         VARAGG(IVAR) = 0
      ENDDO
!
!     LOCX
!
      DO ILCX = 1 , NLOCX
         IVAR = IVAR + 1
         NAME = ' '
         WRITE (NAME, '("VLOCX_",I0)') ILCX
         VARNAM(IVAR) = NAME
         VARARR(IVAR) = IILOCX
         VARIDX(IVAR) = ILCX
         VARTDA(IVAR) = 0
         VARDAG(IVAR) = 0
         VARTAG(IVAR) = 0
         VARAGG(IVAR) = 0
      ENDDO
!
!     FLUX
!
      DO IFLX = 1 , NFLUX
         IVAR = IVAR + 1
         VARARR(IVAR) = IIFLUX
         VARIDX(IVAR) = IFLX
         VARTDA(IVAR) = 2
         VARDAG(IVAR) = IVVOL
         VARTAG(IVAR) = 1
         VARAGG(IVAR) = 0
      ENDDO
!
!     Read list of overrulings
!
      NOVAT = 0
      INQUIRE ( FILE='aggrlist.dat' , EXIST = LEXI )
      IF ( LEXI ) THEN
         OPEN(NEWUNIT=LUN, FILE='aggrlist.dat')
         DO
            NOVAT = NOVAT + 1
            IF ( NOVAT .GT. MAXLOC ) THEN
               LINE = 'ERROR : local dimension overflow in SETVAT'
               CALL MONSYS(LINE,1)
               WRITE(*,*) LINE
               CALL SRSTOP(1)
            ENDIF
            READ(LUN,*,IOSTAT=IERR) VATNAM(NOVAT),VATTAG(NOVAT),VATNAG(NOVAT),
     +                              VATTDA(NOVAT),VATNDA(NOVAT)
            IF ( IERR /=0 ) THEN
               EXIT
            ENDIF
         ENDDO
         NOVAT = NOVAT - 1
         CLOSE (LUN)
      ENDIF
!
!     Check if there are overrulings
!
      DO IVAR = 1 , NOVAR
         CALL ZOEK ( VARNAM(IVAR), NOVAT , VATNAM, 20    , IVAT  )
         IF ( IVAT   .GT. 0 ) THEN
!
!           aggregation
!
            IF ( VATTAG(IVAT) .EQ. 0 ) THEN
!
!              NO aggregation
!
               VARTAG(IVAR) = VATTAG(IVAT)
               VARAGG(IVAR) = 0
!
            ELSEIF ( VATTAG(IVAT) .EQ. 1 ) THEN
!
!              Accumulate
!
               VARTAG(IVAR) = VATTAG(IVAT)
               VARAGG(IVAR) = 0
!
            ELSEIF ( VATTAG(IVAT) .EQ. 2 ) THEN
!
!              Average
!
               VARTAG(IVAR) = VATTAG(IVAT)
               VARAGG(IVAR) = 0
!
            ELSEIF ( VATTAG(IVAT) .EQ. 3 ) THEN
!
!              Weight average
!
               CALL ZOEK ( VATNAG(IVAT), NOVAR , VARNAM, 20    , IV_AG )
               IF ( IV_AG .GT. 0 ) THEN
                  VARTAG(IVAR) = VATTAG(IVAT)
                  VARAGG(IVAR) = IV_AG
               ELSE
                  VARTAG(IVAR) = 2
                  VARAGG(IVAR) = 0
               ENDIF
!
            ELSE
!
!              Error undefined type of aggregtation
!
               IERR = IERR + 1
               LINE = 'ERROR : undefined type off aggregation for :'//
     +                VARNAM(IVAR)
               CALL MONSYS( LINE  , 1 )
               WRITE(LINE,'(''type:'',I5,'' from aggrlist.dat'')')
     +                VATTAG(IVAT)
               CALL MONSYS( LINE  , 1 )
!
            ENDIF
!
!           dis-aggregation
!
            IF ( VATTDA(IVAT) .EQ. 0 ) THEN
!
!              NO dis-aggregation
!
               VARTDA(IVAR) = VATTDA(IVAT)
               VARDAG(IVAR) = 0
!
            ELSEIF ( VATTDA(IVAT) .EQ. 1 ) THEN
!
!              expansion
!
               VARTDA(IVAR) = VATTDA(IVAT)
               VARDAG(IVAR) = 0
!
            ELSEIF ( VATTDA(IVAT) .EQ. 2 ) THEN
!
!              distribute with weight
!
               CALL ZOEK ( VATNDA(IVAT), NOVAR , VARNAM, 20    , IV_DA )
               IF ( IV_DA .GT. 0 ) THEN
                  VARTDA(IVAR) = VATTDA(IVAT)
                  VARDAG(IVAR) = IV_DA
               ELSE
                  VARTDA(IVAR) = 3
                  VARDAG(IVAR) = 0
               ENDIF
!
            ELSEIF ( VATTDA(IVAT) .EQ. 3 ) THEN
!
!              distribute
!
               VARTDA(IVAR) = VATTDA(IVAT)
               VARDAG(IVAR) = 0
!
            ELSE
!
!              Error undefined type of aggregtation
!
               IERR = IERR + 1
               LINE = 'ERROR : undefined type off dis-aggregation for :'
     +                //VARNAM(IVAR)
               CALL MONSYS( LINE  , 1 )
               WRITE(LINE,'(''type:'',I5,'' from aggrlist.dat'')')
     +                VATTAG(IVAT)
               CALL MONSYS( LINE  , 1 )
!
            ENDIF
         ENDIF
      ENDDO
!
      if (timon) call timstop( ithndl )
      RETURN
      END
