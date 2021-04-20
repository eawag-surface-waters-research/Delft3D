      SUBROUTINE WRISAM( MSAM )
      USE M_SAMPLES
      USE M_ARCINFO
      USE M_MISSING, only: DMISS
      implicit none
      integer :: msam, KMOD

      double precision :: af
      integer :: i
      integer :: jflow
      COMMON /PHAROSFLOW/  JFLOW
      COMMON /PHAROSLINE/  REC1
      CHARACTER REC1*132

      CALL READYY('Writing Samples File',0d0)

      if ( MCA*NCA.eq.NS ) then
         call wriarcsam(MSAM,ZS,MCA,NCA,MCA,NCA,X0,Y0,DXA,DYA,DMISS)
         goto 1234
      end if


      KMOD = MAX(1,NS/100)
      DO 10 I = 1,NS
         IF (MOD(I,KMOD) == 0) THEN
            AF = dble(I) / dble(NS)
            CALL READYY('Writing Samples File',AF)
         ENDIF
         ! if (xs(i) > 179.87d0) xs(i) = xs(i) - 360d0
         if ( abs(zs(i)).lt.1d6 ) then
            WRITE (MSAM,'(3(F16.7))') XS(I), YS(I), ZS(I)
         else if ( abs(zs(i)).lt.1d16 ) then
            WRITE (MSAM,"(2F16.7, ' ', F26.7)") XS(I), YS(I), ZS(I)
         else
            call qnerror('wrisam: format error', ' ', ' ')
         end if
   10 CONTINUE

 1234 continue
      CALL DOCLOSE(MSAM)
      CALL READYY('Writing Samples File',-1d0)


      RETURN
      END SUBROUTINE WRISAM
