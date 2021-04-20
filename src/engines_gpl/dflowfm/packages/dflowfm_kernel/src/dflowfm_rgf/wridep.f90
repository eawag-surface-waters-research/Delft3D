      SUBROUTINE WRIDEP(MMDD,ZC,M1,N1,MC,NC,mmax,nmax)
      implicit none
      integer          :: MMDD,M1,N1,MC,NC,mmax,nmax,n,m
      double precision :: ZC(mmax,nmax)
      double precision :: AF

      CALL READYY('Writing Depth File ',0d0)
      DO 10 N = N1, NC
         AF = dble(N) / dble(NC)
         CALL READYY('Writing Dept File',AF)
         WRITE(MMDD,'(12(1PE13.5))') (ZC(M,N),M = M1,MC)
   10 CONTINUE
      CALL READYY('writing Dept File',-1d0)
      CALL DOCLOSE (MMDD)
      RETURN
      END
