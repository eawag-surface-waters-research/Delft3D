      double precision FUNCTION DISLIN(X,Y,N,XX,YY,TV)

!     AFSTAND VAN PUNT XX,YY TOT LIJN MET PARM TV
      use geometry_module, only: dbdistance
      use m_missing, only: dmiss
      use m_sferic, only: jsferic, jasfer3D

      implicit none
      integer :: n
      double precision :: tv
      double precision :: xv
      double precision :: xx
      double precision :: yv
      double precision :: yy

      double precision :: X(N), Y(N)
      TV   = MAX(0d0,MIN(TV,N-1d0))
      CALL LINT(X,Y,N,TV,XV,YV)
      dislin = dbdistance(XV,YV,XX,YY,jsferic, jasfer3D, dmiss)
      RETURN
      END function dislin
