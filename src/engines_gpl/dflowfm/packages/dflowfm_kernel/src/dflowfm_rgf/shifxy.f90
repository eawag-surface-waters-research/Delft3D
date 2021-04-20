      !> Operates on active grid from m_grid directly!
      SUBROUTINE SHIFXY(IS,     JS,     MP,     NP        )

      !     XH,     YH,     mmax, nmax, MC,     NC, IS,     JS,     MP,     NP        )
      use m_missing
      use m_grid
      use geometry_module, only: pinpok

      implicit none
      integer :: is, js, mp, np

      integer :: i, j
!     schuif data naar rechts of boven of beide en geef nieuwe MC,NC

      MC = MC + IS
      NC = NC + JS

      call increasegrid(mc,nc)

      MP = MP + IS
      NP = NP + JS

      DO 10 J = NC,1+JS,-1
         DO 10 I = MC,1+IS,-1
            Xc(I,J) = Xc(I-IS,J-JS)
            Yc(I,J) = Yc(I-IS,J-JS)
            Zc(I,J) = Zc(I-IS,J-JS)
    10 CONTINUE
      IF (IS .EQ. 1) THEN
         DO 20 J = 1,NC
            Xc(1,J) = XYMIS
            Yc(1,J) = XYMIS
            Zc(1,J) = XYMIS
    20   CONTINUE
      ENDIF
      IF (JS .EQ. 1) THEN
         DO 30 I = 1,MC
            Xc(I,1) = XYMIS
            Yc(I,1) = XYMIS
            Zc(I,1) = XYMIS
    30   CONTINUE
      ENDIF
      RETURN
      END subroutine shifxy
