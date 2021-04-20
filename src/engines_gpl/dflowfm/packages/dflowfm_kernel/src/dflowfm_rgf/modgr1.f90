      !> This routine operates directly on active grid data from m_grid
      SUBROUTINE MODGR1(NPUT, MP, NP, IN, JN)!, NCOL)!XH, YH, mmax, nmax, MC, NC,
      use m_missing
      use m_grid
      use unstruc_colors
      implicit none

      integer :: nput, mp, np, in, jn
!      double precision :: XH(MMAX,NMAX), YH(MMAX,NMAX)
!     een beetje flauw geprogrammeerd, ook tekenen bij insert mode

      integer :: ja

      IF (NPUT .EQ. -1) THEN
         JA = 0
         IF (MP .GE. MMAX-1) THEN
           call increasegrid(mp+2,nmax)
!           CALL OKAY(0)
!           CALL QNERROR('Grid Becomes too Large in M-Dimension',' ',' ')
           RETURN
         ELSE
            IF (MP .EQ. 1 .AND. IN .EQ. -1) THEN
               CALL SHIFXY(1,      0,     MP,     NP)!     XH,     YH,     mmax, nmax, MC,     NC,
            ENDIF
         ENDIF
         IF (NP .GE. NMAX-1) THEN
           call increasegrid(mmax, np+2)
!           CALL OKAY(0)
!           CALL QNERROR('Grid Becomes too Large in N-Dimension',' ',' ')
           RETURN
         ELSE
            IF (NP .EQ. 1 .AND. JN .EQ. -1) THEN
               CALL SHIFXY(0,      1,     MP,     NP)!     XH,     YH,     mmax, nmax, MC,     NC,
            ENDIF
         ENDIF

         IF (IN .EQ. 1) THEN
            IF (MP .EQ. MC-1) MC = MC + 1
            IF (Xc(MP+2,NP) .EQ. XYMIS) THEN
                Xc(MP+2,NP) = 2*Xc(MP+1,NP)  - Xc(MP,NP)
                Yc(MP+2,NP) = 2*Yc(MP+1,NP)  - Yc(MP,NP)
                CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC,  MP+2,     NP,   NCOLDG      )
                JA = 1
            ENDIF
            IF (Xc(MP+2,NP+1) .EQ. XYMIS) THEN
                Xc(MP+2,NP+1) = 2*Xc(MP+1,NP+1)  - Xc(MP,NP+1)
                Yc(MP+2,NP+1) = 2*Yc(MP+1,NP+1)  - Yc(MP,NP+1)
                CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC,  MP+2,   NP+1,   NCOLDG      )
                JA = 1
            ENDIF
         ELSE IF (IN .EQ. -1) THEN
            IF (Xc(MP-1,NP) .EQ. XYMIS) THEN
                Xc(MP-1,NP) = 2*Xc(MP,NP)  - Xc(MP+1,NP)
                Yc(MP-1,NP) = 2*Yc(MP,NP)  - Yc(MP+1,NP)
                CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC, MP-1,     NP,   NCOLDG      )
                JA = 1
            ENDIF
            IF (Xc(MP-1,NP+1) .EQ. XYMIS) THEN
                Xc(MP-1,NP+1) = 2*Xc(MP,NP+1)  - Xc(MP+1,NP+1)
                Yc(MP-1,NP+1) = 2*Yc(MP,NP+1)  - Yc(MP+1,NP+1)
                CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC, MP-1,   NP+1,   NCOLDG      )
                JA = 1
            ENDIF
         ELSE IF (JN .EQ. 1) THEN
            IF (NP .EQ. NC-1) NC = NC + 1
            IF (Xc(MP,NP+2) .EQ. XYMIS) THEN
                Xc(MP,NP+2) = 2*Xc(MP,NP+1)  - Xc(MP,NP)
                Yc(MP,NP+2) = 2*Yc(MP,NP+1)  - Yc(MP,NP)
                CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC, MP,   NP+2,   NCOLDG      )
                JA = 1
            ENDIF
            IF (Xc(MP+1,NP+2) .EQ. XYMIS) THEN
                Xc(MP+1,NP+2) = 2*Xc(MP+1,NP+1)  - Xc(MP+1,NP)
                Yc(MP+1,NP+2) = 2*Yc(MP+1,NP+1)  - Yc(MP+1,NP)
                CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC, MP+1,   NP+2,   NCOLDG      )
                JA = 1
            ENDIF
         ELSE IF (JN .EQ. -1) THEN
            IF (Xc(MP,NP-1) .EQ. XYMIS) THEN
                Xc(MP,NP-1) = 2*Xc(MP,NP)  - Xc(MP,NP+1)
                Yc(MP,NP-1) = 2*Yc(MP,NP)  - Yc(MP,NP+1)
                CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC, MP,   NP-1,   NCOLDG      )
                JA = 1
            ENDIF
            IF (Xc(MP+1,NP-1) .EQ. XYMIS) THEN
                Xc(MP+1,NP-1) = 2*Xc(MP+1,NP)  - Xc(MP+1,NP+1)
                Yc(MP+1,NP-1) = 2*Yc(MP+1,NP)  - Yc(MP+1,NP+1)
                CALL TEKGRPT(     Xc,     Yc,     mmax, nmax, MC,     NC, MP+1,   NP-1,   NCOLDG      )
                JA = 1
            ENDIF
         ENDIF
         IF (JA .EQ. 1) THEN
            CALL OKAY(0)
         ELSE
            CALL OKAY(0)
         ENDIF
      ELSE IF (NPUT .EQ. -2) THEN
         Xc(MP,NP) = XYMIS
         Yc(MP,NP) = XYMIS
         IF (MP .EQ. 1 .OR. MP .EQ. MC .OR. NP .EQ. 1 .OR. NP .EQ. NC    ) THEN
            CALL ADJUST(Xc, Yc, mmax, nmax, MC, NC)
         ENDIF
      ENDIF
      RETURN
      END subroutine modgr1
