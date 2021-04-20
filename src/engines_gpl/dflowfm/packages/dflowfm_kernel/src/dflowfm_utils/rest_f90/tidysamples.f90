      SUBROUTINE TIDYSAMPLES(XS,YS,ZS,IPSAM,NS,MXSAM,MYSAM)
      use sorting_algorithms, only: indexx
      implicit none
      integer :: ns
      double precision :: XS(NS), YS(NS), ZS(NS)   !< sample coordinates
      integer, dimension(NS), intent(out) :: IPSAM !< permutation array (increasing x-coordinate)
      integer,                intent(in)  :: MXSAM, MYSAM   !< structured sample data dimensions (>0) or unstructured (0)
!      IF (NS .GT. 1) CALL RSORT3(XS,YS,ZS,NS)

      if ( NS.gt.1 ) then
         call indexx(Ns,xs,IPSAM)
      end if

!     remove double/missing samples (non-structured sample data only)
      if ( MXSAM*MYSAM.ne.NS ) then
         CALL READYY(' ',0.3d0)
         IF (NS .GT. 1) CALL RMDOUBLE(XS,YS,ZS,IPSAM,NS)
      end if

      CALL READYY(' ',1d0)

      RETURN
      END
