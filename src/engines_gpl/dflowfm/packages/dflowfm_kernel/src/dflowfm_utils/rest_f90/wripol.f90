      SUBROUTINE WRIPOL(MPOL)
      USE M_POLYGON
      use m_missing
      implicit none
      integer :: mpol, numnampli
      integer :: NCLAN(0)
      double precision :: ZSH(0)

      if (NPL<=0) return
      numnampli = size(nampli)
      if (zpl(1) == dmiss) then ! No third column for z-values
        CALL WRILDB(MPOL, XPL, YPL, NPL, NCLAN, 0, ZSH, 0, nampli, 64, numnampli)
      else
        CALL WRILDB(MPOL, XPL, YPL, NPL, NCLAN, 0, ZPL, NPL, nampli, 64, numnampli)
      end if
      END subroutine wripol
