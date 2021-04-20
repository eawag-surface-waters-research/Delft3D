! =================================================================================================
! =================================================================================================
subroutine equili_spiralintensity()
   use m_flow
   use m_flowgeom
   use m_sferic, only: jsferic, fcorio
   implicit none
   integer          :: kk
   double precision :: spir_ce, spir_be, fcoriocof

   do kk = 1,ndx
      fcoriocof = fcorio
      if( icorio > 0 .and. jsferic == 1 ) fcoriocof = fcoris(kk)
      spir_ce = fcorio * hs(kk) * 0.5d0
      spir_be = hs(kk) * spircrv(kk) * spirucm(kk)
      spirint(kk) = spir_be - spir_ce
   enddo

end subroutine equili_spiralintensity
