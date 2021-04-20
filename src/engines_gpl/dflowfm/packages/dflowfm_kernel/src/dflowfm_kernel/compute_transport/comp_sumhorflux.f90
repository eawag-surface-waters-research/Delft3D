!> sum horizontal fluxes
subroutine comp_sumhorflux(NUMCONST, kmx, Lnkx, Ndkx, Lbot, Ltop, fluxhor, sumhorflux)
   use m_flowgeom, only: Lnx, Ln, nd, Ndx    ! static mesh information
   use timers

   implicit none

   integer,                                    intent(in)    :: NUMCONST      !< number of transported quantities
   integer,                                    intent(in)    :: kmx           !< number of layers
   integer,                                    intent(in)    :: Ndkx          !< total number of flownodes (dynamically changing)
   integer,                                    intent(in)    :: Lnkx          !< total number of flowlinks (dynamically changing)
   integer,          dimension(Lnx),           intent(in)    :: Lbot          !< flow-link based layer administration
   integer,          dimension(Lnx),           intent(in)    :: Ltop          !< flow-link based layer administration
   double precision, dimension(NUMCONST,Lnkx), intent(in)    :: fluxhor       !< horizontal advection fluxes
   double precision, dimension(NUMCONST,Ndkx), intent(inout) :: sumhorflux    ! sum of horizontal fluxes, dim(NUMCONST,Ndkx)

   integer :: LL, L, Lb, Lt
   integer :: j, k1, k2
   integer :: k

   integer(4) ithndl /0/
   if (timon) call timstrt ( "comp_sumhorflux", ithndl )

   if ( kmx.lt.1 ) then
!     add horizontal fluxes to right-hand side
      do L=1,Lnx
!        get neighboring flownodes
         k1 = ln(1,L)
         k2 = ln(2,L)
         do j=1,NUMCONST
            sumhorflux(j,k1) = sumhorflux(j,k1) - fluxhor(j,L)
            sumhorflux(j,k2) = sumhorflux(j,k2) + fluxhor(j,L)
         end do
      end do
   else
!     add horizontal fluxes to right-hand side
      do LL=1,Lnx
         Lb = Lbot(LL)
         Lt = Ltop(LL)
         do L=Lb,Lt
!           get neighboring flownodes
            k1 = ln(1,L)
            k2 = ln(2,L)
            do j=1,NUMCONST
               sumhorflux(j,k1) = sumhorflux(j,k1) - fluxhor(j,L)
               sumhorflux(j,k2) = sumhorflux(j,k2) + fluxhor(j,L)
            end do
         end do
      end do
   end if

   if (timon) call timstop( ithndl )
   return
end subroutine comp_sumhorflux
