!> check if structures on flowlinks are unique
subroutine check_structures_and_fixed_weirs()
   use m_flowgeom, only: Lnx
   use m_flowexternalforcings, only: ncgensg, kcgen, L1cgensg, L2cgensg
   use m_fixedweirs, only: nfxw, lnfxw
   use unstruc_messages
   implicit none

   character(len=128)                 :: msg

   integer, dimension(:), allocatable :: L2struct
   integer, dimension(:), allocatable :: L2weir

   integer                            :: Lf, n, k
   integer                            :: nummulti
   integer                            :: numweir

!  allocate flowlink -> structure array
   allocate(L2struct(Lnx))
   L2struct = 0
!  allocate flowlink -> weir array
   allocate(L2weir(Lnx))
   L2weir = 0

!  fill flowlink -> fixed weir array
   do n=1,nfxw
      Lf = lnfxw(n)
      L2weir(Lf) = n
   end do

   nummulti = 0
   numweir = 0
!  loop over structures
   do n = ncgensg, 1, -1
!     loop over flowlinks of structure
      do k = L1cgensg(n), L2cgensg(n)
!        get flowlink
         Lf = kcgen(3,k)

!        check if this flowlink is free
         if ( L2struct(Lf).eq.0 ) then
!           flowlink is free
            L2struct(Lf) = n
         else
!           flowlink is not free
            nummulti = nummulti+1
            write(msg, "('Flowlink ', I0, ' found in structure ', I0, ' already claimed by structure ', I0, '.')") Lf, n, L2struct(Lf)
            call mess(LEVEL_WARN, trim(msg))
         end if

!        check if this flowlink is not associated with a fixed weir
         if ( L2weir(Lf).ne.0 ) then
!           flowlink is associated with fixed weir
            numweir = numweir+1
            write(msg, "('Flowlink ', I0, ' found in structure ', I0, ' already claimed by fixed weir.')") Lf, n
            call mess(LEVEL_WARN, trim(msg))
         end if
      end do
   end do

   if ( nummulti.gt.0 ) then
      call mess(LEVEL_ERROR, 'multiple general structures defined on one or more flowlink(s), see preceding message(s).')
   end if

!  deallocate
   if ( allocated(L2struct) ) deallocate(L2struct)
   if ( allocated(L2weir) )   deallocate(L2weir)

   return
end subroutine check_structures_and_fixed_weirs
