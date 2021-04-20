   subroutine reset_sedtra()
   use m_sediment
   use morphology_data_module
   use m_rdstm
   use message_module

   implicit none

   integer            ::   istat

   if (.not. stm_included) return
   istat = clrstm(stmpar)
   call clrsedtra(istat,sedtra)
   if ( associated(mtd%dzbdt)) then
      deallocate(mtd%dzbdt)
      deallocate(mtd%uau)
      deallocate(mtd%rhowat)
      deallocate(mtd%seddif)
      deallocate(mtd%sed)
      deallocate(mtd%ws)
      deallocate(mtd%blchg)

      call clearstack (mtd%messages)
      deallocate(mtd%messages)
   end if
   end subroutine reset_sedtra
