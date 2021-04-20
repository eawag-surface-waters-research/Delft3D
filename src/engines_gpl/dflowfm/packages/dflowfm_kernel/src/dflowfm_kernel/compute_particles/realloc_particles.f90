!> (re)allocate
subroutine realloc_particles(Nsize, LkeepExisting, ierror)
   use m_particles
   use m_alloc
   use m_missing
   use m_sferic, only: jsferic
   implicit none

   integer, intent(in)  :: Nsize          !< array sizes
   logical, intent(in)  :: LkeepExisting  !< keep existing data (1) or not (0)
   integer, intent(out) :: ierror         !< error (1) or not

   ierror = 1

!  reallocate
   call realloc(xpart, Nsize, keepExisting=LkeepExisting, fill=DMISS)
   call realloc(ypart, Nsize, keepExisting=LkeepExisting, fill=DMISS)
   if ( jsferic.eq.1 ) then
      call realloc(zpart, Nsize, keepExisting=LkeepExisting, fill=DMISS)
   end if
   call realloc(dtremaining, Nsize, keepExisting=LkeepExisting, fill=0d0)
   call realloc(kpart, Nsize, keepExisting=LkeepExisting, fill=0)
   call realloc(iglob, Nsize, keepExisting=LkeepExisting, fill=0)

   call realloc(numzero, Nsize, keepExisting=LkeepExisting, fill=0)
   numzero = 0

   ierror = 0
1234 continue

   return
end subroutine realloc_particles
