      subroutine renum_bnd(openbndsect_coll, ibnd, ibnd_new)

      ! function : renumber boundary in the sections

      ! (c) Deltares

      ! global declarations

      use hydmod                   ! module contains everything for the hydrodynamic description
      implicit none

      ! declaration of the arguments

      type(t_openbndsect_coll)               :: openbndsect_coll       ! collection of openbndsects
      integer                                :: ibnd                   ! boundary number (negative)
      integer                                :: ibnd_new               ! new boundary number (negative)

      ! local declarations

      integer                                :: no_sect                ! number of sections
      integer                                :: i_sect                 ! index of section
      integer                                :: no_bnd                 ! number of boundaries in section
      integer                                :: i_bnd                  ! index of boundary
      type(t_openbndsect), pointer           :: openbndsect            ! single section

      ! look for boundary number in the sections set new number

      no_sect = openbndsect_coll%cursize

      do i_sect = 1 , no_sect

         openbndsect => openbndsect_coll%openbndsect_pnts(i_sect)
         no_bnd = openbndsect%openbndlin_coll%cursize

         do i_bnd = 1 , no_bnd
            if ( openbndsect%openbndlin_coll%openbndlin_pnts(i_bnd)%ibnd .eq. ibnd ) then
               openbndsect%openbndlin_coll%openbndlin_pnts(i_bnd)%ibnd_new = ibnd_new
               goto 200
            endif
         enddo

      enddo
  200 continue

      return
      end
