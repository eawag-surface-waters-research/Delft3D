!!  Copyright (C)  Stichting Deltares, 2021-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.

      subroutine merge_data(data_1, data_2)

      ! function : merge data_2 into data_1 not fully implemeted but ok for the discharges in ddcouple

      ! global declarations

      use m_srstop
      use m_monsys
      use dlwqdata_mod             ! module contains everything for the data
      implicit none

      ! declaration of the arguments

      type(t_dlwqdata)      , intent(inout)  :: data_1                 ! first block of data
      type(t_dlwqdata)      , intent(inout)  :: data_2                 ! second block of data

      ! local declarations

      integer                                :: i                      ! loop counter
      integer                                :: ibrkt                  ! index breakpoint in tmp data
      integer                                :: ibrk1                  ! index breakpoint in data 1
      integer                                :: ibrk2                  ! index breakpoint in data 2
      integer                                :: nobrkt                 ! number of breakpoints in merged data
      integer                                :: iloc                   ! index location
      integer                                :: lunrep                 ! unit number report file
      type(t_dlwqdata)                       :: data_tmp               ! temporary block of data with merged data
      integer                                :: ierr_alloc             ! error indication

      call getmlu(lunrep)

      ! nothing to add then return

      if ( data_2%no_loc .eq. 0 ) then
         return
      endif

      ! if nothing in original then just copy

      if ( data_1%no_loc .eq. 0 ) then

         data_1%no_loc   = data_2%no_loc
         data_1%no_param = data_2%no_param
         data_1%no_brk   = data_2%no_brk
         data_1%functype = data_2%functype
         allocate(data_1%times(data_1%no_brk), &
                  data_1%values(data_1%no_param,data_1%no_loc,data_1%no_brk), &
                  stat=ierr_alloc)
         if ( ierr_alloc .ne. 0 ) then
            write(lunrep,*) ' error allocating data arrays'
            write(lunrep,*) ' number of parameters :',data_1%no_param
            write(lunrep,*) ' number of brakpoints :',data_1%no_brk
            write(lunrep,*) ' number of locations  :',data_1%no_loc
            call srstop(1)
         endif
         data_1%times  = data_2%times
         data_1%values = data_2%values

      else

         ! for the moment assume parameters equal and 1

         if ( data_1%no_param .gt. 1 .or. data_2%no_param .gt. 1 ) then
            write(*,*) 'merge for paramters not yet implemented'
         endif

         data_tmp%no_loc   = data_1%no_loc + data_2%no_loc
         data_tmp%no_param = data_1%no_param

         ! count number of breakpoints,  loop till we have passed all breakpoints

         ibrk1  = 0
         ibrk2  = 0
         nobrkt = 0
         do
            if ( ibrk1 .eq. data_1%no_brk ) then
               nobrkt = nobrkt + data_2%no_brk - ibrk2
               exit
            endif
            if ( ibrk2 .eq. data_2%no_brk ) then
               nobrkt = nobrkt + data_1%no_brk - ibrk1
               exit
            endif
            nobrkt = nobrkt + 1
            if ( data_1%times(ibrk1+1) .le. data_2%times(ibrk2+1) ) then
               ibrk1 = ibrk1 + 1
               if ( data_1%times(ibrk1) .eq. data_2%times(ibrk2+1) ) then
                  ibrk2 = ibrk2 + 1
               endif
            else
               ibrk2 = ibrk2 + 1
            endif
         enddo
         data_tmp%no_brk   = nobrkt
         allocate(data_tmp%times(data_tmp%no_brk), &
                  data_tmp%values(data_tmp%no_param,data_tmp%no_loc,data_tmp%no_brk), &
                  stat=ierr_alloc)
         if ( ierr_alloc .ne. 0 ) then
            write(lunrep,*) ' error allocating data arrays'
            write(lunrep,*) ' number of parameters :',data_tmp%no_param
            write(lunrep,*) ' number of brakpoints :',data_tmp%no_brk
            write(lunrep,*) ' number of locations  :',data_tmp%no_loc
            call srstop(1)
         endif

         ! set new timeseries

         ibrk1  = 0
         ibrk2  = 0
         ibrkt  = 0
         do
            if ( ibrk1 .eq. data_1%no_brk ) then
               if ( ibrk2 .lt. data_2%no_brk ) then
                  do i = ibrk2 + 1 , data_2%no_brk
                     ibrkt = ibrkt + 1
                     data_tmp%times(ibrkt) = data_2%times(i)
                     do iloc = 1 , data_1%no_loc
                        data_tmp%values(1,iloc,ibrkt) = data_1%values(1,iloc,ibrk1)
                     enddo
                     do iloc = 1 , data_2%no_loc
                        data_tmp%values(1,data_1%no_loc+iloc,ibrkt) = data_2%values(1,iloc,i)
                     enddo
                  enddo
               endif
               exit
            endif
            if ( ibrk2 .eq. data_2%no_brk ) then
               if ( ibrk1 .lt. data_1%no_brk ) then
                  do i = ibrk1 + 1 , data_1%no_brk
                     ibrkt = ibrkt + 1
                     data_tmp%times(ibrkt) = data_1%times(i)
                     do iloc = 1 , data_1%no_loc
                        data_tmp%values(1,iloc,ibrkt) = data_1%values(1,iloc,i)
                     enddo
                     do iloc = 1 , data_2%no_loc
                        data_tmp%values(1,data_1%no_loc+iloc,ibrkt) = data_2%values(1,iloc,ibrk2)
                     enddo
                  enddo
               endif
               exit
            endif
            ibrkt = ibrkt + 1
            if ( data_1%times(ibrk1+1) .le. data_2%times(ibrk2+1) ) then
               ibrk1 = ibrk1 + 1
               if ( data_1%times(ibrk1) .eq. data_2%times(ibrk2+1) ) then
                  ibrk2 = ibrk2 + 1
               endif
               data_tmp%times(ibrkt) = data_1%times(ibrk1)
            else
               ibrk2 = ibrk2 + 1
               data_tmp%times(ibrkt) = data_2%times(ibrk2)
            endif
            do iloc = 1 , data_1%no_loc
               data_tmp%values(1,iloc,ibrkt) = data_1%values(1,iloc,max(1,ibrk1))
            enddo
            do iloc = 1 , data_2%no_loc
               data_tmp%values(1,data_1%no_loc+iloc,ibrkt) = data_2%values(1,iloc,max(1,ibrk2))
            enddo
         enddo

         ! move the temporary stuff to data_1
         data_1%no_loc   = data_tmp%no_loc
         data_1%no_param = data_tmp%no_param
         data_1%no_brk   = data_tmp%no_brk
         deallocate(data_1%times,data_1%values)
         data_1%times    => data_tmp%times
         data_1%values   => data_tmp%values

      endif

      return
      end
