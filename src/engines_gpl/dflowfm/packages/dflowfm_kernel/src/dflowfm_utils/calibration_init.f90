! Initialise calibration factors
subroutine calibration_init()
 use m_calibration
 use unstruc_messages
 use unstruc_model, only: md_cldfile, md_cllfile
 use m_flowgeom,    only: lnx, lnx1d
 use m_flow,        only: ifrcutp, ifrctypuni, frcu, frcu_bkp
 !
 implicit none
 !
 integer         :: LF
 logical         :: error

 ! Skip 1d friction types. For now the ifrcutp is always equal to 1, while FRCU contains the actual (calculated) Chezy value
 do LF = lnx1D+1, lnx
     if (ifrcutp(LF) /= ifrctypuni) then
         error = .true.
         call mess(LEVEL_ERROR, 'Only uniform background roughness definition supported in combination with [calibration]DefinitionFile')
     end if
 end do

 call read_cldfile(md_cldfile, clddata, GET_DIMENSIONS)
 call read_cldfile(md_cldfile, clddata, FILL_DATA)
 !
 call read_cllfile(md_cllfile, clddata, GET_DIMENSIONS)
 call read_cllfile(md_cllfile, clddata, FILL_DATA)
 !
 ! TO DO: check for obs en crs in domain (similar to trachy code below)

                    ! ! TO DO: Update to allow cross-sections for parts outside domain which are inactive
                    !!itrt = trachy_fl%gen%ittdef(trachy_fl%gen%crs(itrtcrs)%itrt,1)
                    !
                    !!cld_in_arl =  .false.
                    !!itt = 0
                    !!do while ((.not. trt_in_arl) .and. (itt < trachy_fl%dir(1)%nttaru))
                    !!   itt = itt + 1
                    !!   if (trachy_fl%dir(1)%ittaru(itt,3) == itrt) then    ! if trachytope is included in .arl file
                    !!      trt_in_arl = .true.
                    !!   end if
                    !!enddo
                    !!
                    !! TO DO: move to after reading cll ?
                    !if ((clddata%crs(icldcrs) == intmiss) .and. cld_in_arl) then
                    !    call mess(LEVEL_ERROR, 'Error reading calibration definition file: Cross-section does not exist in "'//trim(md_cldfile)//'": '//rec132)
                    !    error = .true.
                    !    goto 9999
                    !end if
                    !
 !
end subroutine calibration_init
