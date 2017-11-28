module ec_module_api

   use iso_c_binding
   
implicit none
   contains
   
function triang(cptr_sx, cptr_sy, cptr_sv, NS, cptr_dx, cptr_dy, numD, cptr_res, jsferic) result(ierr) bind(C, name="triang")
    !DEC$ ATTRIBUTES DLLEXPORT :: triang

    !from ec_module
    use m_ec_basic_interpolation, only: triinterp2

    implicit none

    ! parameters
    type(c_ptr), intent(in)                 :: cptr_sx      ! samples x, y, values
    type(c_ptr), intent(in)                 :: cptr_sy
    type(c_ptr), intent(in)                 :: cptr_sv
    type(c_ptr), intent(in)                 :: cptr_dx      ! destinations x, y
    type(c_ptr), intent(in)                 :: cptr_dy
    type(c_ptr), intent(inout)              :: cptr_res     ! return values (ptr to double array)
    integer(kind=c_int), intent(in)         :: NS, numD, jsferic

    ! local variables

    integer                                 :: jdla = 1  
    real(c_double), pointer                 :: ptr(:)
    real(c_double), pointer                 :: dx(:)
    real(c_double), pointer                 :: dy(:)
    real(c_double), pointer                 :: dRes(:)
      
    !From other modules
    integer                                  :: ierr 
    double precision, allocatable            :: XS(:), YS(:), ZS(:)
    double precision, allocatable            :: XPL(:), YPL(:), ZPL(:)
    double precision                         :: dmiss=-999d0
    
    ierr = 0

    ! (re)allocate sample arrays
    if (allocated(XS)) then
        deallocate(XS,YS,ZS)
    end if
    allocate(XS(NS), YS(NS), ZS(NS))

    ! copy ptr's to fortran arrays
    call c_f_pointer(cptr_sx, ptr, (/NS/))
    XS(:) = ptr

    call c_f_pointer(cptr_sy, ptr, (/NS/))
    YS(:) = ptr

    call c_f_pointer(cptr_sv, ptr, (/NS/))
    ZS(:) = ptr

    call c_f_pointer(cptr_dx, dx, (/numD/))
    call c_f_pointer(cptr_dy, dy, (/numD/))
    call c_f_pointer(cptr_res, dRes, (/numD/))
      
    ! call triangulate (dres is the result)
    ! call triinterp2(dx, dy, dRes, numD, jdla, XS, YS, ZS, NS, dmiss, jsferic, jins = 1, NPL = 0, MXSAM = 0, MYSAM= 0)    
    

end function triang


subroutine averaging(cptr_sx, cptr_sy, cptr_sv, c_nums, cptr_cx, cptr_cy, cptr_cxx, cptr_cyy, cptr_cnp, c_numc, c_n6, cptr_res, cptr_meth, cptr_nmin, cptr_csize, jsferic, jasfer3D) bind(C, name="averaging")
    !DEC$ ATTRIBUTES DLLEXPORT :: averaging
    use kdtree2Factory
    use m_ec_interpolationsettings
    use m_ec_basic_interpolation, only: averaging2

    implicit none

    ! parameters
    type(c_ptr),    intent(in)                 :: cptr_sx      !< samples x, y, values
    type(c_ptr),    intent(in)                 :: cptr_sy
    type(c_ptr),    intent(in)                 :: cptr_sv
    integer(c_int), intent(in)                 :: c_nums       ! number of samples
    type(c_ptr),    intent(in)                 :: cptr_cx      ! destination cell center x, y
    type(c_ptr),    intent(in)                 :: cptr_cy
    type(c_ptr),    intent(in)                 :: cptr_cxx     ! destination cell corner x, y
    type(c_ptr),    intent(in)                 :: cptr_cyy
    type(c_ptr),    intent(in)                 :: cptr_cnp     ! destination cell corner array lengths
    integer(c_int), intent(in)                 :: c_numc       ! number of destination cells
    integer(c_int), intent(in)                 :: c_n6         ! max. cell corner array length
    type(c_ptr),    intent(inout)              :: cptr_res     ! return values (ptr to double array)
    integer(c_int), intent(in)                 :: cptr_meth    ! averaging method
    integer(c_int), intent(in)                 :: cptr_nmin    ! minimum nr of samples for avaraging
    real(c_double), intent(in)                 :: cptr_csize   ! relative search cell size
    integer(c_int), intent(in)                 :: jsferic
    integer(c_int), intent(in)                 :: jasfer3D

    ! local variables
    real(c_double), pointer                 :: sx(:)
    real(c_double), pointer                 :: sy(:)
    real(c_double), pointer                 :: svtmp(:)
    integer                                 :: nums
    real(c_double), pointer                 :: cx(:)
    real(c_double), pointer                 :: cy(:)
    real(c_double), pointer                 :: cxtmp(:)
    real(c_double), pointer                 :: cytmp(:)
    integer, pointer                        :: cnp(:)
    integer                                 :: numc
    integer                                 :: n6
    real(c_double), pointer                 :: res(:)
    double precision, allocatable           :: sv(:,:)
    integer, allocatable                    :: ipsam(:)
    double precision, allocatable           :: cz(:,:)
    double precision, allocatable           :: cxx(:,:)
    double precision, allocatable           :: cyy(:,:)
    integer                                 :: meth
    integer                                 :: nmin
    double precision                        :: csize
    integer                                 :: i, j, k, IAVtmp, NUMMINtmp, INTTYPEtmp, ierr
    double precision                        :: RCELtmp
    double precision                        :: dmiss=-999d0

    ! cache interpolation settings
    IAVtmp = IAV
    NUMMINtmp = NUMMIN
    INTTYPEtmp = INTERPOLATIONTYPE
    RCELtmp = RCEL

    ! assign ranges and settings
    nums = c_nums
    numc = c_numc
    n6 = c_n6
    meth = cptr_meth
    nmin = cptr_nmin
    csize = cptr_csize

    !assign pointers
    call c_f_pointer(cptr_sx, sx, (/nums/))
    call c_f_pointer(cptr_sy, sy, (/nums/))
    call c_f_pointer(cptr_sv, svtmp, (/nums/))
    call c_f_pointer(cptr_cx, cx, (/numc/))
    call c_f_pointer(cptr_cy, cy, (/numc/))
    call c_f_pointer(cptr_cxx, cxtmp, (/n6*numc/))
    call c_f_pointer(cptr_cyy, cytmp, (/n6*numc/))
    call c_f_pointer(cptr_cnp, cnp, (/numc/))
    call c_f_pointer(cptr_res, res, (/numc/))

    !allocate & copy to 2d arrays
    allocate(sv(1, nums), ipsam(nums), cz(1,numc), cxx(n6, numc), cyy(n6, numc))

    sv(1,:) = svtmp(:)
    ipsam(:) = 1
    k = 1
    do i = 1, numc
       cz(1,i) = DMISS
       do j=1,n6
          cxx(j, i) = cxtmp(k)
          cyy(j, i) = cytmp(k)
          k = k + 1
       enddo
    enddo

    if(meth > 0 .and. meth < 8) then
       IAV = meth
    else
       goto 1234
    endif

    if(nmin > 0) then
       NUMMIN = nmin
    else
       goto 1234
    endif

    if(csize > 0 .and. csize < 10) then
       RCEL = csize
    else
       goto 1234
    endif

    INTERPOLATIONTYPE = 2

    call build_kdtree(treeglob, nums, sx, sy, ierr, jsferic, dmiss)
    !call averaging2(1, nums, sx, sy, sv, ipsam, cx, cy, cz, numc, cxx, cyy, n6, cnp, 1, dmiss, jsferic, jasfer3D, jins = 1, NPL = 0)
    call delete_kdtree2(treeglob)

    !copy values back
    res(:) = cz(1,:)

1234 continue

    !unroll & cleanup
    IAV = IAVtmp
    NUMMIN = NUMMINtmp
    INTERPOLATIONTYPE = INTTYPEtmp
    RCEL = RCELtmp
    deallocate(sv, ipsam, cz, cxx, cyy)

end subroutine averaging
   

   
end module ec_module_api