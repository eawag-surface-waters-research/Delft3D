module m_profiles
 ! profile related :
 type tprof                                          !< this is a profile type
   integer                        :: ityp            !< 1 = circle, 2=rectan1dlumped, 3=rectan2d, 9=yzlumped, 10=yzconveyance
   integer                        :: frctp           !< friction type chezy manning etc
   double precision               :: frccf           !< friction coefficient
   double precision               :: width           !< max width
   double precision               :: height          !< max height
   real, allocatable              :: xx(:), yy(:)    !< y z point coordinates
   real, allocatable              :: y (:), z (:)    !< y and z arrays
   real                           :: zmin            ! absolute z lavel of lowest point
 end type tprof

 integer                          :: nprofdefs       !< nr of unique  profile definitions
 type(tprof), allocatable         :: profiles1D(:)   !< these are the profiles

 integer                          :: nproflocs = 0      !< nr of profile locations, always <= nprofdefs
 integer                          :: maxproflocnr       !< highest referred profnr in profloc.xyz
 integer                          :: minproflocnr       !< lowest  referred profnr in profloc.xyz
 double precision, allocatable    :: xpr(:), ypr(:), zpr(:) !< profile locations, x,y,z
 integer,          allocatable    :: npr(:)                 !< at these locations, reference to profdefs
 integer                          :: jainterpolatezk1D = 1
 double precision                 :: tolzprof = 0.1d0
 integer                          :: ntolsave = 0
end module m_profiles
