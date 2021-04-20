module m_partparallel
   double precision, dimension(:,:), allocatable :: worksnd, workrecv   ! work arrays for sending/receiving data

   integer,          dimension(:),   allocatable :: icellother   ! cell number in other domain
   integer,          dimension(:),   allocatable :: isend, jsend ! send list in CRS-format
   integer,          dimension(:),   allocatable :: jrecv        ! start pointers in receive list
   integer,          dimension(:),   allocatable :: jpoint       ! work array
   integer,          dimension(:),   allocatable :: irequest

   integer,          dimension(:), allocatable   :: numsendarr
   integer,          dimension(:), allocatable   :: numrecvarr

   integer                                       :: NDIM         ! data dimension per particle in send/receive arrays
   integer                                       :: INDX_XPART=1
   integer                                       :: INDX_YPART=2
   integer                                       :: INDX_DTREM=3
   integer                                       :: INDX_IGLOB=4
   integer                                       :: INDX_KPART=5
   integer                                       :: INDX_ZPART=0

   integer                                       :: japartsaved   ! particles saved to work array (1) or not (safety)
end module m_partparallel
