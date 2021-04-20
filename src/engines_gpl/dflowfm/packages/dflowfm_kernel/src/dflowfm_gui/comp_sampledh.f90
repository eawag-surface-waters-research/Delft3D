!> compute sample mesh width
double precision function comp_sampleDh(i,j)
   use m_samples
   use geometry_module, only: dbdistance
   use m_missing, only: dmiss
   use m_sferic, only: jsferic, jasfer3D

   implicit none

   integer, intent(in)        :: i,j          !< sample indices

   integer                    :: ip, ipiL, ipiR, ipjL, ipjR

   double precision           :: dum

   if ( MXSAM*MYSAM.ne.NS ) goto 1234  ! structured samples only

   ip = i+(j-1)*MXSAM
   ipiL = max(i-1,1)     + (j-1)*MXSAM
   ipiR = min(i+1,MXSAM) + (j-1)*MXSAM
   ipjL = i + (max(j-1,1)    -1)*MXSAM
   ipjR = i + (min(j+1,MYSAM)-1)*MXSAM

   comp_sampleDh = 0d0
   dum = dbdistance(xs(ip),ys(ip),xs(ipiL),ys(ipiL),jsferic, jasfer3D, dmiss)
   if ( dum.gt.0d0 ) comp_sampleDh = max(comp_sampleDh,dum)
   dum = dbdistance(xs(ip),ys(ip),xs(ipiR),ys(ipiR),jsferic, jasfer3D, dmiss)
   if ( dum.gt.0d0 ) comp_sampleDh = max(comp_sampleDh,dum)
   dum = dbdistance(xs(ip),ys(ip),xs(ipjL),ys(ipjR),jsferic, jasfer3D, dmiss)
   if ( dum.gt.0d0 ) comp_sampleDh = max(comp_sampleDh,dum)
   dum = dbdistance(xs(ip),ys(ip),xs(ipjR),ys(ipjR),jsferic, jasfer3D, dmiss)
   if ( dum.gt.0d0 ) comp_sampleDh = max(comp_sampleDh,dum)

1234 continue

   return
end function comp_sampleDh
