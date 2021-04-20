!>
!! Trachytope module containing FM data for trachytopes.
!! Note that arrays are dimensioned on the number of net links
!! This was done so that roughness characteristics can be specified
!! not related to the location of open boundaries, which are not
!! yet available at the time the model is constructed.
!! Besides that certain sediment transport formulae require a
!! Cf at the flow node, which can only accurately be deterimined if the
!! values at net links are known.
!!
module m_trachy
 use trachytopes_data_module
 use properties
 implicit none

 type(TREE_DATA), pointer          :: trtdef_ptr                  !< Property tree structure containing the trachytopes definitions from MDU [Trachytopes] chapter.
 !
 !type(trachy_type)                 :: trachy_nl                   !< Structure containing trachytope definitions on net links
 type(trachy_type)                 :: trachy_fl                   !< Structure containing trachytope definitions on flow links
 !
 logical                           :: linit                       !< Logical for initial step of trachytopes computation (not used)
 logical                           :: waqol                       !< Logical for waq-online coupling in trachytopes computation
 logical                           :: lfdxx                       !< Logical for sediment diameters  of trachytopes computation (not used yet)
 !logical                           :: spatial_bedform             !< Logical for inclusion of spatially varying dune properties in trachytopes computation (not used yet)
 logical                           :: update_umag                 !< Logical for updating cell-centred velocity magnitude in trachytopes computation
 logical                           :: trachy_resistance = .false. !< Logical for additional resistance term in momentum equation
 !
 !double precision, allocatable     :: rhosol(:)                   !< Density of sediment (lsedtot)
 double precision, allocatable     :: sig(:)                      !< sigma layer notation as in Delft3D in trachytopes computation
 double precision, allocatable     :: umag(:)                     !< velocity magnitude in trachytopes computation (ndx)
 !double precision, allocatable     :: bedformD50(:)               !< 50-th percentile of the sediment considered for bedform in trachytopes computation (ndx)
 !double precision, allocatable     :: bedformD90(:)               !< 90-th percentile of the sediment considered for bedform in trachytopes computation (ndx)
 !double precision, allocatable     :: rksr(:)                     !< roughness due to ripples in trachytopes computation (cf. Van Rijn 20..) (ndx)
 !double precision, allocatable     :: rksmr(:)                    !< roughness due to mega-ripples in trachytopes computation (cf. Van Rijn 20..) (ndx)
 !double precision, allocatable     :: rksd(:)                     !< roughness due to dunes in trachytopes computation (cf. Van Rijn 20..)(ndx)
 !double precision, allocatable     :: dxx(:,:)                    !< sediment percentiles in trachytopes computation (cf. Van Rijn 20..) (ndx,nxx)
 double precision, allocatable     :: z0rou(:)                    !< z0rou in trachytopes computation (numl)
 double precision, allocatable     :: hu_trt(:)                   !< water depth on net links in trachytopes computation (numl)
 double precision, allocatable     :: dx_trt(:)                   !< length of net links in trachytopes computation (numl)
 !
 integer                           :: kmaxtrt                     !< number of sigma layers as in Delft3D in trachytopes computation
 !integer                           :: lsedtot                     !< number of sediment fractions in trachytopes computation
 !integer                           :: i50                         !< index of sediment percentile in trachytopes computation
 !integer                           :: i90                         !< index of sediment percentile in trachytopes computation
 integer                           :: itimtt                      !< update frequency (multiple of dt_max, default = 1 --> every dt_max timestep)
 !integer                           :: nxx                         !< dimension of sediment percentiles in trachytopes computation
 integer, allocatable              :: kcu_trt(:)                  !< temporary array for kcu on net-links instead of flow-links in trachytopes computation (numl)
 !
 character(4)                      :: rouflo                      !< roughness method as described by Delft3D in trachytopes computation
 !
contains

!> Sets ALL (scalar) variables in this module to their default values.
!! Make sure to call this at least once for each newly loaded model.
subroutine default_trachy()
   call tree_destroy(trtdef_ptr)
   linit = .false.             !< Logical for initial step of trachytopes computation (not used)
   waqol = .false.             !< Logical for waq-online coupling in trachytopes computation
   lfdxx = .false.             !< Logical for sediment diameters  of trachytopes computation (not used yet)
   !spatial_bedform = .false.   !< Logical for inclusion of spatially varying dune properties in trachytopes computation (not used yet)
   !lsedtot = 1                 !< number of sediment fractions in trachytopes computation
   !i50 = 1                     !< index of sediment percentile in trachytopes computation
   !i90 = 2                     !< index of sediment percentile in trachytopes computation
   itimtt = 1                  !< update frequency (multiple of dt_user, default = 1 --> every dt_user timestep)
   !nxx = 2                     !< dimension of sediment percentiles in trachytopes computation

end subroutine default_trachy

end module m_trachy
