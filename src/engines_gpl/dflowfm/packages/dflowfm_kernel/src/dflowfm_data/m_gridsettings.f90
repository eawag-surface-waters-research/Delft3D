!> Regular grid generation settings. All orthogonalisation settings are in
!! module m_orthosettings.
MODULE M_GRIDSETTINGS
implicit none

integer :: MFAC  = 2000 !< M-refinement factor for regular grid generation.
integer :: NFAC  = 40 !< N-refinement factor for regular grid generation.
integer :: ITSMO = 10 !< Nr. of inner iterations in regular grid smoothing.
integer :: ITSMA      !< Not in use, old rgfgrid
integer :: JADEPDESIGN = 0
integer :: MDESIGN
double precision :: BFAC=1d0, CSMO = 0.5d0, RFAC
double precision :: SRM,SRN,DEPSLO,FSMA, ALINEN, ALINEM
INTEGER :: KEEPSTARTDIR = 1
double precision :: BAAS2 = 0.5d0, FACMIR = 1.2d0
double precision :: SPLFAC, SPLFAC2
INTEGER :: JDEMO = 0

! Pillar grid settings
double precision :: pil_rad  = 0d0  !< pillar radius
double precision :: pil_x    = 0d0  !< pillar center point x-coordinate
double precision :: pil_y    = 0d0  !< pillar center point y-coordinate
double precision :: pil_grow = 1d0   !< pillar grid growth factor *not used*

END MODULE M_GRIDSETTINGS
