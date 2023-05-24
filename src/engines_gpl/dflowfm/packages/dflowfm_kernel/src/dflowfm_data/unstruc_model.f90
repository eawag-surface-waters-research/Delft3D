!----lAGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2023.
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

! 
! 

!> Manages the unstruc model definition for the active problem.
module unstruc_model

use precision
use properties
use tree_data_types
use tree_structures
use unstruc_messages
use m_globalparameters, only : t_filenames
use time_module, only : ymd2modified_jul, datetimestring_to_seconds
use dflowfm_version_module, only: getbranch_dflowfm

implicit none

    !> The version number of the MDU File format: d.dd, [config_major].[config_minor], e.g., 1.03
    !!
    !! Note: read config_minor as a 2 digit-number, i.e., 1.1 > 1.02 (since .1 === .10 > .02).
    !! Convention for format version changes:
    !! * if a new format is backwards compatible with old MDU files, only
    !!   the minor version number is incremented.
    !! * if a new format is not backwards compatible (i.e., old MDU files
    !!   need to be converted/updated by user), then the major version number
    !!   is incremented.

    ! MDUFormatVersion = 1.08
    integer, parameter       :: MDUFormatMajorVersion = 1
    integer, parameter       :: MDUFormatMinorVersion = 9

    ! History MDUFormatVersion:

    ! 1.09 (2019-08-21): Renamed [geometry] roughnessFiles to frictFile.
    ! 1.08 (2019-07-27): Default option for density changed from Eckart to UNESCO (idensform=2 instead of 1)
    ! 1.07 (2019-06-13): Renamed [model] block as [General] block, replace keyword MDUFormatVersion by FileVersion
    ! 1.06 (2016-05-16): Removed 1 variable for secondary flow, EffectSpiral as it is given by Espir contained in .mor file
    ! 1.05 (2015-07-22): The structure parameters are added (jahisstr, jahisdam, jahispump, jahisgate)
    ! 1.04 (2015-03-19): Anti-Creep option is added
    ! 1.03 (2015-02-25): Added 2 variable for secondary flow, EffectSpiral and BetaSpiral
    ! 1.02 (2015-01-07): Remove [time] AutoTimestep (always automatic).
    ! 1.01 (2014-11-10): Renamed ThindykeFile/Scheme/Contraction -> FixedWeirFile/Scheme/Contraction.
    ! 1.00 (2014-09-22): first version of new permissive checking procedure. All (older) unversioned input remains accepted.

    integer, parameter :: ExtfileNewMajorVersion = 2
    integer, parameter :: ExtfileNewMinorVersion = 1
    ! History ExtfileNewVersion:
    ! 2.00 (2019-08-06): enabled specifying "nodeId" in a 1D network node.
    ! 2.01 (2019-12-04): optional fields targetMaskFile and targetMaskInvert for [Meteo] blocks.


    !> The version number of the 1D2DFile format: d.dd, [config_major].[config_minor], e.g., 1.03
    !!
    !! Note: read config_minor as a 2 digit-number, i.e., 1.1 > 1.02 (since .1 === .10 > .02).
    !! Convention for format version changes:
    !! * if a new format is backwards compatible with old 1D2D files, only
    !!   the minor version number is incremented.
    !! * if a new format is not backwards compatible (i.e., old 1D2D files
    !!   need to be converted/updated by user), then the major version number
    !!   is incremented.

    ! File1D2DLinkMajorVersion = 1.00
    integer, parameter       :: File1D2DLinkMajorVersion = 1
    integer, parameter       :: File1D2DLinkMinorVersion = 0

    ! History File1D2DLinkVersion:
    ! 1.00 (2019-12-04): Initial version.


    integer, parameter :: MD_NOAUTOSTART   = 0   !< Do not autostart (nor stop) this model.
    integer, parameter :: MD_AUTOSTART     = 1   !< Autostart this model and then idle.
    integer, parameter :: MD_AUTOSTARTSTOP = 2   !< Autostart this model and then exit (batchmode)

    type(tree_data), pointer, public :: md_ptr   !< Unstruc Model Data in tree_data
    character(len=64)  :: md_ident         = ' ' !< Identifier of the model, used as suggested basename for some files. (runid)
    character(len=64)  :: md_mdu           = ' ' !< similar, used in parsing parsing commandline
    character(len=64)  :: md_ident_sequential = ' ' !< Sequential model identifier, used for parallel outputdir

    character(len=64)  :: md_specific      = ' ' !< Optional 'model specific ID', read from MDU, to enable certain custom runtime function calls (instead of via MDU name/md_ident).

    character(len=4)   :: md_tunit         = ' ' !< Unit of tstart_user and tstop_user (only for read and write, while running these are always in seconds).

    integer            :: md_paths_relto_parent = 0 !< Option whether or not (1/0) to resolve filenames (e.g. inside the *.ext file) w.r.t. their direct parent, instead of the toplevel MDU working dir. (UNST-1144)
    type(t_filenames)  :: md_1dfiles
    character(len=255) :: md_netfile       = ' ' !< Net definition                    (e.g., *_net.nc)
    character(len=255) :: md_flowgeomfile  = ' ' !< Storing flow geometry (output)    (e.g., *_flowgeom.nc)
    character(len=255) :: md_dryptsfile    = ' ' !< Dry points file (list)            (e.g., *.xyz, *.pol)
    character(len=255) :: md_encfile       = ' ' !< Enclosure file (list)             (e.g., *.xyz, *.pol)
    character(len=255) :: md_s1inifile     = ' ' !< Initial water levels sample file using floodfill  (e.g., *.xyz)
    character(len=255) :: md_ldbfile       = ' ' !< Land boundary file    (show)      (e.g., *.ldb)
    character(len=255) :: md_plifile       = ' ' !< polylinefile file     (show)      (e.g., *.pli)
    character(len=255) :: md_thdfile       = ' ' !< Thin dam file (polygons)          (e.g., *_thd.pli) (block flow)
    character(len=255) :: md_cutcelllist   = ' ' !< contains list of cutcell polygons (e.g., *_cut.lst)
    character(len=255) :: md_fixedweirfile = ' ' !< Fixed weir pliz's                 (e.g., *_fxw.pli), = pli with x,y, Z  column
    character(len=255) :: md_pillarfile    = ' ' !< pillar pliz's                     (e.g., *_pillar.pli), = pli with x,y, diameter and Cd columns
    character(len=255) :: md_roofsfile     = ' ' !< Roof pliz's                      (e.g., *_roof.pli), = pli with x,y, Z  column
    character(len=255) :: md_gulliesfile   = ' ' !< gullies pliz's                    (e.g., *_gul.pli), = pli with x,y, Z  column
    character(len=255) :: md_vertplizfile  = ' ' !< Vertical layering pliz's          (e.g., *_vlay.pliz), = pliz with x,y, Z, first Z =nr of layers, second Z = laytyp
    character(len=255) :: md_proflocfile   = ' ' !< X,Y,and a profile reference nr    (e.g., *_profloc.xyz)
    character(len=255) :: md_profdeffile   = ' ' !< Profile definition of these nrs   (e.g., *_profdef.txt)
    character(len=255) :: md_profdefxyzfile= ' ' !< XYZ profile definition in pliz of these nrs ic yz-def (e.g., *_xyzprof.pliz)
    character(len=255) :: md_1d2dlinkfile  = ' ' !< File containing custom parameters for 1D2D links (e.g., *.ini)
    character(len=255) :: md_pipefile      = ' ' !< File containing pipe-based 'culverts' (e.g., *.pliz)
    character(len=255) :: md_shipdeffile   = ' ' !< File containing shipdefinition    (e.g., *.shd)
    character(len=255) :: md_inifieldfile  = ' ' !< File of initial fields            (e.g., *.ini)

    character(len=255) :: md_restartfile   = ' ' !< File containing map-files to restart a computation          (e.g., *_map.nc), input only, NOT used for storing the names of output restart files.

    character(len=255) :: md_extfile       = ' ' !< External forcing specification file (e.g., *.ext)
    character(len=255) :: md_extfile_new   = ' ' !< External forcing specification file new style (bct format), (e.g., *.ext)
    character(len=255) :: md_extfile_dir   = ' ' !< Directory containing the old-style external forcing specification file (e.g., *.ext) (relative to MDU/current working dir)


    character(len=255) :: md_structurefile = ' ' !< Structure file, (e.g., *.ini)
    character(len=255) :: md_structurefile_dir = ' ' !< Directory containing the structure file (e.g., *.ini) (relative to MDU/current working dir).

    character(len=255) :: md_wavefile      = ' ' !< File containing wave input (e.g., *_wave.nc)
    character(len=255) :: md_surfbeatfile      = ' ' !< File containing surfbeat input (e.g., params.txt)

    character(len=255) :: md_sedfile       = ' ' !< File containing sediment characteristics (e.g., *.sed)
    character(len=255) :: md_morfile       = ' ' !< File containing morphology settings (e.g., *.mor)
    character(len=255) :: md_dredgefile    = ' ' !< File containing dredging settings (e.g., *.dad)
    character(len=255) :: md_bedformfile   = ' ' !< File containing bedform settings (e.g., *.bfm)
    character(len=255) :: md_morphopol     = ' ' !< File containing boundaries of morphologic change extent (e.g., *.pol)
    character(len=255) :: md_sedtrailsfile = ' ' !< File containing extent of sedtrails output grid

    character(len=1024):: md_obsfile       = ' ' !< File containing observation points  (e.g., *_obs.xyn, *_obs.ini)
    character(len=255) :: md_crsfile       = ' ' !< File containing cross sections (e.g., *_crs.pli, observation cross section *_crs.ini)
    character(len=255) :: md_rugfile       = ' ' !< File containing runup gauges (e.g., *_rug.pli)
    character(len=255) :: md_foufile       = ' ' !< File containing fourier modes to be analyzed

    character(len=255) :: md_hisfile       = ' ' !< Output history file for monitoring  (e.g., *_his.nc)
    character(len=255) :: md_mapfile       = ' ' !< Output map     file for full flow fields (e.g., *_map.nc)
    character(len=255) :: md_classmapfile  = ' ' !< Output classmap file for full flow fields in classes (formerly: incremental file) (e.g., *_clm.nc)
    character(len=255) :: md_comfile       = ' ' !< Output com     file for communication (e.g., *_com.nc)
    character(len=255) :: md_timingsfile   = ' ' !< Output timings file (auto-set)
    character(len=255) :: md_avgwavquantfile = ' ' !< Output map file for time-averaged wave output (e.g., *_wav.nc)
    character(len=255) :: md_avgsedquantfile = ' ' !< Output map file for time-averaged sedmor output (e.g., *_sed.nc)
    character(len=255) :: md_avgsedtrailsfile = ' ' !< Output map file for time-averaged sedtrails output (e.g., *_sedtrails.nc)
    character(len=255) :: md_waqfilebase   = ' ' !< File basename for all Delwaq files. (defaults to md_ident)
    character(len=255) :: md_waqoutputdir  = ' ' !< Output directory for all WAQ communication files (waqgeom, vol, flo, etc.)
    character(len=255) :: md_waqhoraggr    = ' ' !< DELWAQ output horizontal aggregation file (*.dwq)
    character(len=255) :: md_waqvertaggr   = ' ' !< DELWAQ output vertical aggregation file (*.vag)

    character(len=255) :: md_partitionfile = ' ' !< File with domain partitioning polygons (e.g. *_part.pol)
    character(len=255) :: md_outputdir     = ' ' !< Output directory for map-, his-, rst-, dat- and timings-files

!   particles
    character(len=255) :: md_partfile      = ' ' !< intitial particles file (*.xyz)
    character(len=255) :: md_partrelfile   = ' ' !< particles release file (*.txyz)
    integer            :: md_partjatracer      = 0
    double precision   :: md_partstarttime     = 0d0
    double precision   :: md_parttimestep      = 0d0
    integer            :: md_part3Dtype        = 0 !< 0: depth averaged, 1: free surface

!   processes (WAQ)
    character(len=255) :: md_subfile = ' '      !< substance file
    character(len=255) :: md_ehofile = ' '      !< extra history output file
    character(len=255) :: md_pdffile = ' '      !< [-] process library file
    character(len=255) :: md_oplfile = ' '      !< [-] open process library dll/so file
    character(len=255) :: md_blmfile = ' '      !< [-] BLOOM aglae species definition file
    character(len=255) :: md_sttfile = ' '      !< statistics definition file
    double precision   :: md_thetav_waq = 0d0   !< thetav for waq
    double precision   :: md_dt_waqproc = 0d0   !< processes time step
    double precision   :: md_dt_waqbal = 0d0    !< mass balance output time step (old)
    integer            :: md_flux_int = 1       !< process fluxes integration option (1: WAQ, 2: D-Flow FM)
    integer            :: md_wqbot3D_output = 0 !< write 3D wqbot output

    ! TODO: reading for trachytopes is still within rdtrt, below was added for partitioning (when no initialization)
    character(len=4)   :: md_trtrfile     = ' ' !< Variable that stores information if trachytopes are used ('Y') or not ('N')
    character(len=255) :: md_trtdfile     = ' ' !< File containing trachytopes definitions
    character(len=255) :: md_trtlfile     = ' ' !< File containing distribution of trachytope definitions
    integer            :: md_mxrtrach     = 8   !< Maximum recursion level for combined trachytope definitions
    character(len=255) :: md_trtcllfile   = ' ' !< Overall calibration factor file for roughness from trachytopes (see also [calibration] block)
    double precision   :: md_mnhtrach     = 0.1d0 !< Minimum water depth for roughness computations
    integer            :: md_mthtrach     = 1   !< Area averaging method, 1: Nikuradse k based, 2: Chezy C based (parallel and serial)

    character(len=255) :: md_mptfile       = ' ' !< File (.mpt) containing fixed map output times w.r.t. RefDate (in TUnit)

! calibration factor
    character(len=256) :: md_cldfile     = ' ' !< File containing calibration definitions
    character(len=256) :: md_cllfile     = ' ' !< File containing distribution of calibration definitions area percentage

! incremental output
    character(len=256) :: md_classmap_file = ' ' !< File for output of classes output

    character(len=200) :: md_snapshotdir   = ' ' !< Directory where hardcopy snapshots should be saved.
                                                 !! Created if non-existent.

    integer            :: md_jaAutoStart     = MD_NOAUTOSTART !< Autostart simulation after loading or not.
    integer            :: md_input_specific  = 0 !< use (0: no, 1: yes) specific hardcoded input.
    integer            :: md_snapshot_seqnr  = 0 !< Sequence number of last snapshot file written.
!   partitioning command line options
    integer            :: md_japartition     = 0   !< partition (1) or not (0)
    integer            :: md_pmethod         = 1   !< partition method: K-way (=1, default), Recursive Bisection(=2), Mesh-dual(=3)
    integer            :: md_ndomains        = 0   !< METIS/number of domains (>0) or use polygon (0)
    integer            :: md_jacontiguous    = 1   !< METIS/contiguous domains (1, default) or not (0)
    integer            :: md_icgsolver       = 0   !< intended solver
    integer            :: md_genpolygon      = 0   !< generate partition polygons and use it in parallel runs (1) or writing cell subdomain information to partitioned net files (0)
    integer            :: md_partugrid       = 0   !< partitioned netfile is ugrid or not
    integer            :: md_partseed        = 0   !< User-defined seed value, passed to METIS. Useful for reproducible partitionings, but only used when /= 0.
    integer            :: md_jaopenGL        = 0   !< use openGL (1) or not (0)
    integer            :: md_jagridgen       = 0   !< Commandline-based simple grid generation.
    integer            :: md_jarefine        = 0   !< sample based mesh refinement or not
    integer            :: md_jamake1d2dlinks = 0   !< Make 1D2D links from commandline (1) or not (0)
    integer            :: md_numthreads      = 0   !< sample based mesh refinement or not
    integer            :: md_jatest          = 0      !< only perform a (speed)test (1), or not (0)
    integer            :: md_M               = 1024   !< size of x in Axpy
    integer            :: md_N               = 2048   !< size of y in Axpy
    integer            :: md_Nruns           = 10     ! number of test runs
    integer            :: md_soltest         = 0      !< solver test (1) or not (0)
    integer            :: md_CFL             = 0      !< wave-based Courant number (if > 0)
    integer            :: md_maxmatvecs      = 0      !< maximum number of matrix-vector multiplications in Krylov solver (if > 0 )
    integer            :: md_epscg           = 0      !< -10log(epscg) (if > 0), tolerance in (inner) Krylov iterations
    integer            :: md_epsdiff         = 0      !< -10log(epsdiff) (if > 0), tolerance in (outer) Schwarz iterations
    integer            :: md_convnetcells    = 0      !< Convert _net.nc files with only netnodes/links into _net.nc files with netcell info.
    integer            :: md_findcells       = 0      !< read netcell info from files and bypass findcells. If not 0, findcells are called.
    integer            :: md_pressakey       = 0      !< press a key (1) or not (0)
    character(len=128) :: md_cfgfile         = ' '    !< cfg-file
    integer            :: md_jasavenet       = 0      !< save network after initialization (1) or not (0)
    integer            :: md_cutcells        = 0
    integer            :: npolf              = 0      !< nr of polygonplotfiles saved with n key in editpol
    integer            :: md_usecaching      = 1      !< Use the caching file if it exists (1) or not (0)

    integer            :: md_convertlongculverts = 0      !< convert culverts (and exit program) yes (1) or no (0)
    character(len=128) :: md_culvertprefix   = ' '    !< prefix for generating long culvert files
    
!   map file output format
    integer,            parameter             :: NUMFORMATS      = 4

    integer,            parameter             :: IFORMAT_NETCDF  = 1
    integer,            parameter             :: IFORMAT_TECPLOT = 2
    integer,            parameter             :: IFORMAT_NETCDF_AND_TECPLOT = 3
    integer,            parameter             :: IFORMAT_UGRID   = 4

    character(len=128), dimension(NUMFORMATS) :: SFORMATNAMES    = [character(len=128) :: &
                                                                    'netCDF', &
                                                                    'Tecplot', &
                                                                    'netCFD and Tecplot', &
                                                                    'NetCDF-UGRID' ]

    integer                                   :: md_mapformat !< map file output format (one of IFORMAT_*)
    integer                                   :: md_unc_conv  !< Unstructured NetCDF conventions (either UNC_CONV_CFOLD or UNC_CONV_UGRID)

    integer                                   :: md_ncformat  !< NetCDF format (3: classic, 4: NetCDF4+HDF5)

    integer                                   :: md_fou_step  !< determines if fourier analysis is updated at the end of the user time step or comp. time step
contains


!> Resets current model variables, generally prior to loading a new MDU.
!!
!! Add initialization/default values for all module variables here.
subroutine resetModel()
use m_trachy, only: trtdef_ptr
use unstruc_netcdf, only: UNC_CONV_UGRID
use unstruc_channel_flow

    call tree_destroy(md_ptr)
    nullify(trtdef_ptr) ! trtdef_ptr was only pointing to subtree of md_ptr, so is now a dangling pointer: model's responsibility to nullify it here.

    network%loaded = .false.
    network%initialized = .false.

    md_ident   = ' '
    md_mdu = ' ' ! TODO: AvD: I believe the (newer) md_mdu may be an unnecessary duplicate of md_ident_sequential
    md_ident_sequential = ' '
    md_tunit   = 'S'

    md_specific = ' '

    md_paths_relto_parent = 0

    md_netfile = ' '
    md_1dfiles%cross_section_definitions = ' '
    md_1dfiles%cross_section_locations = ' '
    md_1dfiles%roughness = ' '
    md_1dfiles%roughnessdir = ' '
    md_1dfiles%storage_nodes = ' '
    md_1dfiles%structures = ' '

    md_flowgeomfile = ' '
    md_dryptsfile = ' '
    md_encfile = ' '
    md_s1inifile = ' '
    md_ldbfile = ' '
    md_thdfile = ' '
    md_fixedweirfile = ' '
    md_pillarfile = ' '
    md_roofsfile     = ' '
    md_gulliesfile   = ' '
    md_vertplizfile = ' '
    md_proflocfile = ' '
    md_profdeffile = ' '
    md_profdefxyzfile = ' '
    md_1d2dlinkfile = ' '
    md_shipdeffile = ' '
    md_restartfile = ' '
    md_extfile = ' '
    md_extfile_new  = ' '
    md_extfile_dir = ' '
    md_structurefile = ' '
    md_structurefile_dir = ' '
    md_wavefile = ' '
    md_surfbeatfile = ' '

    md_sedfile = ' '
    md_morfile = ' '
    md_dredgefile = ' '
    md_bedformfile = ' '
    md_morphopol = ' '
    md_sedtrailsfile = ' '

    md_obsfile = ' '
    md_crsfile = ' '
    md_rugfile = ' '
    md_foufile = ' '
    md_hisfile = ' '
    md_mapfile = ' '
    md_classmapfile = ' '
    md_comfile = ' '
    md_timingsfile = ' '
    md_avgwavquantfile = ' '
    md_avgsedquantfile = ' '
    md_waqfilebase   = ' '
    md_waqoutputdir  = ' '
    md_partitionfile = ' '
    md_outputdir     = ' '
    md_partfile      = ' '
    md_partrelfile   = ' '
    md_trtrfile     = ' '
    md_trtdfile     = ' '
    md_trtlfile     = ' '
    md_mptfile = ' '
    md_classmap_file = ' '

    md_snapshotdir   = ' '

    md_mapformat    = IFORMAT_UGRID   !< map file output format (one of IFORMAT_*)
    md_unc_conv     = UNC_CONV_UGRID  ! TODO: AvD: does this double now with IFORMAT_UGRID?

    md_ncformat     = 3               !< NetCDF format (3: classic, 4: NetCDF4+HDF5)

    md_fou_step     = 0               !< default: fourier analysis is updated on a user-timestep basis

    md_jaAutoStart     = MD_NOAUTOSTART !< Autostart simulation after loading or not.

    md_partfile = ' '
    md_partrelfile = ' '
    md_partjatracer = 0
    md_partstarttime = 0d0
    md_parttimestep = 0d0
    md_part3Dtype = 0

    md_cfgfile = ' '

    md_usecaching = 1      !< Use the caching file if it exists (1) or not (0)

    ! The following settings are intentionally *not* reset for each model.
    !md_snapshot_seqnr  = 0 ! not handy in practice, it destroys previous plots without warning
    !md_japartition     = 0   !< partition (1) or not (0)
    !md_pmethod         = 1   !< partition method: K-way (=1, default), Recursive Bisection(=2), Mesh-dual(=3)
    !md_ndomains        = 0   !< METIS/number of domains (>0) or use polygon (0)
    !md_jacontiguous    = 0   !< METIS/contiguous domains (1) or not (0)
    !md_icgsolver       = 0   !< intended solver
    !md_jaopenGL        = 0   !< use openGL (1) or not (0)
    !md_jagridgen       = 0   !< Commandline-based simple grid generation.
    !md_jarefine        = 0   !< sample based mesh refinement or not
    !md_numthreads      = 0   !< sample based mesh refinement or not
    !md_jatest          = 0      !< only perform a test (1) or not (0)
    !md_M               = 1024   !< size of x in Axpy
    !md_N               = 2048   !< size of y in Axpy
    !md_Nruns           = 10     !< number of test runs
    !md_soltest         = 0      !< solver test (1) or not (0)
    !md_CFL             = 0      !< wave-based Courant number (if > 0)
    !md_icgsolver       = 0      !< overwrite solver type (if > 0)
    !md_maxmatvecs      = 0      !< maximum number of matrix-vector multiplications in Krylov (if > 0 )
    !md_epscg           = 0      !< -10log(epscg) (if > 0), tolerance in (inner) Krylov iterations
    !md_epsdiff         = 0      !< -10log(epsdiff) (if > 0), tolerance in (outer) Schwarz iterations
    !md_convnetcells    = 0      !< Convert _net.nc files with only netnodes/links into _net.nc files with netcell info.
    !md_findcells       = 0      !< If it is not zero, then the codes call findcells
    !md_pressakey       = 0      !< press a key (1) or not (0)
    !md_jasavenet = 0

end subroutine resetModel

!> Loads a model definition from file and makes it active.
subroutine loadModel(filename)
    use timers
    use m_readstructures
    use m_netw
    use m_observations
    use m_monitoring_crosssections
    use m_monitoring_runupgauges
    use m_thindams
    use m_flow, only: isimplefixedweirs
    use m_polygon
    use m_fixedweirs
    use m_partitioninfo
    use string_module
    use m_sediment
    use m_alloc
    use m_cross_helper
    use m_flow1d_reader
    use m_flowexternalforcings, only: pillar
    use m_sferic
    ! use string_module, only: get_dirsep
    use unstruc_caching
    use m_longculverts
    use unstruc_channel_flow
    use unstruc_netcdf, only: unc_meta_net_file
    use system_utils, only: remove_path

    interface
       subroutine realan(mlan, antot)
          integer, intent(inout)                ::  mlan
          integer, intent(inout), optional      ::  antot
       end subroutine realan
    end interface

    character(*), intent(inout)  :: filename !< Name of file to be read (in current directory or with full path).

    character(len=200), dimension(:), allocatable       :: fnames
    character(len=1024) :: fnamesstring
    double precision, dimension(2) :: tempbob

    integer :: istat, minp, ifil, jadoorladen

    integer :: i
    integer :: L, k1, k2
    integer :: ntot_lb
    integer :: handle_loadModel
    integer :: timerHandle

    handle_loadModel = 0
    call timstrt('Load model', handle_loadModel)

    call resetModel()

    call setmd_ident(filename)

    call readMDUFile(filename, istat)

    if (istat /= 0) then
        return
    end if

    if (len_trim(md_extfile) > 0) then
       ! DEBUG: call convert_externalforcings_file(md_extfile)
    end if

    ! load the caching file - if there is any
    call loadCachingFile(md_ident, md_netfile, md_usecaching)

    ! read and proces dflow1d model
    ! This routine is still used for Morphology model with network in INI-File (Willem Ottevanger)


       jadoorladen = 0

    timerHandle = 0
    call timstrt('Load network', timerHandle)
    call loadNetwork(md_netfile, istat, jadoorladen)
    if (istat == 0) then
       ! Pass a copy of netfile name to unstruc_netcdf to avoid cyclic dependency.
       call remove_path(md_netfile, unc_meta_net_file)
    end if
    call timstop(timerHandle)
    
    if (useVolumeTables .and. ( network%loaded == .false. ) ) then
        useVolumeTables = .false.
    end if 

    network%sferic = jsferic==1

    threshold_abort = LEVEL_FATAL
    if (istat == 0 .and. jadoorladen == 0 .and. network%numk > 0 .and. network%numl > 0) then 
       timerHandle = 0
       call timstrt('Read 1d attributes', timerHandle)
       call read_1d_attributes(md_1dfiles, network)
       call timstop(timerHandle)
    endif
    
       timerHandle = 0
       call timstrt('Read structures', timerHandle)
       if (len_trim(md_1dfiles%structures) > 0) then
          call SetMessage(LEVEL_INFO, 'Reading Structures ...')
          call readStructures(network, md_1dfiles%structures)
          call SetMessage(LEVEL_INFO, 'Reading Structures Done')

          if ( md_convertlongculverts == 0) then
             fnamesstring = md_1dfiles%structures
             call strsplit(fnamesstring,1,fnames,1)
             call loadLongCulvertsAsNetwork(fnames(1), 0, istat)
             do ifil=2,size(fnames)
                call loadLongCulvertsAsNetwork(fnames(ifil), 1, istat)
             end do
             deallocate(fnames)
             if (.not. newculverts .and. nlongculverts > 0) then
                call finalizeLongCulvertsInNetwork()
             end if
          end if
       end if

       call timstop(timerHandle)

       ! set administration arrays and fill cross section list. So getbobs for 1d can be called.
       call timstrt('Initialise 1d administration', timerHandle)
       call initialize_1dadmin(network, network%numl, numl)
       call timstop(timerHandle)

    if (getMaxErrorLevel() >= LEVEL_ERROR) then
       msgbuf = 'loadModel for '''//trim(filename)//''': Errors were found, please check the diagnostics file.'
       call fatal_flush()
    endif
    
  
    ! fill bed levels from values based on links
    do L = 1,  network%numl
       tempbob = getbobs(network, L)
       if (tempbob(1) > 0.5d0* huge(1d0)) tempbob(1) = dmiss
       if (tempbob(2) > 0.5d0* huge(1d0)) tempbob(2) = dmiss

       k1 = kn(1,L)
       k2 = kn(2,L)
       if (zk(k1) == dmiss) then
          zk(k1) = tempbob(1)
       endif
       if (zk(k2) == dmiss) then
          zk(k2) = tempbob(2)
       endif
       zk(k1) = min(zk(k1),tempbob(1))
       zk(k2) = min(zk(k2),tempbob(2))
    enddo    

    ! Load land boundary from file.
    if (len_trim(md_ldbfile) > 0) then
        call strsplit(md_ldbfile,1,fnames,1)
        call oldfil (minp, fnames(1))
        ntot_lb = 0
        if (minp /= 0) call realan(minp, ntot_lb)
        do ifil=2,size(fnames)
           call oldfil(minp, fnames(ifil))
           if (minp /= 0) call realan(minp, ntot_lb)
        enddo
        deallocate(fnames)
    end if

    ! Load observations from file.
    timerHandle = 0
    call timstrt('Read polygon type information', timerHandle)
    call deleteObservations()
    if (len_trim(md_obsfile) > 0) then
      call strsplit(md_obsfile,1,fnames,1)
      call loadObservations(fnames(1), 0)
      do ifil=2,size(fnames)
         call loadObservations(fnames(ifil), 1)
      enddo
      deallocate(fnames)
   end if

    threshold_abort = LEVEL_ERROR
    if (getMaxErrorLevel() >= LEVEL_ERROR) then
       msgbuf = 'loadModel for '''//trim(filename)//''': Errors were found, please check the diagnostics file.'
       call fatal_flush()
    end if

   ! Cross sections in two steps
   call delCrossSections()
   call delpol()

   ! Load thin dam polygons from file.
   if (len_trim(md_thdfile) > 0) then
      call strsplit(md_thdfile,1,fnames,1)
      call oldfil(minp, fnames(1))
      call reapol(minp,0)
      do ifil=2,size(fnames)
         call oldfil(minp, fnames(ifil))
         call reapol(minp,1)
      enddo
      call pol_to_thindams(xpl, ypl, npl)
      deallocate(fnames)
   end if

   ! Load fixed weirs polygons from file.
   if (len_trim(md_fixedweirfile) > 0) then
      call strsplit(md_fixedweirfile,1,fnames,1)
      if (isimplefixedweirs == 0) then
         call oldfil(minp, fnames(1))
         call reapol(minp, 0)
         do ifil=2,size(fnames)
            call oldfil(minp, fnames(ifil))
            call reapol(minp, 1)
         enddo
         call pol_to_flowlinks(xpl, ypl, zpl, npl, nfxw, fxw)
      endif
      deallocate(fnames)
   end if

   ! Load pillar polygons from file.
   if (len_trim(md_pillarfile) > 0) then
      call strsplit(md_pillarfile,1,fnames,1)
      i = size(fnames)
      if (allocated(pillar)) deallocate(pillar)
      allocate( pillar(i) )
      do ifil=1,size(fnames)
         call oldfil(minp, fnames(ifil))
         call reapol(minp, 1)
         allocate( pillar(ifil)%xcor(npl) ) ; pillar(ifil)%xcor = dmiss
         allocate( pillar(ifil)%ycor(npl) ) ; pillar(ifil)%ycor = dmiss
         allocate( pillar(ifil)%dia(npl)  ) ; pillar(ifil)%dia  = dmiss
         allocate( pillar(ifil)%cd(npl)   ) ; pillar(ifil)%cd   = dmiss
         pillar(ifil)%np = npl
         do i = 1,npl
            pillar(ifil)%xcor(i) = xpl(i)
            pillar(ifil)%ycor(i) = ypl(i)
            pillar(ifil)%dia(i)  = zpl(i)
            pillar(ifil)%cd(i)   = dzl(i)
         enddo
      enddo
      deallocate(fnames)
   end if
   ! Load cross sections polygons from file, and load observation cross sections from *.ini files
   if (len_trim(md_crsfile) > 0) then
      call strsplit(md_crsfile,1,fnames,1)
      call loadObservCrossSections(fnames(1), 0)
      do ifil=2,size(fnames)
         call loadObservCrossSections(fnames(ifil), 1)
      enddo
      deallocate(fnames)
   end if
   ! Load runup gauge polygons from file
   if (len_trim(md_rugfile) > 0) then
      call strsplit(md_rugfile,1,fnames,1)
      call loadRunupGauges(fnames(1),0)
      do ifil=2,size(fnames)
         call loadRunupGauges(fnames(ifil), 1)
      enddo
      deallocate(fnames)
   endif
   call timstop(timerHandle)

    ! Load partition polygons from file
    if (len_trim(md_partitionfile) > 0 .and. md_japartition == 0) then
       call oldfil(minp, md_partitionfile)
       call reapol(minp, 0)
       call pol_to_tpoly(npartition_pol, partition_pol)
    end if

    call delpol()

    call timstop(handle_loadModel)
end subroutine loadModel


!> Reads a model definition file.
!! Important in loadModel() process.
!! @see writeMDUFile
subroutine readMDUFile(filename, istat)
    use time_module, only : ymd2modified_jul, datetimestring_to_seconds
    use m_flow, notinuse_s=>success
               !,                  only : kmx, layertype, mxlayz, sigmagrowthfactor, iturbulencemodel, &
               !                         LAYTP_SIGMA, numtopsig, spirbeta,                              &
               !                         dztopuniabovez, dztop, uniformhu, jahazlayer, Floorlevtoplay,         &
               !                         javakeps,                                                             &
               !                         fixedweirtopwidth, fixedweirtopfrictcoef, fixedweirtalud, ifxedweirfrictscheme,  &
               !                         Tsigma, jarhoxu,                                                      &
               !                         iStrchType, STRCH_UNIFORM, STRCH_USER, STRCH_EXPONENT, STRCH_FIXLEVEL, laycof

    use m_globalparameters, only : sl
    use m_flowgeom !,              only : wu1Duni, bamin, rrtol, jarenumber, VillemonteCD1, VillemonteCD2
    use m_flowtimes
    use m_flowparameters
    use m_waves,                 only: rouwav, gammax, hminlw, jauorb, jahissigwav, jamapsigwav
    use m_wind ! ,                  only : icdtyp, cdb, wdb,
    use network_data,            only : zkuni, Dcenterinside, removesmalllinkstrsh, cosphiutrsh
    use m_sferic,                only : anglat, anglon, jasfer3D
    use m_physcoef
    use m_alloc
    use m_equatorial
    use m_netw,                  only : Makeorthocenters
    use m_partitioninfo
    use m_fixedweirs
    use m_trachy, only: trtdef_ptr
    use m_reduce,                only : maxdge
    use m_structures
    use m_grw
    use m_sobekdfm,              only : sbkdfm_umin,sbkdfm_umin_method,minimal_1d2d_embankment, sbkdfm_relax
    use string_module
    use m_heatfluxes
    use m_fm_wq_processes
    use m_xbeach_avgoutput
    use unstruc_netcdf, only: UNC_CONV_CFOLD, UNC_CONV_UGRID, unc_set_ncformat, unc_writeopts, UG_WRITE_LATLON, UG_WRITE_NOOPTS, unc_nounlimited, unc_noforcedflush, unc_uuidgen, unc_metadatafile
    use dfm_error
    use MessageHandling
    use system_utils, only: split_filename
    use m_commandline_option, only: iarg_usecaching
    use m_subsidence, only: sdu_update_s1
    use unstruc_channel_flow
    use m_bedform, only: bfm_included

    use m_sediment
    use m_waves, only: hwavuni, twavuni, phiwavuni
    use m_sedtrails_data, only: sedtrails_analysis
    use unstruc_display,  only: jaGUI 

    character(*), intent(in)  :: filename !< Name of file to be read (the MDU file must be in current working directory).
    integer,      intent(out) :: istat    !< Return status (0=success)

    character(len=32):: program
    logical :: success, ex
    character(len=1),dimension(1) :: dummychar
    logical :: dummylog
    character(len=1000) :: charbuf = ' '
    character(len=255) :: tmpstr, fnam, bnam
    double precision, allocatable ::tmpdouble(:)
    integer :: ibuf, ifil, mptfile, warn
    integer :: i, n, j, je, iostat, readerr, ierror
    integer :: jadum
    real(kind=hp) :: ti_rst_array(3), ti_map_array(3), ti_his_array(3), acc, ti_wav_array(3), ti_waq_array(3), ti_classmap_array(3),ti_st_array(3)
    character(len=200), dimension(:), allocatable       :: fnames
    double precision, external     :: densfm
    double precision :: tim
    double precision :: sumlaycof
    double precision, parameter :: tolSumLay = 1d-12
    integer, parameter :: maxLayers = 300
    integer :: major, minor
    external :: unstruc_errorhandler
    istat = 0 ! Success

! Put .mdu file into a property tree
    call tree_create(trim(filename), md_ptr)

    call prop_inifile(filename , md_ptr, readerr)                       ! without preprocessing the mdu-file

!   check if file was successfully opened
    if ( readerr.ne.0 ) then
       istat = -1
       call set_mh_callback(unstruc_errorhandler)
       call mess(LEVEL_ERROR, 'Error opening file ', trim(filename), '.')
       return
    endif

!    call prop_inifile(filename , md_ptr, readerr, japreproc=.true.)     ! with preprocessing
    if (loglevel_StdOut <= LEVEL_DEBUG) then
       call tree_traverse(md_ptr, print_tree, dummychar, dummylog)
    end if

! Early read of OutputDir, such that the .dia fle can be stored there.
    call prop_get_string(md_ptr, 'output', 'OutputDir', md_outputdir, success)
    call switch_dia_file()

! Read FileVersion into major and minor from the [General] block.
! If it fails, then try to read MDUFormatVersion from the [model] block
    call prop_get_version_number(md_ptr, major = major, minor = minor, success = success)
    if (success) then
       bnam = 'General'
       call prop_get_string(md_ptr, bnam, 'fileType', tmpstr, success) ! Not used currently. Mark as read to prevent warnings.
    else
       bnam = 'model'
       tmpstr = ''
       call prop_get(md_ptr, bnam, 'MDUFormatVersion', tmpstr)

       if (len_trim(tmpstr) == 0) then
          tmpstr = '1.00' ! If MDU file has no version number, don't do any checking (such that older pre-versioning models continue to work)
       end if

       read (tmpstr, '(i1,1X,i2)', err=999) major, minor
999    continue
    end if
! Correct file version ?
! Note: older MDUs are usually supported (backwards compatible).
! Only if major version d.xx nr of MDU file is other than current, show an error.
! Check for equality of major version number
    if (major /= MDUFormatMajorVersion) then
       write (msgbuf, '(a,i0,".",i2.2,a,i0,".",i2.2,a)') 'Unsupported MDU format detected: v', major, minor, '. Current format: v', MDUFormatMajorVersion, MDUFormatMinorVersion, '. Please review your input.'
       call qnerror(trim(msgbuf), ' ',' ')
       call err_flush()
       istat = DFM_EFILEFORMAT
       istat = DFM_NOERR ! For now: error was shown to user, and below try reading anyway.
       ! TODO: fix all mdu's in modeldatabase before quiting.
       ! return
    end if
! Note: in future, version checking may be done on a per-key basis below


    program = ''
    call prop_get_string(md_ptr, bnam, 'Program', program, success)
! Correct program ?
    if (.not. success .or. (program /= 'Unstruc' .and. program /= 'UNSTRUC' .and. program /= 'D-Flow FM' .and. program /= 'D-Flow Flexible Mesh')) then
        istat = -1
        call mess(LEVEL_ERROR, 'Wrong model definition file. ', trim(program), ' should be ''D-Flow FM''.')
        return
    end if
    call prop_get_string(md_ptr, bnam, 'Version', tmpstr, success) ! Not used currently. Mark as read to prevent warnings.

    call prop_get_integer(md_ptr, bnam, 'AutoStart',  md_jaAutoStart)
    call prop_get_integer(md_ptr, bnam, 'InputSpecific',  md_input_specific)
    call prop_get_string(md_ptr,  bnam, 'ModelSpecific',  md_specific)
    call prop_get_integer(md_ptr, bnam, 'PathsRelativeToParent',  md_paths_relto_parent)

! Geometry
    call prop_get_string ( md_ptr, 'geometry', 'CrossDefFile',     md_1dfiles%cross_section_definitions, success)
    call prop_get_string ( md_ptr, 'geometry', 'CrossLocFile',     md_1dfiles%cross_section_locations,   success)
    call prop_get_string ( md_ptr, 'geometry', 'StorageNodeFile',  md_1dfiles%storage_nodes,             success)
    call prop_get_string ( md_ptr, 'geometry', 'frictFile',        md_1dfiles%roughness,                 success)
    call prop_get_string ( md_ptr, 'geometry', 'StructureFile',    md_1dfiles%structures,                success)       ! pending code merge, structure file either applies to v2.00 structure file, or the old one, so store in both
    md_1dfiles%roughnessdir = ' '
    call prop_get_string ( md_ptr, 'geometry', 'NetFile',          md_netfile,      success)
    call prop_get_string ( md_ptr, 'geometry', 'GridEnclosureFile',md_encfile,      success)
    call prop_get_string ( md_ptr, 'geometry', 'DryPointsFile',    md_dryptsfile,   success)
    call prop_get_string ( md_ptr, 'geometry', 'WaterLevIniFile',  md_s1inifile,    success)
    call prop_get_string ( md_ptr, 'geometry', 'LandBoundaryFile', md_ldbfile,      success)
    call prop_get_string ( md_ptr, 'geometry', 'ThinDamFile' ,     md_thdfile,      success)
    call prop_get_string ( md_ptr, 'geometry', 'Cutcelllist' ,     md_cutcelllist,  success)
    call prop_get_string ( md_ptr, 'geometry', 'IniFieldFile',     md_inifieldfile, success)

    call prop_get_integer( md_ptr, 'geometry', 'UseCaching',       md_usecaching,   success)
   ! Merge cmd line switches with mdu file settings
    if ( iarg_usecaching /= -1 ) then
        md_usecaching = iarg_usecaching
    endif

    call prop_get_string ( md_ptr, 'geometry', 'FixedWeirFile',    md_fixedweirfile, success)
    call prop_get_string ( md_ptr, 'geometry', 'PillarFile',       md_pillarfile,   success)
    if ( len_trim(md_pillarfile) > 0 ) then
       japillar = 3
    endif
    call prop_get_string ( md_ptr, 'geometry', 'GulliesFile',      md_gulliesfile,   success)
    call prop_get_string ( md_ptr, 'geometry', 'RoofsFile',        md_roofsfile,     success)

    call prop_get_string ( md_ptr, 'geometry', 'VertplizFile',     md_vertplizfile, success)

    call prop_get_string ( md_ptr, 'geometry', 'ProflocFile' ,     md_proflocfile    , success)
    call prop_get_string ( md_ptr, 'geometry', 'ProfdefFile' ,     md_profdeffile    , success)
    call prop_get_string ( md_ptr, 'geometry', 'ProfdefxyzFile' ,  md_profdefxyzfile , success)

    call prop_get_double ( md_ptr, 'geometry', 'Uniformwidth1D'  , wu1Duni           , success)
    call prop_get_double ( md_ptr, 'geometry', 'Uniformheight1D' , hh1Duni           , success)
    call prop_get_integer( md_ptr, 'geometry', 'Uniformtyp1D'    , iproftypuni       , success)

    call prop_get_double ( md_ptr, 'geometry', 'Uniformwidth1Dstreetinlets'     , wu1Duni5          , success)
    call prop_get_double ( md_ptr, 'geometry', 'Uniformheight1Dstreetinlets'    , hh1Duni5          , success)
    call prop_get_integer( md_ptr, 'geometry', 'Uniformtyp1Dstreetinlets'       , iproftypuni5      , success)

    call prop_get_double ( md_ptr, 'geometry', 'Uniformwidth1Droofgutterpipes'  , wu1Duni7          , success)
    call prop_get_double ( md_ptr, 'geometry', 'Uniformheight1roofgutterpipes'  , hh1Duni7          , success)
    call prop_get_integer( md_ptr, 'geometry', 'Uniformtyp1Droofgutterpipes'  , iproftypuni7      , success)
    call prop_get_string ( md_ptr, 'geometry', '1D2DLinkFile' ,     md_1d2dlinkfile , success)

    call prop_get_double ( md_ptr, 'geometry', 'Dxmin1D'       , Dxmin1D)
    call prop_get_double ( md_ptr, 'geometry', 'Dxwuimin2D'    , Dxwuimin2D)

    call prop_get_integer( md_ptr, 'geometry', '1D2Dinternallinktype' , ja1D2Dinternallinktype)


    call prop_get_string ( md_ptr, 'geometry', 'PipeFile'    ,     md_pipefile    , success)
    call prop_get_string ( md_ptr, 'geometry', 'ShipdefFile' ,     md_shipdeffile , success)

    call prop_get_double ( md_ptr, 'geometry', 'Removesmalllinkstrsh' , removesmalllinkstrsh)
    call prop_get_double ( md_ptr, 'geometry', 'Cosphiutrsh' , Cosphiutrsh)

    call prop_get_double ( md_ptr, 'geometry', 'WaterLevIni' , sini)
    call prop_get_double ( md_ptr, 'geometry', 'WaterdepthIni1D' , WaterdepthIni1D)
    call prop_get_double ( md_ptr, 'geometry', 'Uniformhu'   , uniformhu)

    call prop_get_double ( md_ptr, 'geometry', 'BedlevUni'   ,   zkuni)
    call prop_get_double (md_ptr,  'geometry', 'Bedslope'    , bedslope)
    call prop_get_double (md_ptr,  'geometry', 'Bedwaveamplitude' , bedwaveamplitude)
    call prop_get_double (md_ptr,  'geometry', 'Bedwavelength'    , bedwavelength   )

    call prop_get_integer( md_ptr, 'geometry', 'BedlevMode'  , ibedlevmode)

    call prop_get_integer( md_ptr, 'geometry', 'BedlevType'  , ibedlevtyp)
    call prop_get_double ( md_ptr, 'geometry', 'Blmeanbelow' , blmeanbelow)
    call prop_get_double ( md_ptr, 'geometry', 'Blminabove'  , blminabove)
    call prop_get_double ( md_ptr, 'geometry', 'Groundlayerthickness'  , grounLayuni)

    call prop_get_double ( md_ptr, 'geometry', 'AngLat'      ,      anglat)
    call prop_get_double ( md_ptr, 'geometry', 'AngLon'      ,      anglon)

    call prop_get_integer( md_ptr, 'geometry', 'Helmert'     , jaHelmert)

    call prop_get_integer( md_ptr, 'geometry', 'Conveyance2D', Jaconveyance2D)
    call prop_get_integer( md_ptr, 'geometry', 'Conveyance3D', Jaconveyance3D)
    call prop_get_integer( md_ptr, 'geometry', 'Nonlin2D'    , Nonlin2D)
    call prop_get_integer( md_ptr, 'geometry', 'Nonlin1D'    , Nonlin1D)
    call prop_get_double ( md_ptr, 'geometry', 'Slotw2D'     , slotw2D)
    call prop_get_double ( md_ptr, 'geometry', 'Slotw1D'     , slotw1D)
    ! use slotw1d also in getcspars routines
    sl = slotw1D

    call prop_get_double ( md_ptr, 'geometry', 'Sillheightmin', sillheightmin)

    kmx = 0
    call prop_get_integer( md_ptr, 'geometry', 'Kmx'             , kmx)
    if (kmx>0) then
       call prop_get_integer( md_ptr, 'geometry', 'Layertype'       , Layertype)
       if (Layertype /= LAYTP_SIGMA) then
           mxlayz = kmx
       endif
       
       call prop_get_integer( md_ptr, 'geometry', 'Numtopsig'        , Numtopsig)
       call prop_get_integer( md_ptr, 'geometry', 'Numtopsiguniform' , JaNumtopsiguniform)
       call prop_get_double ( md_ptr, 'geometry', 'SigmaGrowthFactor', sigmagrowthfactor)
       call prop_get_double ( md_ptr, 'geometry', 'Dztopuniabovez'   , dztopuniabovez )
       call prop_get_double ( md_ptr, 'geometry', 'Dztop'            , Dztop )
       call prop_get_double ( md_ptr, 'geometry', 'Toplayminthick'   , Toplayminthick)
       call prop_get_double ( md_ptr, 'geometry', 'Floorlevtoplay'   , Floorlevtoplay )
       call prop_get_integer( md_ptr, 'geometry', 'OrgFloorlevtoplaydef'   , jaorgFloorlevtoplaydef )
       call prop_get_double ( md_ptr, 'geometry', 'Tsigma'           , Tsigma )
       call prop_get_double ( md_ptr, 'geometry', 'ZlayBot'          , zlaybot )
       call prop_get_double ( md_ptr, 'geometry', 'ZlayTop'          , zlaytop )
       call prop_get_integer( md_ptr, 'geometry', 'StretchType'      , iStrchType)
       
       if (Dztop > 0d0) then  ! hk claims back original functionality
           iStrchType = -1
       endif

       if( iStrchType == STRCH_USER ) then
          call realloc(laycof, kmx)
          call prop_get_doubles( md_ptr, 'geometry', 'StretchCoef', laycof, kmx )
          sumlaycof = sum(laycof)
          if (comparereal(sumlaycof, 100d0, tolSumLay) /= 0) then
             call realloc(tmpdouble, maxLayers, fill=0d0)
             call prop_get_doubles( md_ptr, 'geometry', 'StretchCoef', tmpdouble, maxLayers )
             n = 0
             do i = 1, maxLayers
                if (.not.(tmpdouble(i) > 0)) then
                   exit
                endif
                n = n + 1
             end do
             if ( kmx /= n ) then
                call mess(LEVEL_ERROR, 'The number of values specified in "StretchCoef" is inconsistent with "Kmx"!')
             else
                call mess(LEVEL_ERROR, 'The values specified in "StretchCoef" do not add up to 100! We got: ', sumlaycof)
             endif
          endif
       else if( iStrchType == STRCH_EXPONENT ) then
          call realloc(laycof, 3)
          call prop_get_doubles( md_ptr, 'geometry', 'StretchCoef', laycof, 3)
       endif

       call prop_get_integer(md_ptr, 'geometry', 'Keepzlayeringatbed'      , keepzlayeringatbed , success)
       if (.not. success) then
          call prop_get_integer(md_ptr, 'numerics', 'Keepzlayeringatbed', keepzlayeringatbed)
       endif
       call prop_get_integer(md_ptr, 'geometry', 'Ihuz'           , ihuz             , success)
       call prop_get_integer(md_ptr, 'geometry', 'Ihuzcsig'       , ihuzcsig         , success)
       call prop_get_integer(md_ptr, 'geometry', 'Keepzlay1bedvol', keepzlay1bedvol  , success)
       call prop_get_integer(md_ptr, 'geometry', 'Zlayeratubybob' , jaZlayeratubybob , success)
    endif
    call prop_get_integer( md_ptr, 'geometry', 'Makeorthocenters' , Makeorthocenters)
    call prop_get_double ( md_ptr, 'geometry', 'Dcenterinside'    , Dcenterinside)
    call prop_get_string ( md_ptr, 'geometry', 'PartitionFile'    , md_partitionfile, success)
    if (jampi .eq. 1 .and. md_japartition .ne. 1) then
       if (len_trim(md_partitionfile) < 1) then
          call mess(LEVEL_INFO, 'no partitioning polygons file: read subdomain numbering from netfiles')
!          md_genpolygon = 0
!          write(*,*) 'Warning: readMDUFile: [partitionfile] is empty,'
!          write(*,*) 'hence, read subdomain info. from partition files.'
       else
          call mess(LEVEL_INFO, 'generate subdomain numbering from ' // trim(md_partitionfile))
!          md_genpolygon = 1
!          write(*,*) 'Warning: readMDUFile: [partitionfile] is specified,'
!          write(*,*) 'hence, use polygon to obtain subdomain info.'
       endif
    endif
    call prop_get_double ( md_ptr, 'geometry', 'Bamin'   ,  Bamin )
    call prop_get_double ( md_ptr, 'geometry', 'OpenBoundaryTolerance',  rrtol)
    call prop_get_integer( md_ptr, 'geometry', 'AllowBndAtBifurcation',  jaAllowBndAtBifurcation)
    call prop_get_integer( md_ptr, 'geometry', 'CreateLinks1D2D',  md_jamake1d2dlinks)
    call prop_get_integer( md_ptr, 'geometry', 'RenumberFlowNodes',  jarenumber) ! hidden option for testing renumbering
    call prop_get_integer( md_ptr, 'geometry', 'Autotrisam',  autotrisam) ! hidden option for auto generation of networks from samples or not
    call prop_get_logical( md_ptr, 'geometry', 'dxDoubleAt1DEndNodes', dxDoubleAt1DEndNodes)
    call prop_get_logical( md_ptr, 'geometry', 'ChangeVelocityAtStructures', changeVelocityAtStructures)

    call prop_get_logical( md_ptr, 'geometry', 'ChangeStructureDimensions', changeStructureDimensions)

    ! 1D Volume tables
    useVolumeTables = .false.
    call prop_get_logical( md_ptr, 'volumeTables', 'useVolumeTables', useVolumeTables)
    call prop_get_double  (md_ptr, 'volumeTables', 'increment', tableIncrement)
    useVolumeTableFile = .false.
    call prop_get_logical(md_ptr, 'volumeTables', 'useVolumeTableFile', useVolumeTableFile)

    ! Numerics
    call prop_get_double( md_ptr, 'numerics', 'CFLMax'          , cflmx)
    call prop_get_double( md_ptr, 'numerics', 'EpsMaxlev'       , epsmaxlev)
    call prop_get_double( md_ptr, 'numerics', 'EpsMaxlevm'      , epsmaxlevm)
    !call prop_get_double( md_ptr, 'numerics', 'CFLWaveFrac'     , cflw)
    call prop_get_integer(md_ptr, 'numerics', 'AdvecType'       , iadvec)
    if (Layertype > 1) then
       iadvec = 33 ; iadvec1D = 33
    endif
    call prop_get_integer(md_ptr, 'numerics', 'AdvecCorrection1D2D', iadveccorr1D2D)
    call prop_get_integer(md_ptr, 'numerics', 'TimeStepType'    , itstep)

    call prop_get_integer(md_ptr, 'numerics', 'maxNonlinearIterations'    , maxNonlinearIterations)
    call prop_get_logical(md_ptr, 'numerics', 'setHorizontalBobsFor1d2d', setHorizontalBobsFor1d2d)
    
    call prop_get_integer(md_ptr, 'numerics', 'Testdryingflooding', testdryflood)
    call prop_get_integer(md_ptr, 'numerics', 'Testfixedweirs'    , testfixedweirs)

    call prop_get_integer(md_ptr, 'numerics', 'Icoriolistype'   , icorio)
    call prop_get_integer(md_ptr, 'numerics', 'Newcorio'        , newcorio)

    call prop_get_integer(md_ptr, 'numerics', 'Corioconstant'   , jacorioconstant)
    call prop_get_double (md_ptr, 'numerics', 'Corioadamsbashfordfac', Corioadamsbashfordfac)
    call prop_get_double (md_ptr, 'numerics', 'Coriohhtrsh'          , hhtrshcor)

    call prop_get_integer(md_ptr, 'numerics', 'Limtyphu'        , limtyphu)
    call prop_get_integer(md_ptr, 'numerics', 'Limtypmom'       , limtypmom)
    call prop_get_integer(md_ptr, 'numerics', 'Limtypsa'        , limtypsa)
    call prop_get_integer(md_ptr, 'numerics', 'Limtypw'         , limtypw)
    if (kmx > 1) then ! package deal
       ja_timestep_auto = 5
    endif

    call prop_get_integer(md_ptr, 'numerics', 'TransportAutoTimestepdiff' , jatransportautotimestepdiff)

    if (jatransportautotimestepdiff == 3 .and. kmx > 0) then
       call mess(LEVEL_ERROR, 'Implicit horizontaldiffusion is only implemented in 2D', 'set TransportAutoTimestepdiff = 0, 1 or 2')
    endif

    call prop_get_integer(md_ptr, 'numerics', 'Implicitdiffusion2D' , Implicitdiffusion2D)
    if (Implicitdiffusion2D == 1) then
       if (kmx > 0) then
          call mess(LEVEL_ERROR, 'Implicit horizontaldiffusion is only implemented in 2D',' ')
       else
          jatransportautotimestepdiff = 3
       endif
    endif

    call prop_get_integer(md_ptr, 'numerics', 'DiagnosticTransport' , jadiagnostictransport)
    
    call prop_get_integer(md_ptr, 'numerics', 'Vertadvtypsal'       , javasal)
    call prop_get_integer(md_ptr, 'numerics', 'Vertadvtyptem'       , javatem)
    call prop_get_integer(md_ptr, 'numerics', 'Vertadvtypmom'       , javau)
    call prop_get_integer(md_ptr, 'numerics', 'Vertadvtypmom3onbnd' , javau3onbnd)

    call prop_get_double (md_ptr, 'numerics', 'Cffacver'        , Cffacver)
    call prop_get_double (md_ptr, 'numerics', 'Cffachormom'     , Cffachormom)
    call prop_get_double (md_ptr, 'numerics', 'Cfexphormom'     , Cfexphormom)
    call prop_get_double (md_ptr, 'numerics', 'Cfconhormom'     , Cfconhormom)
    call prop_get_double (md_ptr, 'numerics', 'Cffachu'         , Cffachu)
    call prop_get_double (md_ptr, 'numerics', 'Cfexphu'         , Cfexphu)


    call prop_get_integer(md_ptr, 'numerics', 'Horadvtypzlayer' , jahazlayer)

    call prop_get_integer(md_ptr, 'numerics', 'Pure1D'          , jaPure1D)
    call prop_get_integer(md_ptr, 'numerics', 'Junction1D'      , jaJunction1D)

    call prop_get_integer(md_ptr, 'numerics', 'Zlayercenterbedvel', jaZlayercenterbedvel)

    call prop_get_integer(md_ptr, 'numerics', 'Structurelayersactive', jastructurelayersactive)

    call prop_get_integer(md_ptr, 'numerics', 'Zerozbndinflowadvection', jaZerozbndinflowadvection)

    call prop_get_integer(md_ptr, 'numerics', 'Jarhoxu' , Jarhoxu)
    if (Jarhoxu > 0) then
        call mess(LEVEL_INFO, 'WARNING: Jarhoxu=0 is strongly advised and other values are discouraged', '.')
    end if

    call prop_get_integer(md_ptr, 'numerics', 'Icgsolver'       , Icgsolver)
    call prop_get_integer(md_ptr, 'numerics', 'Maxdegree'       , Maxdge)
    ! call prop_get_integer(md_ptr, 'numerics', 'Noderivedtypes'  , Noderivedtypes)
    if (icgsolver == 7 .or. icgsolver == 6) then
       Noderivedtypes = min(Noderivedtypes, 4) ! no deallocation of derived types
    endif
    call prop_get_integer(md_ptr, 'numerics', 'jposhchk'       , jposhchk)
    call prop_get_integer(md_ptr, 'numerics', 'FixedWeirScheme'  , ifixedweirscheme, success)
    call prop_get_double( md_ptr, 'numerics', 'FixedWeirContraction' , Fixedweircontraction, success)

    call prop_get_integer(md_ptr, 'numerics', 'Fixedweirfrictscheme'  , ifxedweirfrictscheme)
    call prop_get_double( md_ptr, 'numerics', 'Fixedweirtopwidth'     , fixedweirtopwidth)
    call prop_get_double( md_ptr, 'numerics', 'Fixedweirtopfrictcoef' , fixedweirtopfrictcoef)
    call prop_get_double( md_ptr, 'numerics', 'Fixedweirtalud'        , fixedweirtalud)
    call prop_get_double (md_ptr, 'numerics', 'FixedweirRelaxationcoef' , waquaweirthetaw)

    call prop_get_integer(md_ptr, 'numerics', 'Izbndpos'          , Izbndpos)
    call prop_get_double (md_ptr, 'numerics', 'Tlfsmo'            , Tlfsmo)
    call prop_get_integer(md_ptr, 'numerics', 'Keepstbndonoutflow', keepstbndonoutflow )
    call prop_get_integer(md_ptr, 'numerics', 'Diffusiononbnd'    , jadiffusiononbnd )
    call prop_get_double (md_ptr, 'numerics', 'Tspinupturblogprof', Tspinupturblogprof )
    call prop_get_integer(md_ptr, 'numerics', 'Logprofatubndin'   , jaLogprofatubndin  )
    call prop_get_integer(md_ptr, 'numerics', 'Logprofkepsbndin'  , jaLogprofkepsbndin )

    call prop_get_double( md_ptr, 'numerics', 'Slopedrop2D'  , Slopedrop2D)
    call prop_get_logical(md_ptr, 'numerics', 'Drop1D'       , Drop1D)
    call prop_get_double( md_ptr, 'numerics', 'Drop3D'       , Drop3D)
    call prop_get_integer(md_ptr, 'numerics', 'Lincontin'    , lincontin)
    call prop_get_double (md_ptr, 'numerics', 'Chkadvd'      , chkadvd)

    call prop_get_integer(md_ptr, 'numerics', 'Linkdriedmx'  , Linkdriedmx)
    call prop_get_double (md_ptr, 'numerics', 'Huweirregular', Huweirregular) 

    call prop_get_double (md_ptr, 'numerics', 'Chkdifd'      , chkdifd)
    call prop_get_double (md_ptr, 'numerics', 'Zwsbtol'      , zwsbtol)
    call prop_get_double (md_ptr, 'numerics', 'Trsh_u1Lb'    , Trsh_u1Lb)
    call prop_get_double (md_ptr, 'numerics', 'Epshstem'     , epshstem)

    call prop_get_double (md_ptr, 'numerics', 'Teta0'        , teta0)
    call prop_get_integer(md_ptr, 'numerics', 'Jbasqbnddownwindhs'    , jbasqbnddownwindhs)

    call prop_get_integer(md_ptr, 'numerics', 'Maxitverticalforestersal' , Maxitverticalforestersal)
    call prop_get_integer(md_ptr, 'numerics', 'cstbnd'       , jacstbnd)
    call prop_get_integer(md_ptr, 'numerics', 'Maxitverticalforestertem' , Maxitverticalforestertem)
    call prop_get_integer(md_ptr, 'numerics', 'Turbulencemodel' , Iturbulencemodel)
    call prop_get_integer(md_ptr, 'numerics', 'Turbulenceadvection' , javakeps)
    call prop_get_integer(md_ptr, 'numerics', 'Jadrhodz' , jadrhodz)
    call prop_get_double (md_ptr, 'numerics', 'Eddyviscositybedfacmax' , Eddyviscositybedfacmax)
    call prop_get_integer(md_ptr, 'numerics', 'AntiCreep' , jacreep)

   
    call prop_get_integer(md_ptr, 'numerics', 'Orgbarockeywords' , jaorgbarockeywords)
    if (jaorgbarockeywords == 1) then
       call prop_get_integer(md_ptr, 'numerics', 'Barocterm' , jabarocterm)
       call prop_get_integer(md_ptr, 'numerics', 'Baroctimeint' , jabaroctimeint)
    endif
    call prop_get_integer(md_ptr, 'numerics', 'Baroczlaybed'   , jabaroczlaybed)
    call prop_get_integer(md_ptr, 'numerics', 'Barocponbnd'    , jaBarocponbnd)
    call prop_get_integer(md_ptr, 'numerics', 'Maxitpresdens'  , maxitpresdens)
    call prop_get_integer(md_ptr, 'numerics', 'Rhointerfaces'  , jarhointerfaces)
 
    call prop_get_integer(md_ptr, 'numerics', 'EnableJRE', jajre)

    if ( icgsolver.eq.8 ) then   ! for parms solver
       do i=1,NPARMS_INT
          call prop_get_integer(md_ptr, 'numerics', trim(iparmsnam(i)), iparms(i))
       end do

       do i=1,NPARMS_DBL
          call prop_get_double( md_ptr, 'numerics', trim(dparmsnam(i)), dparms(i))
       end do
    end if

    call prop_get_double(md_ptr, 'numerics', 'Maxwaterleveldiff', s01max)
    call prop_get_double(md_ptr, 'numerics', 'Maxvelocitydiff', u01max)
    call prop_get_double(md_ptr, 'numerics', 'Maxvelocity', umagmax)
    call prop_get_double(md_ptr, 'numerics', 'Waterlevelwarn', s01warn)
    call prop_get_double(md_ptr, 'numerics', 'Velocitywarn', u01warn)
    call prop_get_double(md_ptr, 'numerics', 'Velmagnwarn', umagwarn)
    call prop_get_double(md_ptr, 'numerics', 'MinTimestepBreak', dtminbreak)
    call prop_get_double(md_ptr, 'numerics', 'MaxSSC', sscmax)
    call prop_get_double(md_ptr, 'numerics', 'Epshu' , epshu)
    call prop_get_double(md_ptr, 'numerics', 'Epsz0' , epsz0)
    epshs = .2d0*epshu ! minimum waterdepth for setting cfu

    call prop_get_double(md_ptr, 'numerics', 'SobekDFM_umin', sbkdfm_umin)
    call prop_get_integer(md_ptr, 'numerics', 'SobekDFM_umin_method', sbkdfm_umin_method)
    call prop_get_double (md_ptr, 'numerics', 'SobekDFM_Minimal_1d2d_Embankment', minimal_1d2d_embankment)
    call prop_get_double (md_ptr, 'numerics', 'sobekDFM_relax', sbkdfm_relax)

    call prop_get_integer(md_ptr, 'numerics', 'jaupwindsrc', jaupwindsrc)

    call prop_get_integer(md_ptr, 'numerics', 'jasfer3D', jasfer3D, success)
    if ( success .and. jasfer3D == 1 ) then
       jalimnor = 1
    end if

    call prop_get_integer(md_ptr, 'numerics', 'BarrierAdvection', jabarrieradvection);

    call prop_get_integer(md_ptr, 'numerics', 'HorizontalMomentumFilter', jafilter)
    !call prop_get_integer(md_ptr, 'numerics', 'filter'          , jafilter)
    !call prop_get_integer(md_ptr, 'numerics', 'filterorder'     , filterorder)
    call prop_get_integer(md_ptr, 'numerics', 'checkerboardmonitor', jacheckmonitor)
    !
    ! Filter to suppress checkerboarding is also available for z-layers (so that ERROR message has been switched off)
    !
    ! if (Layertype == 2 .and. jafilter /= 0) then
      ! call mess(LEVEL_ERROR, 'The checkerboard-filter has not been implemented for Z-models yet', '.')
    ! endif
    !
    ! Filter to suppress checkerboarding is also available for z-layers (so that ERROR message has been switched off)
    !
    ! if (Layertype == 2 .and. jacheckmonitor /= 0) then
      ! call mess(LEVEL_WARN, 'The checkerboardmonitor has not been implemented for Z-models yet, is automatically switched off now.')
      ! jacheckmonitor = 0
    ! end if

    call prop_get_double (md_ptr, 'numerics', 'LocSaltLev'      , locsaltlev)
    call prop_get_double (md_ptr, 'numerics', 'LocSaltMin'      , locsaltmin)
    call prop_get_double (md_ptr, 'numerics', 'LocSaltMax'      , locsaltmax)

    call prop_get_integer(md_ptr, 'numerics', 'Numlimdt_baorg'  , Numlimdt_baorg)
    call prop_get_double (md_ptr, 'numerics', 'Baorgfracmin'    , Baorgfracmin)

    call prop_get_integer(md_ptr, 'numerics', 'LogSolverConvergence', jalogsolverconvergence)
    call prop_get_integer(md_ptr, 'numerics', 'LogTransportSolverLimiting', jalogtransportsolverlimiting)
    call prop_get_integer(md_ptr, 'numerics', 'SubsUplUpdateS1', sdu_update_s1)
    if (sdu_update_s1<0 .or. sdu_update_s1>1) then
       call mess(LEVEL_WARN, 'Invalid settings specified for SubsUplUpdateS1; using 0.')
       sdu_update_s1 = 0
    endif
    
    call prop_get_integer(md_ptr, 'numerics', 'PerotType', iperot)

    call prop_get(md_ptr, 'numerics', 'Oceaneddysizefrac'   , Oceaneddysizefrac)
    call prop_get(md_ptr, 'numerics', 'Oceaneddysize'       , Oceaneddysize)
    call prop_get(md_ptr, 'numerics', 'Oceaneddyamp'        , Oceaneddyamp)
    call prop_get(md_ptr, 'numerics', 'Oceaneddyvel'        , Oceaneddyvel)
    call prop_get(md_ptr, 'numerics', 'Oceaneddyyoff'       , Oceaneddyyoff)
    call prop_get(md_ptr, 'numerics', 'Oceaneddyxoff'       , Oceaneddyxoff)

    ! Physics
    call prop_get_double (md_ptr, 'physics', 'UnifFrictCoef'  , frcuni)
    call prop_get_integer(md_ptr, 'physics', 'UnifFrictType'  , ifrctypuni)
    call prop_get_double (md_ptr, 'physics', 'UnifFrictCoef1D', frcuni1D) ! TODO: LUMBRICUS: HK: ook UnifFrictType1D? EN/OF prof1d type --> frcutp(LF) zetten?
    call prop_get_double (md_ptr, 'physics', 'UnifFrictCoef1D2D', frcuni1D2D)
    call prop_get_double (md_ptr, 'physics', 'UnifFrictCoefLin' , frcunilin)
    call prop_get_double (md_ptr, 'physics', 'UnifFrictCoef1DgrLay', frcuni1Dgrounlay)

    if (frcunilin > 0) then
       jafrculin = 1
    end if

    call prop_get_double (md_ptr, 'physics', 'Umodlin'        , umodlin)
    call prop_get_double (md_ptr, 'physics', 'Vicouv'         , vicouv)
    call prop_get_double (md_ptr, 'physics', 'Dicouv'         , dicouv)
    call prop_get_double (md_ptr, 'physics', 'Vicoww'         , vicoww)
    call prop_get_double (md_ptr, 'physics', 'Dicoww'         , dicoww)
    call prop_get_double (md_ptr, 'physics', 'Vicwminb'       , Vicwminb)
    call prop_get_double (md_ptr, 'physics', 'Xlozmidov'      , Xlozmidov)


    call prop_get_double (md_ptr, 'physics', 'Smagorinsky'    , Smagorinsky)
    call prop_get_double (md_ptr, 'physics', 'Elder   '       , Elder)
    call prop_get_integer(md_ptr, 'physics', 'irov'           , irov)
    call prop_get_double (md_ptr, 'physics', 'wall_ks'        , wall_ks)
    wall_z0 = wall_ks/30d0
    call prop_get_integer(md_ptr, 'physics', 'TidalForcing'   , jatidep)
    call prop_get_integer(md_ptr, 'physics', 'SelfAttractionLoading', jaselfal)
    call prop_get_integer(md_ptr, 'physics', 'SelfAttractionLoading_correct_wl_with_ini',  jaSELFALcorrectWLwithIni)
    call prop_get_double (md_ptr, 'physics', 'ITcap'          , ITcap)
    call prop_get_double (md_ptr, 'physics', 'Doodsonstart'   , Doodsonstart)
    call prop_get_double (md_ptr, 'physics', 'Doodsonstop'    , Doodsonstop)
    call prop_get_double (md_ptr, 'physics', 'Doodsoneps'     , Doodsoneps)

    call prop_get_double (md_ptr, 'physics', 'VillemonteCD1'  , VillemonteCD1)
    call prop_get_double (md_ptr, 'physics', 'VillemonteCD2'  , VillemonteCD2)

    call prop_get_double (md_ptr, 'physics', 'Rhomean'        , rhomean)
    call prop_get_double (md_ptr, 'physics', 'Ag'             , ag) ; sag = sqrt(ag)

    call prop_get_integer(md_ptr, 'physics', 'Salinity'       , jasal)
    call prop_get_double (md_ptr, 'physics', 'InitialSalinity', salini)
    call prop_get_double (md_ptr, 'physics', 'DeltaSalinity'  , deltasalinity)

    call prop_get_double (md_ptr, 'physics', 'Sal0abovezlev'  , Sal0abovezlev)
!    Secondary Flow
    call prop_get_integer(md_ptr, 'physics', 'SecondaryFlow'  , jasecflow)
    call prop_get_double (md_ptr, 'physics', 'BetaSpiral'     , spirbeta )
    call prop_get_integer(md_ptr, 'physics', 'Equili'         , jaequili ) ! TODO: Ottevanger/Nabi: consider changing the name of these settings: add "spiral/secondary flow" into it.

    call prop_get_integer(md_ptr, 'physics', 'Idensform'      , idensform)

    !call prop_get_integer(md_ptr, 'physics', 'Baroczlaybed'   , jabaroczlaybed)
    !call prop_get_integer(md_ptr, 'physics', 'Barocponbnd'    , jaBarocponbnd)
    !call prop_get_integer(md_ptr, 'physics', 'Maxitpresdens'  , maxitpresdens)
    !call prop_get_integer(md_ptr, 'physics', 'Rhointerfaces'  , jarhointerfaces)
   
    call prop_get_integer(md_ptr, 'physics', 'Temperature'       , jatem)
    call prop_get_double (md_ptr, 'physics', 'InitialTemperature', temini)
    call prop_get_double (md_ptr, 'physics', 'Secchidepth'       , Secchidepth)
    call prop_get_double (md_ptr, 'physics', 'Secchidepth2'      , Secchidepth2)
    call prop_get_double (md_ptr, 'physics', 'Secchidepth2fraction'  , Secchidepth2fraction)
    zab(1) = Secchidepth / 1.7d0 ; sfr (1) = 1d0
    if (Secchidepth2 > 0) then
       zab(2) = Secchidepth2 / 1.7d0 ; sfr(2) = Secchidepth2fraction ; sfr(1) = 1d0 - sfr(2)
    endif

    call prop_get_double (md_ptr, 'physics', 'Stanton'           , Stanton)
    call prop_get_double (md_ptr, 'physics', 'Dalton'            , Dalton)
    call prop_get_double (md_ptr, 'physics', 'Tempmax'           , Tempmax)
    call prop_get_double (md_ptr, 'physics', 'Tempmin'           , Tempmin)
    call prop_get_integer(md_ptr, 'physics', 'Allowcoolingbelowzero'  , Jaallowcoolingbelowzero)
  
    call prop_get_double (md_ptr, 'physics', 'Salimax'           , Salimax)
    call prop_get_double (md_ptr, 'physics', 'Salimin'           , Salimin)
    call prop_get_double (md_ptr, 'physics', 'Surftempsmofac'    , Surftempsmofac)
    call prop_get_integer(md_ptr, 'physics', 'RhoairRhowater'    , jaroro)
    call prop_get_integer(md_ptr, 'physics', 'Heat_eachstep'     , jaheat_eachstep)
    call prop_get_double (md_ptr, 'physics', 'Soiltempthick'     , Soiltempthick)
    if (soiltempthick > 0d0) then
       jaheat_eachstep = 1
    endif

    call prop_get_double(md_ptr, 'physics', 'Nudgetimeuni', Tnudgeuni)
    call prop_get_integer(md_ptr, 'physics', 'IniWithNudge', jainiwithnudge)

    call prop_get_integer(md_ptr, 'physics', 'Jadelvappos'       , Jadelvappos)

    call prop_get_double (md_ptr, 'physics', 'Backgroundsalinity', Backgroundsalinity)
    call prop_get_double (md_ptr, 'physics', 'Backgroundwatertemperature', Backgroundwatertemperature)
    ! Set molecular viscosity
    vismol = 4.d0/(20.d0 + backgroundwatertemperature)*1d-5 ! Van Rijn, 1993, from iniphys.f90

    call prop_get_integer(md_ptr, 'veg',     'Vegetationmodelnr', javeg) ! Vegetation model nr, (0=no, 1=Baptist DFM)
    if( japillar == 2 ) then
       javeg = 1
    endif
    if (kmx == 0 .and. javeg > 0) then
       jabaptist = javeg
    endif
    call prop_get_double (md_ptr, 'veg'    , 'Clveg'          , Clveg)
    call prop_get_double (md_ptr, 'veg'    , 'Cdveg'          , Cdveg)
    call prop_get_integer(md_ptr, 'veg'    , 'Cdvegspatial'   , JaCdvegsp)
    call prop_get_double (md_ptr, 'veg'    , 'Cbveg'          , Cbveg)
    call prop_get_double (md_ptr, 'veg'    , 'Rhoveg'         , Rhoveg)
    call prop_get_double (md_ptr, 'veg'    , 'Stemheightstd'  , Stemheightstd)
    call prop_get_double (md_ptr, 'veg'    , 'Densvegminbap'  , Densvegminbap)

    call prop_get_double (md_ptr, 'veg'    , 'Expchistem'     , expchistem)
    call prop_get_double (md_ptr, 'veg'    , 'Expchileaf'     , expchileaf)
    call prop_get_double (md_ptr, 'veg'    , 'Uchistem'       , uchistem)
    call prop_get_double (md_ptr, 'veg'    , 'Uchileaf'       , uchileaf)
    call prop_get_double (md_ptr, 'veg'    , 'Arealeaf'       , arealeaf)
    call prop_get_double (md_ptr, 'veg'    , 'Cdleaf'         , Cdleaf)

    call prop_get_integer(md_ptr, 'sediment', 'Sedimentmodelnr'    ,  jased) ! 1 = krone, 2 = svr, 3 engelund, 4=D3D
    call prop_get_string (md_ptr, 'sediment', 'SedFile',              md_sedfile,    success)
    call prop_get_string (md_ptr, 'sediment', 'MorFile',              md_morfile,    success)
    call prop_get_string (md_ptr, 'sediment', 'DredgeFile',           md_dredgefile, success)
    call prop_get_integer(md_ptr, 'sediment', 'BndTreatment',         jabndtreatment, success)           ! separate treatment boundary links in upwinding transports
    call prop_get_integer(md_ptr, 'sediment', 'SourSink',             jasourcesink, success)             ! switch off source or sink terms for sed advection
    call prop_get_string (md_ptr, 'sediment', 'MorphoPol',            md_morphopol, success)             ! Only apply mormerge operation/bottom change in polygon
    call prop_get_integer(md_ptr, 'sediment', 'InMorphoPol',          inmorphopol, success)              ! value of the update inside morphopol (only 0 or 1 make sense)
    call prop_get_integer(md_ptr, 'sediment', 'MorCFL',               jamorcfl, success )                ! use morphological time step restriction (1, default) or not (0)
    call prop_get_double (md_ptr, 'sediment', 'DzbDtMax',             dzbdtmax, success)                 ! Max bottom level change per timestep
    call prop_get_double (md_ptr, 'sediment', 'MasBalMinDep',         botcrit, success)                  ! Minimum depth *after* bottom update for SSC adaptation mass balance
    call prop_get_integer(md_ptr, 'sediment', 'MormergeDtUser',       jamormergedtuser, success)         ! Mormerge operation at dtuser timesteps (1) or dts (0, default)
    call prop_get_double (md_ptr, 'sediment', 'UpperLimitSSC',        upperlimitssc, success)             ! Upper limit of cell centre SSC concentration after transport timestep. Default 1d6 (effectively switched off)
    
    call prop_get_integer(md_ptr, 'sediment', 'Nr_of_sedfractions' ,  Mxgr)
    call prop_get_integer(md_ptr, 'sediment', 'MxgrKrone'          ,  MxgrKrone)
    call prop_get_integer(md_ptr, 'sediment', 'Seddenscoupling'    ,  jaseddenscoupling)
    call prop_get_integer(md_ptr, 'sediment', 'Implicitfallvelocity', jaimplicitfallvelocity)


    stm_included = (len_trim(md_sedfile) /= 0 .and. len_trim(md_morfile) /= 0 .and. jased .eq. 4)

    if (jased .eq. 4 .and. .not. stm_included) then
       call mess(LEVEL_ERROR, 'unstruc_model::readMDUFile: Sedimentmodelnr=4, but no *.sed or no *.mor file specified.')
    endif

    !if (jased == 4 .and. stm_included .and. ibedlevtyp > 1) then
    !    call mess(LEVEL_WARN, 'BedlevType = 1 should be used in combination with Sedimentmodelnr = 4')
    !endif

    if (jased*mxgr > 0 .and. .not. stm_included) then

      call allocgrains()

      call prop_get_doubles(md_ptr, 'sediment', 'D50'                ,  D50, Mxgr)
      call prop_get_doubles(md_ptr, 'sediment', 'Rhosed'             ,  rhosed, Mxgr)
      call setgrainsizes()


      if (mxgrKrone > 0) then
         call prop_get_doubles(md_ptr, 'sediment', 'Ws'              ,  Ws,        MxgrKrone)
         call prop_get_doubles(md_ptr, 'sediment', 'Erosionpar'      , erosionpar, MxgrKrone)
         call prop_get_doubles(md_ptr, 'sediment', 'Taucre'          ,    Ustcre2, MxgrKrone)
         Ustcre2 = Ustcre2/rhomean      ! ust2 = tau/rho
      endif

      call prop_get_doubles(md_ptr, 'sediment', 'InitialSedimentConcentration', sedini, mxgr)
      call prop_get_doubles(md_ptr, 'sediment', 'Uniformerodablethickness'    , Uniformerodablethickness, Mxgr)
      call prop_get_integer(md_ptr, 'sediment', 'Numintverticaleinstein'      , Numintverticaleinstein)
      call prop_get_integer(md_ptr, 'sediment', 'Jaceneqtr'                   , Jaceneqtr)
      call prop_get_double (md_ptr, 'sediment', 'Morfac'                      , Dmorfac)
      if (jased == 0) then
         dmorfac = 0d0
      end if

      call prop_get_double (md_ptr, 'sediment', 'TMorfspinup'  , tmorfspinup)
      call prop_get_double (md_ptr, 'sediment', 'Alfabed'      , alfabed)
      call prop_get_double (md_ptr, 'sediment', 'Alfasus'      , alfasus)
      call prop_get_double (md_ptr, 'sediment', 'Crefcav'      , crefcav)

    endif ! jased

    call prop_get_string (md_ptr, 'bedform', 'BedformFile',  md_bedformfile, success)
    bfm_included = len_trim(md_bedformfile) /= 0

    call prop_get_string (md_ptr, 'sedtrails', 'SedtrailsGrid',  md_sedtrailsfile, success)
    if (md_sedtrailsfile/='') then
       inquire(file=md_sedtrailsfile,exist=ex)
       if (ex) then
          jasedtrails=1
          call mess(LEVEL_INFO, 'SedTrails enabled.')
          call prop_get_string (md_ptr, 'sedtrails', 'SedtrailsAnalysis',  sedtrails_analysis, success)
          
          ti_st_array = 0d0
          call prop_get_doubles(md_ptr, 'sedtrails', 'SedtrailsInterval'   ,  ti_st_array, 3, success)
          if (ti_st_array(1) .gt. 0d0) ti_st_array(1) = max(ti_st_array(1) , dt_user)
          call getOutputTimeArrays(ti_st_array, ti_sts, ti_st, ti_ste, success)
          call prop_get_string (md_ptr, 'sedtrails', 'SedtrailsOutputFile',  md_avgsedtrailsfile, success)
          
          call str_lower(sedtrails_analysis)
          if (.not.(trim(sedtrails_analysis)=='all' .or. &
                    trim(sedtrails_analysis)=='flowvelocity' .or. & 
                    trim(sedtrails_analysis)=='transport' .or. &
                    trim(sedtrails_analysis)=='soulsby')) then
             call mess(LEVEL_WARN,'Invalid entry for SedtrailsAnalysis. Should be all, transport, flowvelocity or soulsby. Sedtrails output switched off.')
             jasedtrails=0
          endif  
       endif
    endif
    
    
    call prop_get_integer(md_ptr, 'wind', 'ICdtyp'                    , ICdtyp)
    if (Icdtyp == 1) then
       call prop_get_doubles(md_ptr, 'wind', 'Cdbreakpoints'          , Cdb, 1)
    else if (Icdtyp == 2) then
       call prop_get_doubles(md_ptr, 'wind', 'Cdbreakpoints'          , Cdb, 2)
       call prop_get_doubles(md_ptr, 'wind', 'Windspeedbreakpoints'   , Wdb, 2)
       Cdb(3) = Cdb(2)
       Wdb(2) = max(Wdb(2), Wdb(1) + .1d0)
       Wdb(3) = Wdb(2) + .1d0
    else if (Icdtyp == 3) then
       call prop_get_doubles(md_ptr, 'wind', 'Cdbreakpoints'          , Cdb, 3)
       call prop_get_doubles(md_ptr, 'wind', 'Windspeedbreakpoints'   , Wdb, 3)
       Wdb(2) = max(Wdb(2), Wdb(1) + .1d0)
       Wdb(3) = max(Wdb(3), Wdb(2) + .1d0)
    else if (Icdtyp == 4) then
       call prop_get_doubles(md_ptr, 'wind', 'Cdbreakpoints'          , Cdb, 1)
       cdb(2) = 0d0
    else if (Icdtyp == 7 .or. Icdtyp == 8) then
       call prop_get_doubles(md_ptr, 'wind', 'Cdbreakpoints'          , Cdb, 2)
    endif
    call prop_get_double    (md_ptr, 'wind',  'Relativewind'          , relativewind)
    if (kmx == 0) then
       jawindhuorzwsbased = 1
    else
       jawindhuorzwsbased = 0
    endif
    call prop_get_integer(md_ptr, 'wind',  'Windhuorzwsbased'         , jawindhuorzwsbased)
    call prop_get_integer(md_ptr, 'wind',  'Windpartialdry'           , jawindpartialdry)

    call prop_get_double (md_ptr, 'wind' , 'Rhoair'                   , rhoair )
    call prop_get_double (md_ptr, 'wind' , 'PavIni'                   , PavIni )
    call prop_get_double (md_ptr, 'wind' , 'PavBnd'                   , PavBnd )
    call prop_get_integer(md_ptr, 'wind' , 'Stresstowind'             , jastresstowind )

    call prop_get_integer(md_ptr, 'waves', 'Wavemodelnr'              , jawave)
    call prop_get_double (md_ptr, 'waves', 'Tifetchcomp'              , Tifetch)

    call prop_get_string (md_ptr, 'waves', 'SurfbeatInput'            , md_surfbeatfile)
    if (jawave==4) then
       if (trim(md_surfbeatfile)=='') then
          md_surfbeatfile = 'params.txt'
       end if
    end if
    if (jawave == 5) then
       call prop_get_double(md_ptr, 'waves', 'Hwavuni'    , Hwavuni)
       call prop_get_double(md_ptr, 'waves', 'Twavuni'    , Twavuni)
       call prop_get_double(md_ptr, 'waves', 'Phiwavuni'  , Phiwavuni)
    endif

    call prop_get_string (md_ptr,   'waves', 'Rouwav'           , rouwav)
    if (jawave > 0 .and. .not. flowWithoutWaves) then
       call setmodind(rouwav, modind)
    endif
    call prop_get_double (md_ptr, 'waves', 'Gammax'         , gammax)
    call prop_get_double (md_ptr, 'waves', 'hminlw'         , hminlw)
    call prop_get_integer(md_ptr, 'waves', 'uorbfac'        , jauorb)    ! 0=delft3d4, sqrt(pi)/2 included in uorb calculation; >0: FM, factor not included; default: 0
    call prop_get_logical(md_ptr, 'waves', 'flowWithoutWaves'    , flowWithoutWaves) ! True: Do not use Wave data in the flow computations, it will only be passed through to D-WAQ
    ! backward compatibility for hk in tauwavehk:
    if (jawave>0 .and. jawave<3 .or. flowWithoutWaves) then
       jauorb=1
    endif
    call prop_get_integer(md_ptr, 'waves', 'jahissigwav'         , jahissigwav)     ! 1: sign wave height on his output; 0: hrms wave height on his output. Default=1
    call prop_get_integer(md_ptr, 'waves', 'jamapsigwav'         , jamapsigwav)     ! 1: sign wave height on map output; 0: hrms wave height on map output. Default=0 (legacy)
    call prop_get_integer(md_ptr, 'waves', 'jauorbfromswan'      , jauorbfromswan)  ! 1: use orbital velocities from com file; 0=internal uorb calculation. Default=0
    call prop_get_double (md_ptr, 'waves', 'fwfac'               , fwfac)           ! factor for adjusting wave boundary layer streaming, default 1.0
    call prop_get_double (md_ptr, 'waves', 'ftauw'               , ftauw)           ! factor for adjusting wave related bottom shear stress
    call prop_get_double (md_ptr, 'waves', 'fbreak'              , fbreak)          ! factor for adjusting wave breaking contribution to tke
    if (ftauw<0d0) then
       call mess(LEVEL_WARN, 'unstruc_model::readMDUFile: ftauw<0d0, reset to 0d0. Bed shear stress due to waves switched off.')
       ftauw = 0d0
    endif
    if (fwfac<0d0) then
       call mess(LEVEL_WARN, 'unstruc_model::readMDUFile: fwfac<0d0, reset to 0d0. Wave streaming effect switched off.') 
       fwfac = 0d0  
    endif
    if (fbreak<0d0) then
       call mess(LEVEL_WARN, 'unstruc_model::readMDUFile: fbreak<0d0, reset to 0d0. Wave breaking contribution to tke switched off.') 
       fwfac = 0d0  
    endif
    
    ! using stokes drift in very discontinuous wave fields in the fetch approach is discouraged (experience from SF Bay)
    if (jawave==1 .or. jawave==2) then
       jawaveStokes = 0
    endif   
    call prop_get_integer(md_ptr, 'waves', '3Dstokesprofile'     , jawaveStokes)    ! Stokes profile. 0: no, 1:uniform over depth, 2: 2nd order Stokes theory; 3: 2, with vertical stokes gradient in adve; 4: 3, with stokes contribution vert viscosity 
    if ((jawave==1 .or. jawave==2) .and. jawaveStokes>0) then
       write(msgbuf, *) 'unstruc_model::readMDUFile: wavemodelnr=',jawave,', and 3Dstokesprofile=',jawavestokes,'. It is *strongly* advised to leave 3Dstokesprofile at 0 when using fetch based wave models.'
       call warn_flush()
    endif   
    call prop_get_integer(md_ptr, 'waves', '3Dwavestreaming'     , jawavestreaming) ! Influence of wave streaming. 0: no, 1: added to adve
    call prop_get_integer(md_ptr, 'waves', '3Dwaveboundarylayer' , jawavedelta)     ! Boundary layer formulation. 1: Sana
    call prop_get_integer(md_ptr, 'waves', '3Dwaveforces'        , jawaveforces)    ! Diagnostic mode: apply wave forces (1) or not (0)
    call prop_get_double(md_ptr,  'waves', '3Dwaveturbpendepth'  , fwavpendep)      ! Layer thickness as proportion of Hrms over which wave breaking adds to TKE source. Default 0.5
    !
    ! safety
    if (fwavpendep<0d0) then
       fwavpendep = 0d0
       write(msgbuf, *) 'unstruc_model::readMDUFile: 3Dwaveturbpendepth<0.0, reset to 0.0. Wave breaking switched off as a source for TKE.'
       call warn_flush()
    endif
    
    call prop_get_integer(md_ptr, 'grw'  , 'groundwater'        , jagrw)

    call prop_get_integer(md_ptr, 'grw'  , 'Infiltrationmodel'  , Infiltrationmodel)
    if (infiltrationmodel == 1 .or. infiltrationmodel == DFM_HYD_INFILT_DARCY) then
       jagrw = 1
    else if (infiltrationmodel == DFM_HYD_INFILT_HORTON) then
       jadhyd = 1
    end if

    call prop_get_double (md_ptr, 'grw'  , 'Hinterceptionlayer' , Hinterceptionlayer)
    call prop_get_double (md_ptr, 'grw'  , 'InfiltrationVelocity' , infiltcapuni) ! old keyword: backwards compatibility
    call prop_get_double (md_ptr, 'grw'  , 'UnifInfiltrationCapacity' , infiltcapuni)
    call prop_get_double (md_ptr, 'grw'  , 'Conductivity'       , Conductivity)
    call prop_get_double (md_ptr, 'grw'  , 'h_aquiferuni'       , h_aquiferuni)
    call prop_get_double (md_ptr, 'grw'  , 'h_unsatini'         , h_unsatini)
    call prop_get_double (md_ptr, 'grw'  , 'sgrwini'            , sgrwini)
    call prop_get_double (md_ptr, 'grw'  , 'bgrwuni'            , bgrwuni)

    !
    ! Hydrology (takes over some functionality that used to be under [grw])
    !
    call prop_get_integer(md_ptr, 'hydrology'  , 'InterceptionModel', interceptionmodel)

! Time
    call prop_get_string(md_ptr, 'time', 'RefDate', refdat)
    read(refdat,*) irefdate
    success = ymd2modified_jul(irefdate, refdate_mjd)
    if (.not.success) then
       call mess(LEVEL_ERROR, 'Something went wrong in conversion from RefDate to Modified Julian Date')
    endif
    call prop_get_double(md_ptr, 'time', 'Tzone', Tzone)
    call prop_get_string(md_ptr, 'time', 'Tunit', md_tunit)
    call prop_get_double(md_ptr, 'time', 'TStart', tstart_user)
    tstart_user = max(tstart_user, 0d0)
    call prop_get_double (md_ptr, 'time', 'TStop', tstop_user)
    select case (md_tunit)                                            ! tfac added here for use in sedmorinit
    case('D')
        tstart_user = tstart_user*3600*24
        tstop_user  = tstop_user*3600*24
        tfac = 3600d0*24d0
    case('H')
        tstart_user = tstart_user*3600
        tstop_user  = tstop_user*3600
        tfac = 3600d0
    case('M')
        tstart_user = tstart_user*60
        tstop_user  = tstop_user*60
        tfac = 60d0
    case default
        tfac = 1d0
    end select

    call setTUDUnitString()

    call prop_get_double  (md_ptr, 'time', 'DtUser', dt_user)
    if (dt_user <= 0) then
        dt_user          = 300d0
        dt_max           = 60d0
        ja_timestep_auto = 1
    end if

    call prop_get_double (md_ptr, 'time', 'DtNodal', dt_nodal)

    call prop_get_double  (md_ptr, 'time', 'DtMax',  dt_max)
    if (dt_max > dt_user) then
        dt_max = dt_user
        write(msgbuf, '(a,f9.3)') 'DtMax should be <= DtUser. It has been reset to: ', dt_max
        call msg_flush()
    end if

    ! 1.02: Don't read [time] AutoTimestep (ja_timestep_auto) from MDU anymore.
    ! ibuf = 1
    call prop_get_integer (md_ptr, 'time', 'AutoTimestep', ja_timestep_auto, success)
    call prop_get_integer (md_ptr, 'time', 'Autotimestepdiff' , jadum, success)
    if ( success .and. jadum.ne.0 ) then
       call mess(LEVEL_ERROR, 'Autotimestepdiff not supported')
    end if
    call prop_get_integer (md_ptr, 'time', 'Autotimestepvisc' , ja_timestep_auto_visc, success)
    if ( success .and. ja_timestep_auto_visc.ne.0 ) then
       if (  ja_timestep_auto_visc.ne.1234 ) then
!         hide feature
          call mess(LEVEL_ERROR, 'Autotimestepvisc not supported')
       else
          ja_timestep_auto_visc = 1
       end if
    end if
    ! if (success .and. ibuf /= 1) then
    !   write(msgbuf, '(a,i0,a)') 'MDU [time] AutoTimestep=', ibuf, ' is deprecated, timestep always automatic. Use DtMax instead.'
    !   call warn_flush()
    ! endif
    call prop_get_integer (md_ptr, 'time', 'AutoTimestepNoStruct' , ja_timestep_nostruct, success)
    call prop_get_integer (md_ptr, 'time', 'AutoTimestepNoQout' ,   ja_timestep_noqout,   success)

    call prop_get_double  (md_ptr, 'time', 'Dtfacmax',  dtfacmax)

    call prop_get_double  (md_ptr, 'time', 'DtInit',  dt_init)

    call prop_get_integer (md_ptr, 'time', 'Timestepanalysis', jatimestepanalysis)

    Startdatetime = ' '
    call prop_get_string  (md_ptr, 'time', 'Startdatetime', Startdatetime, success)
    if (len_trim(Startdatetime) > 0 .and. success) then
        call datetimestring_to_seconds(Startdatetime, refdat, tim, iostat)
        if (iostat == 0) then
           Tstart_user = tim
        endif
    endif

    Stopdatetime = ' '
    call prop_get_string  (md_ptr, 'time', 'Stopdatetime', Stopdatetime, success)
    if (len_trim(Stopdatetime) > 0 .and. success) then
        call datetimestring_to_seconds(Stopdatetime, refdat, tim, iostat)
        if (iostat == 0) then
           Tstop_user = tim
        endif
    endif

    ! Set update frequency for the time dependent roughness from frictFile.
    call prop_get (md_ptr, 'time', 'UpdateRoughnessInterval', dt_UpdateRoughness)
    if (dt_UpdateRoughness < dt_User) then
       ! NOTE: dt_UpdateRoughness must at least be >= DTMax, but we'll enforce DTUser, because that makes more sense anyway.
       call SetMessage(LEVEL_ERROR, 'The value of "UpdateRoughnessInterval" must be equal to or larger than the user time step.')
    endif
    !
    ! TIDAL TURBINES: Insert calls to rdturbine and echoturbine here (use the structure_turbines variable defined in m_structures)
    !
! Restart information
    call prop_get_string(md_ptr, 'restart', 'RestartFile', md_restartfile, success)
    restartdatetime = 'yyyymmddhhmmss'
    call prop_get_string(md_ptr, 'restart', 'RestartDateTime', restartdatetime, success)
    call prop_get_integer(md_ptr, 'restart', 'RstIgnoreBl', jarstignorebl, success)

! External forcings
    call prop_get_string(md_ptr, 'external forcing', 'ExtForceFile', md_extfile, success)
    call prop_get_string(md_ptr, 'external forcing', 'ExtForceFileNew', md_extfile_new, success)
    call prop_get_integer(md_ptr, 'external forcing', 'Rainfall', jarain, success)
    if (jarain > 0) then
       jaqin = 1
    end if
    call prop_get_integer(md_ptr, 'external forcing', 'QExt', jaQext, success)
    if (jaQext > 0) then
       jaqin = 1
    end if
    call prop_get_integer(md_ptr, 'external forcing', 'Evaporation', jaevap, success)
    if (jaevap > 0) then
       jaqin = 1
    end if
    call prop_get_integer(md_ptr, 'external forcing', 'WindExt', jawind, success)
 

! Trachytopes
    ! Further reading is done in m_rdtrt, by passing just the [trachytopes] chapter as a separate trtdef_ptr to rdtrt.
    ! Mark [Trachytopes] section as read
    call tree_get_node_by_name( md_ptr, 'trachytopes', trtdef_ptr)
    !if (associated(trtdef_ptr)) call visit_tree(trtdef_ptr,1)
    trtdef_ptr => null()

    call prop_get_string (md_ptr, 'trachytopes', 'TrtRou' , md_trtrfile, success)
    if (strcmpi(md_trtrfile,'Y')) then
       call tree_get_node_by_name( md_ptr, 'trachytopes', trtdef_ptr)
       if (associated(trtdef_ptr)) then
          jatrt = 1
       end if
    end if
    call prop_get_string(md_ptr, 'trachytopes', 'TrtDef', md_trtdfile, success)
    call prop_get_string(md_ptr, 'trachytopes', 'TrtL', md_trtlfile, success)
    call prop_get_double(md_ptr, 'trachytopes', 'DtTrt', dt_trach, success)
    if (.not. success) dt_trach = dt_user
    call prop_get_integer(md_ptr, 'trachytopes', 'TrtMxR', md_mxrtrach, success)
    call prop_get_string(md_ptr, 'trachytopes', 'TrtCll' , md_trtcllfile, success)
    call prop_get_double(md_ptr, 'trachytopes', 'TrtMnH', md_mnhtrach, success)
    call prop_get_integer(md_ptr, 'trachytopes', 'TrtMth', md_mthtrach, success)

! Calibration factor
    call prop_get_integer(md_ptr, 'calibration', 'UseCalibration'   , jacali, success)
    call prop_get_string(md_ptr, 'calibration', 'DefinitionFile'   , md_cldfile,  success)
    call prop_get_string(md_ptr, 'calibration', 'AreaFile'  , md_cllfile,  success)

! Output
    ! [output] OutputDir was read earlier already.
    call prop_get_string(md_ptr, 'output', 'ObsFile', md_obsfile, success)
    call prop_get_string(md_ptr, 'output', 'CrsFile', md_crsfile, success)
    call prop_get_string(md_ptr, 'output', 'RugFile', md_rugfile, success)
    call prop_get_string(md_ptr, 'output', 'FouFile', md_foufile, success)
    call prop_get_integer(md_ptr, 'output', 'FouUpdateStep', md_fou_step, success)

    call prop_get_string(md_ptr, 'output', 'HisFile', md_hisfile, success)
    ti_his_array = 0d0
    call prop_get_doubles(md_ptr, 'output', 'HisInterval'   ,  ti_his_array, 3, success)
    if (ti_his_array(1) .gt. 0d0) ti_his_array(1) = max(ti_his_array(1) , dt_user)
    call getOutputTimeArrays(ti_his_array, ti_hiss, ti_his, ti_hise, success)

    call prop_get_double(md_ptr, 'output', 'XLSInterval', ti_xls, success)

    call prop_get_string(md_ptr, 'output', 'FlowGeomFile', md_flowgeomfile, success)

    call prop_get_string(md_ptr, 'output', 'MapFile', md_mapfile, success)

    ti_map_array = 0d0
    call prop_get_doubles(md_ptr, 'output', 'MapInterval'   ,  ti_map_array, 3, success)
    if (ti_map_array(1) .gt. 0d0) ti_map_array(1) = max(ti_map_array(1) , dt_user)
    call getOutputTimeArrays(ti_map_array, ti_maps, ti_map, ti_mape, success)

    call prop_get_integer(md_ptr, 'output', 'MapFormat', md_mapformat, success)
    if (md_mapformat == IFORMAT_UGRID) then
       md_unc_conv = UNC_CONV_UGRID
    else
       md_unc_conv = UNC_CONV_CFOLD
       ! write (msgbuf, '(a,i0,a,i0,a)') 'Old format MapFormat=', md_mapformat, ' requested, which is now deprecated. Consider upgrading to UGRID-compliant output: MapFormat=', IFORMAT_UGRID, '.'
       ! call warn_flush() ! TODO: AvD: enable this message once UGRID has become the default.

       ! md_unc_conv = UNC_CONV_UGRID ! TODO: AvD: TEMP for testing. REMOVE!
    end if
    !
    ! If sedtrails is used, only use Mapformat=4
    if (jasedtrails>0 .and. md_mapformat==IFORMAT_NETCDF) then
       md_unc_conv = UNC_CONV_UGRID
       write (msgbuf, '(a,i0,a,i0,a)') 'Old format MapFormat=', IFORMAT_NETCDF, ' requested, which is not used when SedTrails output is activated. Output set to MapFormat=', IFORMAT_UGRID, '.'
       call warn_flush()
    endif   

    call prop_get_integer(md_ptr, 'output', 'NcFormat', md_ncformat, success)
    call unc_set_ncformat(md_ncformat)

    call prop_get_integer(md_ptr, 'output', 'NcNoUnlimited', unc_nounlimited, success)
    call prop_get_integer(md_ptr, 'output', 'NcNoForcedFlush',  unc_noforcedflush, success)

    ibuf = 0
    call prop_get_integer(md_ptr, 'output', 'NcWriteLatLon', ibuf, success)
    if (success .and. ibuf > 0) then
       unc_writeopts = UG_WRITE_LATLON
    end if

    call prop_get_string(md_ptr, 'output', 'MetaDataFile', unc_metadatafile, success)

    call prop_get_integer(md_ptr, 'output', 'GenerateUUID', unc_uuidgen, success)

    call prop_get_integer(md_ptr, 'output', 'Wrihis_balance', jahisbal, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_sourcesink', jahissourcesink, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_structure_gen', jahiscgen, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_structure_dam', jahiscdam, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_structure_pump', jahispump, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_structure_gate', jahisgate, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_structure_weir', jahisweir, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_structure_orifice', jahisorif, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_structure_bridge', jahisbridge, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_structure_culvert', jahisculv,  success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_structure_damBreak', jahisdambreak,  success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_structure_uniWeir', jahisuniweir,  success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_structure_compound', jahiscmpstru,  success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_structure_longculvert', jahislongculv,  success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_turbulence', jahistur, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_wind', jahiswind, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_rain', jahisrain, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_infiltration', jahisinfilt, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_temperature', jahistem, success)
    if (success .and. jahistem == 1 .and. jatem < 1) then
      write (msgbuf, '(a)') 'MDU setting "Wrihis_temperature = 1" asks to write temperature to the output his file, ' &
         //'but no temperature is involved due to MDU setting "Temperature = 0". So we set "Wrihis_temperature = 0" '&
         //' and do not write temperature to his file.'
      call warn_flush()
    end if
    call prop_get_integer(md_ptr, 'output', 'Wrihis_heat_fluxes', jahisheatflux, success)
    if (.not. success) then
      call prop_get_integer(md_ptr, 'output', 'Wrihis_heatflux', jahisheatflux, success)
    endif
    call prop_get_integer(md_ptr, 'output', 'Wrihis_salinity', jahissal, success)
    if (success .and. jahissal == 1 .and. jasal < 1) then
      write (msgbuf, '(a)') 'MDU setting "Wrihis_salinity = 1" asks to write salinity to the output his file, ' &
         //'but no salinity is involved due to MDU setting "Salinity = 0". So we set "Wrihis_salinity = 0" '&
         //'and do not write salinity to his file.'
      call warn_flush()
    end if
    call prop_get_integer(md_ptr, 'output', 'Wrihis_density', jahisrho, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_waterlevel_s1', jahiswatlev, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_bedlevel', jahisbedlev, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_waterdepth', jahiswatdep, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_waves', jahiswav, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_velocity_vector', jahisvelvec, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_upward_velocity_component', jahisww, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_sediment', jahissed, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_constituents', jahisconst, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_zcor', jahiszcor, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_lateral', jahislateral, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_taucurrent', jahistaucurrent, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_velocity', jahisvelocity, success)
    call prop_get_integer(md_ptr, 'output', 'Wrihis_discharge', jahisdischarge, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_waterlevel_s0', jamaps0, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_waterlevel_s1', jamaps1, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_evaporation', jamapevap, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_volume1', jamapvol1, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_waterdepth', jamaphs, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_waterdepth_hu', jamaphu, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_ancillary_variables', jamapanc, success)
    if (jamapanc > 0) then
       if (jamaps1 <= 0) then
          jamaps1 = 1
          write (msgbuf, '(a, i0, a)') 'MDU setting "Wrimap_ancillary_variables = ', jamapanc, '" requires ' &
             //'"Wrimap_waterlevel_s1 = 1". Has been enabled now.'
          call warn_flush()
       end if
       if (jamaphs <= 0) then
          jamaphs = 1
          write (msgbuf, '(a, i0, a)') 'MDU setting "Wrimap_ancillary_variables = ', jamapanc, '" requires ' &
             //'"Wrimap_waterdepth = 1". Has been enabled now.'
          call warn_flush()
       end if
       if (jamaphu <= 0) then
          jamaphu = 1
          write (msgbuf, '(a, i0, a)') 'MDU setting "Wrimap_ancillary_variables = ', jamapanc, '" requires ' &
             //'"Wrimap_waterdepth_hu = 1". Has been enabled now.'
          call warn_flush()
       end if
    end if
    call prop_get_integer(md_ptr, 'output', 'Wrimap_flow_analysis', jamapFlowAnalysis, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_flowarea_au', jamapau, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_velocity_component_u1', jamapu1, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_velocity_component_u0', jamapu0, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_velocity_vector', jamapucvec, success)
    if ((jawave==3 .or. jawave==6) .and. jamapucvec==0) then
       jamapucvec = 1
       write (msgbuf, '(a, i0, a)') 'MDU setting "Wavemodelnr = ', jawave, '" requires ' &
          //'"Wrimap_velocity_vector = 1". Has been enabled now.'
       call warn_flush()
    endif
    call prop_get_integer(md_ptr, 'output', 'Wrimap_velocity_magnitude', jamapucmag, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_velocity_vectorq', jamapucqvec, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_upward_velocity_component', jamapww1, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_density_rho', jamaprho, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_horizontal_viscosity_viu', jamapviu, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_horizontal_diffusivity_diu', jamapdiu, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_flow_flux_q1', jamapq1, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_flow_flux_q1_main', jamapq1main, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_fixed_weir_energy_loss', jamapfw, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_spiral_flow', jamapspir, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_numlimdt', jamapnumlimdt, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_taucurrent', jamaptaucurrent, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_z0', jamapz0, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_salinity', jamapsal, success)
    if (success .and. jamapsal == 1 .and. jasal < 1) then
      write (msgbuf, '(a)') 'MDU setting "Wrimap_salinity = 1" asks to write salinity to the output map file, ' &
         //'but no salinity is involved due to MDU setting "Salinity = 0". So we set "Wrimap_salinity = 0" '&
         //'and do not write salinity to map file.'
      call warn_flush()
    end if
    
    call prop_get_integer(md_ptr, 'output', 'Wrimap_chezy', jamap_chezy_elements, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_chezy_on_flow_links', jamap_chezy_links, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_input_roughness', jamap_chezy_input, success)
    
    call prop_get_integer(md_ptr, 'output', 'Wrimap_temperature', jamaptem, success)
    if (success .and. jamaptem == 1 .and. jatem < 1) then
      write (msgbuf, '(a)') 'MDU setting "Wrimap_temperature = 1" asks to write temperature to the output map file, ' &
         //'but no temperature is involved due to MDU setting "Temperature = 0". So we set "Wrimap_temperature = 0"'&
         //'and do not write temperature to map file.'
      call warn_flush()
    end if

    call prop_get_integer(md_ptr, 'output', 'Wrimap_constituents', jamapconst, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_sediment', jamapsed, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_turbulence', jamaptur, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_trachytopes', jamaptrachy, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_calibration', jamapcali, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_rain', jamaprain, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_interception', jamapicept, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_wind', jamapwind, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_windstress', jamapwindstress, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_heat_fluxes', jamapheatflux, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_tidal_potential', jamaptidep, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_sal_potential', jamapselfal, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_internal_tides_dissipation', jamapIntTidesDiss, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_nudging', jamapnudge, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_waves',jamapwav, success)
    jamapwav_hwav   = 0
    jamapwav_twav   = 0
    jamapwav_phiwav = 0
    jamapwav_sxwav  = 0
    jamapwav_sywav  = 0
    jamapwav_sxbwav = 0
    jamapwav_sybwav = 0
    jamapwav_mxwav  = 0
    jamapwav_mywav  = 0
    jamapwav_dsurf  = 0
    jamapwav_dwcap  = 0
    jamapwav_uorb   = 0

    call prop_get_integer(md_ptr, 'output', 'Wrimap_DTcell',jamapdtcell, success)
    epswetout = epshs ! the same as numerical threshold to counts as 'wet'.
    call prop_get_double (md_ptr, 'output', 'Wrimap_wet_waterdepth_threshold', epswetout, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_time_water_on_ground', jamapTimeWetOnGround, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_freeboard', jamapFreeboard, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_waterdepth_on_ground', jamapDepthOnGround, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_volume_on_ground', jamapVolOnGround, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_total_net_inflow_1d2d', jamapTotalInflow1d2d, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_total_net_inflow_lateral', jamapTotalInflowLat, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_water_level_gradient', jamapS1Gradient, success)
    call prop_get_integer(md_ptr, 'output', 'Writek_CdWind', jatekcd, success)
    call prop_get_integer(md_ptr, 'output', 'Wrirst_bnd', jarstbnd, success)
    call prop_get_integer(md_ptr, 'output', 'Writepart_domain', japartdomain, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_bnd', jamapbnd, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_Qin', jamapqin, success)
    call prop_get_integer(md_ptr, 'output', 'Wrimap_every_dt', jaeverydt, success)

    !if (md_mapformat /= 4 .and. jamapwindstress /= 0) then
     !  call mess(LEVEL_ERROR, 'writing windstress to mapfile is only implemented for NetCDF - UGrid (mapformat=4)')
    !endif

    if (jatem <= 1) then
      jamapheatflux = 0
      jahisheatflux = 0
    endif
    if (jatem < 1) then ! If no temperature is involved, then do not write temperature to output map/his files.
      jamaptem = 0
      jahistem = 0
    end if
    if (jasal < 1) then ! If no salinity is involved, then do not write salinity to output map/his files.
      jamapsal = 0
      jahissal = 0
    end if

    call prop_get_integer(md_ptr, 'output', 'Richardsononoutput', jaRichardsononoutput, success)

    call prop_get_integer(md_ptr, 'output', 'Wrishp_crs', jashp_crs, success)
    call prop_get_integer(md_ptr, 'output', 'Wrishp_obs', jashp_obs, success)
    call prop_get_integer(md_ptr, 'output', 'Wrishp_weir', jashp_weir, success)
    call prop_get_integer(md_ptr, 'output', 'Wrishp_thd', jashp_thd, success)
    call prop_get_integer(md_ptr, 'output', 'Wrishp_gate', jashp_gate, success)
    call prop_get_integer(md_ptr, 'output', 'Wrishp_emb', jashp_emb, success)
    call prop_get_integer(md_ptr, 'output', 'Wrishp_fxw', jashp_fxw, success)
    call prop_get_integer(md_ptr, 'output', 'Wrishp_src', jashp_src, success)
    call prop_get_integer(md_ptr, 'output', 'Wrishp_pump', jashp_pump, success)
    call prop_get_integer(md_ptr, 'output', 'Wrishp_dryarea', jashp_dry, success)
    call prop_get_integer(md_ptr, 'output', 'wrishp_genstruc', jashp_genstruc, success)

    call prop_get_integer(md_ptr, 'output', 'WriteDFMinterpretedvalues', jawriteDFMinterpretedvalues, success)

    call prop_get_integer(md_ptr, 'output', 'WriteDetailedTimers', jawriteDetailedTimers, success)

    ti_rst_array = 0d0
    call prop_get_doubles(md_ptr, 'output', 'RstInterval'   ,  ti_rst_array, 3, success)
    call getOutputTimeArrays(ti_rst_array, ti_rsts, ti_rst, ti_rste, success)

    call prop_get_double (md_ptr, 'output', 'MbaInterval', ti_mba, success)

    call prop_get_integer (md_ptr, 'output', 'MbaWriteCsv', jambawritecsv, success)

    call prop_get_integer (md_ptr, 'output', 'MbaLumpFromToMba', jambalumpmba, success)
    call prop_get_integer (md_ptr, 'output', 'MbaLumpBoundaries', jambalumpbnd, success)
    call prop_get_integer (md_ptr, 'output', 'MbaLumpSourceSinks', jambalumpsrc, success)
    call prop_get_integer (md_ptr, 'output', 'MbaLumpProcesses', jambalumpproc, success)

!    call prop_get_string(md_ptr, 'output', 'WaqFileBase', md_waqfilebase, success)
    ! Default basename of Delwaq files is model identifier:
    if (len_trim(md_waqfilebase) == 0) then
      md_waqfilebase = md_ident
    end if

    call prop_get_string(md_ptr, 'output', 'WaqOutputDir', md_waqoutputdir, success)

    ti_waq_array = 0d0
    call prop_get_doubles(md_ptr, 'output', 'WaqInterval'   ,  ti_waq_array, 3, success)
    call getOutputTimeArrays(ti_waq_array, ti_waqs, ti_waq, ti_waqe, success)
    call prop_get_string(md_ptr, 'output', 'WaqHorAggr', md_waqhoraggr, success)
    call prop_get_string(md_ptr, 'output', 'WaqVertAggr', md_waqvertaggr, success)
    call prop_get_integer(md_ptr, 'waves', 'waveSwartDelwaq' , jawaveSwartDelwaq, success)
    if (jawave==0) then
       if (jawaveSwartdelwaq==1) then
          jawaveswartdelwaq = 0
          call mess(LEVEL_WARN,'Waves inactive and waveswartdelwaq=1; Reset waveswartdelwaq to 0.')
       endif
    endif
    !
    if(dt_user > 0 .and. ti_waq > 0) then
       if(ti_waq < dt_user .or. modulo(ti_waq, dt_user) /= 0d0) then
         ti_waq = max(1,floor(ti_waq/dt_user))*dt_user
         ! Delwaq can only handle integer multiples of seconds exchange times...
         if(int(ti_waq) == ti_waq) then
            write(msgbuf, '(a,f9.3)') 'WaqInterval should be a multiple of DtUser. It has been reset to: ', ti_waq
         else
            ti_waq = 0d0
            write(msgbuf, '(a,f9.3)') 'WaqInterval cannot be clipped to integer number of seconds. Waq output has been disabled '
         end if
         call mess(LEVEL_WARN, msgbuf)
       end if
    end if

    call prop_get_double(md_ptr, 'output', 'StatsInterval', ti_stat, success)

    call prop_get_double(md_ptr, 'output', 'TimingsInterval', ti_timings, success)

    charbuf = ' '
    call prop_get_string(md_ptr, 'output', 'TimeSplitInterval', charbuf, success)
    if (success) then
        read(charbuf, *, iostat=iostat), ibuf, ti_split_unit
        if (iostat == 0) then
            ti_split = dble(ibuf)
            select case(ti_split_unit)
            case ('Y','M','D','h','m','s')
                if (ti_split < 0d0) then ! Invalid time value, error.
                    iostat = 12345
                end if
            case default
                iostat = 12346 ! Invalid time unit, error.
            end select
        end if
        if (iostat /= 0) then
            ti_split = 0d0
            ti_split_unit = 's'
            call mess(LEVEL_WARN, 'TimeSplitInterval invalid, disabling time partitioning of output. Got: ', trim(charbuf))
        end if
    end if

    call prop_get_string ( md_ptr, 'output', 'MapOutputTimeVector', md_mptfile, success)

    warn = 0
    inquire (file = trim(md_mptfile), exist = success)
    if (success) then
       call oldfil (mptfile, trim(md_mptfile))
       readerr = 0
       j = 0
       do while (readerr.eq.0)
          read(mptfile,*, iostat = readerr)
          j = j + 1
       enddo
       je = j-1
       je = max(je,1)
       if ( allocated(ti_mpt)     ) deallocate(ti_mpt)
       if ( allocated(ti_mpt_rel) ) deallocate(ti_mpt_rel)
       allocate(ti_mpt(je))
       allocate(ti_mpt_rel(je))
       rewind(mptfile)
       do j = 1,je
           read(mptfile,*) ti_mpt(j)
           acc          = 1d0
           ti_mpt(j) = ceiling(ti_mpt(j)*acc)/acc
           if (ti_mpt(j) .lt. 0d0) then
               call mess(LEVEL_WARN, 'Negative times demanded in MapOutputTimeVector. Please modify the time values. MapOutputTimeVector is kept out of consideration.')
               warn = 1
           end if
           if (j .gt. 1) then
               if (ti_mpt(j) .le. ti_mpt(j-1)) then
                   call mess(LEVEL_WARN, 'MapOutputTimeVector contains unsorted times (rounded to seconds). Please sort the data first. MapOutputTimeVector is kept out of consideration.')
                   warn = 1
               end if
           end if
       enddo
       close(mptfile)
    else
       if ( allocated(ti_mpt)     ) deallocate(ti_mpt)
       if ( allocated(ti_mpt_rel) ) deallocate(ti_mpt_rel)
       allocate(ti_mpt(1))
       allocate(ti_mpt_rel(1))
       ti_mpt     = 0D0
       ti_mpt_rel = tstop_user
    endif
    if (warn .eq. 1) then
       if ( allocated(ti_mpt)     ) deallocate(ti_mpt)
       if ( allocated(ti_mpt_rel) ) deallocate(ti_mpt_rel)
       allocate(ti_mpt(1))
       allocate(ti_mpt_rel(1))
       ti_mpt     = 0D0
       ti_mpt_rel = tstop_user
    endif

    call prop_get_integer( md_ptr, 'output', 'FullGridOutput', jafullgridoutput, success)
    if (jawave==3 .and. kmx>0) then
       jafullgridoutput=1
       call mess(LEVEL_WARN, 'D-WAVES coupling enabled and kmx>0, so layer interface coordinates are needed on com-file. FullGridOutput set to 1.')
       call warn_flush()
    endif
    ! Check for ocean_sigma_z in combination with numtopsig=0, then fullgridoutput=1 is the only option (UNST-5477).
    if (success) then
       if (jafullgridoutput == 0 .and. Layertype > 1 .and. Numtopsig > 0 .and. kmx > 0 .and. janumtopsiguniform /= 1) then
          jafullgridoutput = 1
          call mess(LEVEL_WARN, 'A combination of Z- and Sigma-layers is used, but Numtopsiguniform is not 1. FullGridOutput must be set to 1.')
          call warn_flush()
       endif
    else
       if (Layertype > 1 .and. Numtopsig > 0 .and. kmx > 0 .and. janumtopsiguniform /= 1) then
          jafullgridoutput = 1
       endif
    endif
   
    call prop_get_integer( md_ptr, 'output', 'EulerVelocities', jaeulervel)
    if ((jawave<3 .or. flowWithoutWaves) .and. jaeulervel == 1 ) then 
       call mess(LEVEL_WARN, '''EulerVelocities'' is not compatible with the selected Wavemodelnr. ''EulerVelocities'' is set to zero.')
       jaeulervel = 0
    endif
    !
    if (jawave .eq. 4) then    ! not for Delta Shell
       call prop_get_integer(md_ptr, 'output', 'AvgWaveQuantities'    , jaavgwavquant)
       call prop_get_string(md_ptr,  'output', 'AvgWaveQuantitiesFile', md_avgwavquantfile, success)
       ti_wav_array=0d0
       call prop_get_doubles(md_ptr, 'output', 'AvgWaveOutputInterval'   ,  ti_wav_array, 3, success)
       if (ti_wav_array(2).lt.0d0) then
          ti_wav_array(2) = 0d0
          ti_wav_array(3) = 0d0
       end if
       call getOutputTimeArrays(ti_wav_array, ti_wavs, ti_wav, ti_wave, success)

       if (jaavgwavquant>0 .and. ti_wav > (tstop_user-tstart_user)) then
          ti_wav = tstop_user-tstart_user
          call mess(LEVEL_WARN, '''AvgWaveOutputInterval'' is larger than output duration in the simulation.')
          write(msgbuf, *)     'Setting ''AvgWaveOutputInterval'' to tstop-tstart = ', ti_wav, ' s'
          call warn_flush()
       endif

       if (jaavgwavquant==1)  then
           jaavgwriteall = 1            ! write all by default, unless explicitly switched off
       end if

       call prop_get_integer(md_ptr, 'output', 'MomentumBalance'    , jamombal, success)
       call prop_get_integer(md_ptr, 'output', 'AvgWaveWriteAll'    , jaavgwriteall, success)
       if (success .and. jaavgwriteall.eq.0) then    ! else don't bother, everything written anyway
           call prop_get_integer(md_ptr, 'output', 'AvgWaveWriteH'    , jaavgwriteH    , success)   ! height
           call prop_get_integer(md_ptr, 'output', 'AvgWaveWriteE'    , jaavgwriteE    , success)   ! energy
           call prop_get_integer(md_ptr, 'output', 'AvgWaveWriteR'    , jaavgwriteR    , success)   ! roller
           call prop_get_integer(md_ptr, 'output', 'AvgWaveWriteD'    , jaavgwriteD    , success)   ! dissipation breaking+roller
           call prop_get_integer(md_ptr, 'output', 'AvgWaveWriteCel'  , jaavgwriteCel  , success)   ! celerity+group velocity
           call prop_get_integer(md_ptr, 'output', 'AvgWaveWriteDir'  , jaavgwriteDir  , success)   ! wave direction
           call prop_get_integer(md_ptr, 'output', 'AvgWaveWriteU'    , jaavgwriteU    , success)   ! velocity + stokes
           call prop_get_integer(md_ptr, 'output', 'AvgWaveWriteF'    , jaavgwriteF    , success)   ! wave forces
           call prop_get_integer(md_ptr, 'output', 'AvgWaveWriteUrms' , jaavgwriteUrms , success)   ! orbital velocities
           call prop_get_integer(md_ptr, 'output', 'AvgWaveWriteS'    , jaavgwriteS    , success)   ! water level
           call prop_get_integer(md_ptr, 'output', 'AvgWaveWriteSigm' , jaavgwriteSigm , success)   ! frequency
       end if

       ! Override settings above if momentum balance data is switched on
       if (jamombal>0) then
          md_unc_conv = UNC_CONV_UGRID
          call mess(LEVEL_WARN, 'Momentum balance terms requested as output, Mapformat set to UGRID.')
          call warn_flush()
          call mess(LEVEL_WARN, 'Momentum balance terms requested as output, GLM velocities saved on wav-file.')  ! set in make_averages
          call warn_flush()
          jaavgwriteU    = 1
          jaavgwriteF    = 1  
          jaavgwriteS    = 1 
          jaavgwriteUrms = 1
       endif 

    end if

    ! Map classes output (formerly: incremental file)
    ti_classmap_array = 0d0
    call prop_get_doubles(md_ptr, 'output', 'ClassMapInterval', ti_classmap_array, 3, success)
    if (ti_classmap_array(1) .gt. 0d0) ti_classmap_array(1) = max(ti_classmap_array(1) , dt_user)
    call getOutputTimeArrays(ti_classmap_array, ti_classmaps, ti_classmap, ti_classmape, success)

    if (ti_classmap > 0d0) then
       call prop_get_string(md_ptr, 'output', 'ClassMapFile', md_classmap_file, success)

       call readClasses('WaterlevelClasses', map_classes_s1)
       call readClasses('WaterdepthClasses', map_classes_hs)

       call readClasses('VelocityMagnitudeClasses', map_classes_ucmag)
       call prop_get_double(md_ptr, 'output', 'VelocityDirectionClassesInterval', map_classes_ucdirstep, success)
       if (success) then
          call createDirectionClasses(map_classes_ucdir, map_classes_ucdirstep)
       else
          allocate(map_classes_ucdir(0))
       endif

       if (size(map_classes_s1) == 0 .and. size(map_classes_hs) == 0 .and. size(map_classes_ucmag) == 0 .and. map_classes_ucdirstep < 0d0) then
          call mess(LEVEL_ERROR, 'ClassMapInterval given, but none of WaterlevelClasses, WaterdepthClasses, VelocityMagnitudeClasses, VelocityDirectionClassesInterval is defined.')
       endif

    endif

    call prop_get_double ( md_ptr, 'equatorial', 'Ampfreeleft'     , amm)
    call prop_get_double ( md_ptr, 'equatorial', 'Ampfreeright'    , app)
    call prop_get_double ( md_ptr, 'equatorial', 'Ampforcedzerofr' , ztyp)
    call prop_get_integer( md_ptr, 'equatorial', 'Nmode' , Nmode)
    call prop_get_integer( md_ptr, 'equatorial', 'Nfreq' , Nfreq)

   ! Create a separate property tree for structures.
   call tree_create('structures_'//trim(filename), strs_ptr)

   ! First, try to read from a single StructureFile.
   md_structurefile = ' '
   call prop_get_string(md_ptr, 'geometry', 'StructureFile', md_structurefile, success)
   if (len_trim(md_structurefile) > 0 &
      .and. index(md_structurefile, ';') == 0) then ! UNST-2854: workaround, as long as we don't support ;-separator in MDU, skip it here and leave it all to readStructures().
      call strsplit(md_structurefile,1,fnames,1)
      do ifil=1,size(fnames)
         ! TODO: UNST-2452: support multiple structurefile basedirs
         call split_filename(fnames(ifil), md_structurefile_dir, fnam) ! Remember base dir for this structures.ini file

         ! TODO: AvD: here we should actually filter out the [structure] blocks only, others may be in the file too
         call prop_file('ini',fnames(ifil),strs_ptr,istat)
         if (istat /= 0) then
            call mess(LEVEL_ERROR, 'Failed to read structure data from '''//trim(fnames(ifil))//''' not found. Code: ', istat)
         else
            call mess(LEVEL_INFO, 'Read Structure data from ', trim(fnames(ifil)))
         end if
      enddo
      deallocate(fnames)

      ! [debug]
      if (loglevel_StdOut <= LEVEL_DEBUG) then
         write (*,*) '** DEBUG  : This is the list of structures that will be processed:' ! no MessageHandling, because tree_traverse does not (yet) support it.
         call tree_traverse(strs_ptr, print_tree, dummychar, dummylog)
      end if
   end if

!  particles
   call prop_get_string (md_ptr, 'particles', 'ParticlesFile', md_partfile, success)
   call prop_get_string (md_ptr, 'particles', 'ParticlesReleaseFile', md_partrelfile, success)
   call prop_get_integer(md_ptr, 'particles', 'AddTracer', md_partjatracer, success)
   call prop_get_double (md_ptr, 'particles', 'StartTime', md_partstarttime, success)
   call prop_get_double (md_ptr, 'particles', 'TimeStep', md_parttimestep, success)
   call prop_get_integer(md_ptr, 'particles', '3Dtype', md_part3Dtype, success)

!  processes (WAQ)
   call prop_get_string (md_ptr, 'processes', 'SubstanceFile', md_subfile, success)
   call prop_get_string (md_ptr, 'processes', 'AdditionalHistoryOutputFile', md_ehofile, success)
   call prop_get_string (md_ptr, 'processes', 'StatisticsFile', md_sttfile, success)
   call prop_get_double (md_ptr, 'processes', 'ThetaVertical', md_thetav_waq, success)
   call prop_get_integer(md_ptr, 'processes', 'ProcessFluxIntegration', md_flux_int, success)
   call prop_get_integer(md_ptr, 'processes', 'Wriwaqbot3Doutput', md_wqbot3D_output, success)
   call prop_get_double (md_ptr, 'processes', 'VolumeDryThreshold', waq_vol_dry_thr)
   call prop_get_double (md_ptr, 'processes', 'DepthDryThreshold', waq_dep_dry_thr)

   call prop_get_double (md_ptr, 'processes', 'DtProcesses', md_dt_waqproc, success)
   ti_waqproc = md_dt_waqproc
   if (md_dt_waqproc > 0d0) then
      if(dt_user > 0d0 .and. md_dt_waqproc > 0d0) then
         if(md_dt_waqproc < dt_user .or. modulo(md_dt_waqproc, dt_user) /= 0d0) then
            ti_waqproc = max(1,floor(md_dt_waqproc/dt_user))*dt_user
            ! Processes timestep can only a multiple of dtuser...
            write(msgbuf, '(a,f9.3,a,f9.3,a)') 'DtProcesses should be a multiple of DtUser. It has been reset to: ', ti_waqproc, '(was: ',md_dt_waqproc,')'
            call mess(LEVEL_WARN, msgbuf)
         end if
      end if
   else if (md_dt_waqproc < 0d0) then
      ! DtProcesses is negative
      call mess(LEVEL_INFO, 'DtProcesses is negative. Water quality processes are calculated with every hydrodynamic time step.')
   end if

   call prop_get_double (md_ptr, 'processes', 'DtMassBalance', md_dt_waqbal, success)
   if(md_dt_waqbal > 0d0) then
      call mess(LEVEL_WARN, 'The keyword DtMassBalance in the Processes section is now replaced by MbaInterval in the Output section.')
      if (ti_mba > 0d0) then
         call mess(LEVEL_WARN, 'MbaInterval was already specified, will ignore DtMassBalance.')
      else
         call mess(LEVEL_WARN, 'MbaInterval was not specified, will use DtMassBalance for backwards compatibility.')
         ti_mba = md_dt_waqbal
      endif
   endif
   if(ti_mba > 0d0 .and. md_dt_waqproc > 0d0) then
      if(ti_mba < md_dt_waqproc .or. modulo(ti_mba, md_dt_waqproc) /= 0d0) then
         ti_mba = max(1,floor(ti_mba/md_dt_waqproc))*dt_user
         ! dtprocesses can only a multiple of Processes timestep (when processes are on)...
         write(msgbuf, '(a,f9.3,a,f9.3,a)') 'MbaInterval should be a multiple of DtProcesses when WQ processes are on. It has been reset to: ', ti_mba, '(was: ',md_dt_waqbal,')'
         call mess(LEVEL_WARN, msgbuf)
      end if
   end if

   ! Some combined validation checks of model settings:
   if (len_trim(md_restartfile)>0 .and. Tspinupturblogprof>0d0) then
      write (msgbuf, '(a,f9.3,a)') 'You start from a restartfile and also have a non-zero Tspinupturblogprof'
      call warn_flush()
   endif

   if (len_trim(md_restartfile)>0 .and. Tlfsmo>0d0) then
      write (msgbuf, '(a,g15.9,a)') 'MDU settings combine a restart file and a smoothing time: Tlfsmo = ',Tlfsmo, '. This is no longer allowed. Tlfsmo is set to 0.0.'
      call warn_flush()
      Tlfsmo = 0d0
   endif

   if (len_trim(md_crsfile) == 0 .and. jashp_crs > 0) then
      write (msgbuf, '(a,I1,a)') 'MDU settings does not specify any cross section: CrsFile =  , but sets to write a shape file' &
         //' for cross sections: Wrishp_crs = ', jashp_crs, '. In this situation, no such shape file is written.'
      call warn_flush()
      jashp_crs = 0
   endif

   if (len_trim(md_obsfile) == 0 .and. jashp_obs > 0) then
      write (msgbuf, '(a,I1,a)') 'MDU settings does not specify any observation station: ObsFile =  , but sets to write a ' &
         //'shape file for observation stations: Wrishp_crs = ', jashp_obs, '. In this situation, no such shape file is written.'
      call warn_flush()
      jashp_obs = 0
   endif

   if (len_trim(md_structurefile) == 0 .and. (jashp_weir > 0 .or. jashp_gate > 0 .or. jashp_genstruc > 0 )) then
      write (msgbuf, '(a)') 'MDU settings does not specify any structure: StructureFile =  , but sets to write a ' &
         //'shape file for structures (Wrishp_weir/Wrishp_gate/Wrishp_genstruc = 1). In this situation, no such shape file is written.'
      call warn_flush()
      jashp_weir = 0
      jashp_gate = 0
   endif

   if (len_trim(md_thdfile) == 0 .and. jashp_thd > 0) then
      write (msgbuf, '(a,I1,a)') 'MDU settings do not specify any thin dam: ThinDamFile =  , but sets to write a ' &
         //'shape file for thin dams: Wrishp_thindam = ', jashp_thd, '. In this situation, no such shape file is written.'
      call warn_flush()
      jashp_thd = 0
   endif

   if (len_trim(md_fixedweirfile) == 0 .and. jashp_fxw > 0) then
      write (msgbuf, '(a,I1,a)') 'MDU settings does not specify any fixed weir: FixedWeirFile =  , but sets to write a ' &
         //'shape file for fixed weirs: Wrishp_fixed_weir = ', jashp_fxw, '. In this situation, no such shape file is written.'
      call warn_flush()
      jashp_fxw = 0
   endif

   ! Switch on rst boundaries if jawave==3 and restartinterval>0
   ! For now here, and temporarily commented
   !if (jarstbnd==0 .and. jawave==3 .and. ti_rst>eps10) then
   !   write (msgbuf, '(a)') 'MDU settings specify that writing restart files is requested, and switch off writing boundary data to the restart file. ' &
   !      //'A coupling with SWAN is specified as well. To correctly restart a SWAN+FM model, boundary restart data are required. Switching Wrirst_bnd to 1.'
   !   call warn_flush()
   !   jarstbnd=1
   !endif

   
   if (jagui == 0) then 
      ! If obsolete entries are used in the mdu-file, return with that error code.
      call final_check_of_mdu_keywords (md_ptr, ierror, prefix='While reading '''//trim(filename)//'''')
      if (ierror /= DFM_NOERR) then
         istat = ierror
      endif
   endif
end subroutine readMDUFile

!> helper routine to read the class boundaries
subroutine readClasses(className, incr_classes)
    character(len=*),           intent(in)  :: className       !< class name
    real(kind=hp), allocatable, intent(out) :: incr_classes(:) !< output array

    integer                    :: i
    integer                    :: j
    logical                    :: success
    integer, parameter         :: maxClasses = 99
    real(kind=hp), allocatable :: reals(:)

    allocate(reals(maxClasses))
    reals = huge(reals)
    call prop_get_doubles(md_ptr, 'output', className, reals, maxClasses, success)
    if (success) then
        j = maxClasses+1
        do i = 1, maxClasses
            if (reals(i) == huge(reals)) then
                j = i
                exit
            endif
        enddo
        allocate(incr_classes(j-1))
        do i = 1, j-1
            incr_classes(i) = reals(i)
        enddo
    else
        allocate(incr_classes(0))
    endif
    deallocate(reals)
end subroutine readClasses

!> helper routine to convert step size to the class boundaries
!! give error if step size is not valid (e.g. negative, > 360, number of steps is not an integer)
subroutine createDirectionClasses(map_classes_ucdir, map_classes_ucdirstep)
   use MessageHandling, only : mess, LEVEL_FATAL
   use m_alloc, only : aerr
   double precision, allocatable, intent(inout) :: map_classes_ucdir(:)   !< the constructed classes
   double precision,              intent(in)    :: map_classes_ucdirstep  !< the input step size

   double precision, parameter :: wholeCircle = 360d0
   integer :: n  !< number of classes
   integer :: i, ierr

   if (map_classes_ucdirstep <= 0d0 .or. map_classes_ucdirstep > wholeCircle) then
      call mess(LEVEL_FATAL, 'Step size for classes out of range; must be > 0 and <= 360; found: ', map_classes_ucdirstep)
   end if

   n = nint(wholeCircle / map_classes_ucdirstep)
   if (comparereal(wholeCircle, n * map_classes_ucdirstep) /= 0) then
      call mess(LEVEL_FATAL, 'Step size for classes must meet: 360 / step size gives an integer')
   else if (n > 127) then
      call mess(LEVEL_FATAL, 'Step size too small. To many classes to fit in a signed byte')
   end if

   allocate(map_classes_ucdir(n - 1), stat = ierr)
   if (ierr /= 0) call aerr( 'map_classes_ucdir', ierr, n+1)
   do i = 1, n - 1
      map_classes_ucdir(i) = dble(i) * map_classes_ucdirstep
   enddo
end subroutine createDirectionClasses

!> Check if a keyword is deprecated (but still supported).
logical function isdeprecated(chap, key)
   character(len=*)                           :: chap  !< chapter name
   character(len=*)                           :: key   !< keyword name
   
   isdeprecated = .false.

   ! NOTE: make sure to have all chapter names and key names all in lowercase below:
   select case (trim(chap))
   case ('processes')
      select case (trim(key))
      case ('dtmassbalance')
         isdeprecated = .true.
      end select
   end select
end function isdeprecated

!> Check if a keyword is obsolete (removed and hence no longer supported).
logical function isobsolete(chap, key)
   character(len=*)                           :: chap  !< chapter name
   character(len=*)                           :: key   !< keyword name
   
   isobsolete = .false.
   
   ! NOTE: make sure to have all chapter names and key names all in lowercase below:
   select case (trim(chap))
   case ('geometry')
      select case (trim(key))
      case ('bathymetryfile','bedlevelfile','botlevuni','botlevtype','ithindykescheme','manholefile','nooptimizedpolygon')
         isobsolete = .true.
      end select
   case ('numerics')
      select case (trim(key))
      case ('hkad','ithindykescheme','thindykecontraction', 'transportmethod', 'transporttimestepping')
         isobsolete = .true.
      end select
   case ('output')
      select case (trim(key))
      case ('writebalancefile')
         isobsolete = .true.
      end select
   end select
end function isobsolete
    
!> Present a list of all MDU entries (tree struct) that were not read D-Flow FM or read more than once.
!> Show errors if obsolete (removed) keywords are used and stop.
!> Show warnings if deprecated (but not yet removed) keywords are used.
subroutine final_check_of_mdu_keywords(md_tree, istat, prefix)
   use MessageHandling
   use dfm_error
   
   implicit none
   type (TREE_DATA), pointer      :: md_tree           !< MDU-tree
   integer, intent(out)           :: istat             !< Results status (DFM_NOERR if no ignored entries)
   character(len=*), optional     :: prefix            !< Optional message string prefix, default empty

   type (TREE_DATA), pointer                      :: achapter                !< tree data pointer for chapter level
   type (TREE_DATA), pointer                      :: anode                   !< tree data pointer for keyword level
   integer                                        :: inode                   !< index of the keyword being processed
   integer                                        :: nnode                   !< number of keywords in the chapter
   integer                                        :: ichapter                !< index of the chapter being processed
   integer                                        :: nchapter                !< number of chapters in the file
   integer                                        :: i                       !< loop variable
   integer, parameter                             :: strlen = 30             !< maximum length of chapter and keyword/node names
   character(len=strlen)                          :: nodename                !< name of the keyword
   character(len=strlen)                          :: chaptername             !< name of the chapter
   character(len=5)                               :: node_visit_str          !< temporary string containing the number of times a keyword was accessed
   character(len=100)                             :: nodestring              !< string containing the keyword value
   logical                                        :: ismatch                 !< flag indicating whether the chapter/keyword matches a certain condition
   integer                                        :: threshold_abort_current !< backup variable for default abort threshold level (temporarily overruled)
   logical                                        :: success                 !< flag indicating successful completion of a call
   integer                                        :: num_obsolete            !< count the number of obsolete (removed) keywords
   integer                                        :: num_deprecated          !< count the number of deprecated keywords
   integer                                        :: num_doubleaccess        !< count the number of keywords accessed multiple times
   integer                                        :: num_notaccessed         !< count the number of keywords not accessed at all (not being obsolete keywords)
   integer, parameter                             :: numignore = 1           !< number of ignored chapters
   character(len=strlen), dimension(numignore)    :: ignorechaps             !< which chapters to skip while checking

   istat = DFM_NOERR
   ignorechaps(:) = ' '
   ignorechaps(1) = 'model'

   num_obsolete = 0     ! support for these keywords has been removed from the code
   num_deprecated = 0   ! keywords still supported, but may be removed in upcoming release
   num_doubleaccess = 0 ! supported keywords, read multiple times (possibly inefficient code)
   num_notaccessed = 0  ! keywords not recognized by this version, possible typo by the user

   threshold_abort_current = threshold_abort
   threshold_abort = LEVEL_FATAL

   nchapter = size(md_tree%child_nodes)
ch:do ichapter = 1, nchapter
      achapter => md_tree%child_nodes(ichapter)%node_ptr
      chaptername = tree_get_name(achapter)
      do i = 1,numignore
         if (trim(ignorechaps(i)) == trim(chaptername)) then
            cycle ch
         end if
      end do

      if (associated(achapter%child_nodes)) then
         nnode = size(achapter%child_nodes)
      else
         nnode = 0
      endif

      do inode = 1, nnode
         anode => achapter%child_nodes(inode)%node_ptr
         call tree_get_data_string(anode,nodestring,success)
         nodename = tree_get_name(anode)
         if (success) then
            if (size(anode%node_data) > 0) then
               if (anode%node_visit < 1) then
                  ! report any unused keywords
                  if (isobsolete(trim(chaptername), trim(nodename))) then
                      ! keyword is known, but no longer supported
                      num_obsolete = num_obsolete + 1
                      call mess(LEVEL_ERROR, prefix//': keyword ['//trim(chaptername)//'] '//trim(nodename)//' is no longer supported.')
                  else
                      ! keyword unknown, or known keyword that was not accessed because of the reading was switched off by the value of another keyword
                      num_notaccessed = num_notaccessed + 1
                      call mess(LEVEL_WARN, prefix//': keyword ['//trim(chaptername)//'] '//trim(nodename)//'='//trim(nodestring)//' was in file, but not used. Check possible typo.')
                  endif
               else
                   ! keyword is known and used (node_visit >= 1)
                  
                  if (isdeprecated(trim(chaptername), trim(nodename))) then
                      ! keyword is used, but deprecated
                      num_deprecated = num_deprecated + 1
                      call mess(LEVEL_WARN, prefix//': keyword ['//trim(chaptername)//'] '//trim(nodename)//' is deprecated and may be removed in a future release.')
                  endif
                  if (anode%node_visit > 1) then
                     write(node_visit_str,'(i0)') anode%node_visit
                     num_doubleaccess = num_doubleaccess + 1
!                    call mess(LEVEL_WARN, prefix//': ['//trim(chaptername)//'] '//trim(nodename)//'='//trim(nodestring)//' was accessed more than once ('//trim(node_visit_str)//' times). Please contact support.')
                  endif
               endif
            endif
         endif
      enddo
   enddo ch

   if (num_deprecated > 0) then
      ! to bring this message to the attention of the user, we write an error but continue ..
      ! but set it to warning because it leads to a stop futher on in loadModel when the highest message level is checked...
      call mess(LEVEL_WARN, prefix//': Deprecated keywords used: Check Section "Overview of deprecated and removed keywords" in the User Manual for information on how to update the input file.')
   end if

   threshold_abort = threshold_abort_current

   if (num_obsolete > 0) then
      ! this is a fatal error that we want to stop at. 
      call mess(LEVEL_ERROR, prefix//': Old unsupported keywords used: Check Section "Overview of deprecated and removed keywords" in the User Manual for information on how to update the input file.')
      istat = DFM_WRONGINPUT
   end if

end subroutine final_check_of_mdu_keywords


!> Write a model definition to a file.
subroutine writeMDUFile(filename, istat)
    character(*), intent(inout) :: filename !< Name of file to be read (in current directory or with full path).
    integer,      intent(  out) :: istat !< Return status (0=success)

    integer :: mout

    call newfil(mout, filename)
    !call writeMDUFilepointer(mout, .true., istat)
    call writeMDUFilepointer(mout, .false., istat)
    call doclose(mout)
    call setmd_ident(filename)
    call switch_dia_file() ! Used to be in setmd_ident, now separate.

end subroutine writeMDUFile


!> Write a model definition to a file pointer
subroutine writeMDUFilepointer(mout, writeall, istat)
    use m_flow ! ,                !  only : kmx, layertype, mxlayz, sigmagrowthfactor, numtopsig, &
                                  !         Iturbulencemodel, spirbeta, dztopuniabovez, dztop, jahazlayer, Floorlevtoplay ,  &
                                  !         fixedweirtopwidth, fixedweirtopfrictcoef, fixedweirtalud, ifxedweirfrictscheme,         &
                                  !         Tsigma, jarhoxu, iStrchType, STRCH_USER, STRCH_EXPONENT, STRCH_FIXLEVEL, laycof
    use m_flowgeom ! ,              only : wu1Duni, Bamin, rrtol, jarenumber, VillemonteCD1, VillemonteCD2
    use m_flowtimes
    use m_flowparameters
    use m_wind
    use network_data,            only : zkuni, Dcenterinside, removesmalllinkstrsh, cosphiutrsh
    use m_sferic,                only : anglat, anglon, jsferic, jasfer3D
    use m_physcoef
    use unstruc_netcdf, only: unc_writeopts, UG_WRITE_LATLON, UG_WRITE_NOOPTS, unc_nounlimited, unc_noforcedflush, unc_uuidgen, unc_metadatafile
    use dflowfm_version_module
    use m_equatorial
    use m_sediment
    use m_netw,                  only : Makeorthocenters
    use m_fixedweirs
    use m_reduce,                only : maxdge
    use m_grw
    use m_missing
    use m_heatfluxes
    use m_fm_wq_processes
    use m_trachy
    use m_transport, only: ITRA1
    use m_structures, only: jahiscgen, jahiscdam, jahispump, jahisgate, jahisweir, jahisorif, jahisbridge, jahisculv, jahisdambreak, jahisuniweir, jahiscmpstru, jahislongculv
    use m_sobekdfm,              only : sbkdfm_umin, sbkdfm_umin_method, minimal_1d2d_embankment, sbkdfm_relax
    use m_subsidence, only: sdu_update_s1
    use m_xbeach_data, only: swave
    use unstruc_channel_flow
    use m_sedtrails_data

    integer, intent(in)  :: mout  !< File pointer where to write to.
    logical, intent(in)  :: writeall !< Write all fields, including default values
    integer, intent(out) :: istat !< Return status (0=success)

    type(tree_data),  pointer      :: prop_ptr
    ! TODO: [TRUNKMERGE] WO: I prefer having tfac not in m_flowtimes, but back here again.
    character(len=20)              :: rundat
    character(len=128)             :: helptxt
    character(len=256)             :: tmpstr
    integer                        :: i, ibuf, help
    real(kind=hp)                  :: ti_wav_array(3), ti_map_array(3), ti_rst_array(3), ti_his_array(3), ti_waq_array(3), ti_classmap_array(3), ti_st_array(3)

    logical, external              :: get_japart
    istat = 0 ! Success

! Put settings for .mdu file into a property tree first
    call tree_create(trim(md_ident), prop_ptr)
    call prop_set(prop_ptr, 'General', 'Program',   product_name,     'Program')
    call prop_set(prop_ptr, 'General', 'Version',   version_full,     'Version number of computational kernel')
    call prop_set(prop_ptr, 'General', 'fileType',  'modelDef',          'File type. Do not edit this.')
    tmpstr = ''
    write(tmpstr, '(i0,".",i2.2)') MDUFormatMajorVersion, MDUFormatMinorVersion
    call prop_set(prop_ptr, 'General', 'fileVersion', trim(tmpstr),      'File format version (do not edit this)')
    call prop_set(prop_ptr, 'General', 'AutoStart', md_jaAutoStart,      'Autostart simulation after loading MDU (0: no, 1: autostart, 2: autostartstop)')
    call prop_set(prop_ptr, 'General', 'InputSpecific', md_jaAutoStart,  'Use of hardcoded specific inputs, shall not be used by users (0: no, 1: yes)')
    call prop_set(prop_ptr, 'General', 'ModelSpecific',  md_specific,    'Optional ''model specific ID'', to enable certain custom runtime function calls (instead of via MDU name).')
    call prop_set(prop_ptr, 'General', 'PathsRelativeToParent',  md_paths_relto_parent, 'Default: 0. Whether or not (1/0) to resolve file names (e.g. inside the *.ext file) relative to their direct parent, instead of to the toplevel MDU working dir.')


! Geometry
    call prop_set(prop_ptr, 'geometry', 'NetFile',          trim(md_netfile),       'Unstructured grid file *_net.nc')
    call prop_set(prop_ptr, 'geometry', 'GridEnclosureFile',trim(md_encfile),       'Enclosure file to clip outer parts from the grid *.pol')
    call prop_set(prop_ptr, 'geometry', 'DryPointsFile',    trim(md_dryptsfile),    'Dry points file *.xyz (third column dummy z values), or dry areas polygon file *.pol (third column 1/-1: inside/outside)')

    ! 1D network related input
    if (writeall .or. len_trim(md_1dfiles%cross_section_definitions) > 0) then
       call prop_set(prop_ptr, 'geometry', 'CrossDefFile',     trim(md_1dfiles%cross_section_definitions), '1D Cross section definition file (*.ini)')
    end if
    if (writeall .or. len_trim(md_1dfiles%cross_section_locations) > 0) then
       call prop_set(prop_ptr, 'geometry', 'CrossLocFile',     trim(md_1dfiles%cross_section_locations),   '1D Cross section location file (*.ini)')
    end if
    if (writeall .or. len_trim(md_1dfiles%storage_nodes) > 0) then
       call prop_set(prop_ptr, 'geometry', 'StorageNodeFile',         trim(md_1dfiles%storage_nodes),          '1D Storage node/manhole file (*.ini)')
    end if
    if (writeall .or. len_trim(md_1dfiles%roughness) > 0) then
       call prop_set(prop_ptr, 'geometry', 'frictFile',   trim(md_1dfiles%roughness),                      '1D Roughness files (*.ini)')
    end if
    if (writeall .or. len_trim(md_1dfiles%structures) > 0) then
       call prop_set(prop_ptr, 'geometry', 'StructureFile',    trim(md_1dfiles%structures),                'Hydraulic structure file (*.ini)')
    else if (writeall .or. len_trim(md_structurefile) > 0) then
       ! pending code merge, structure file either applies to v2.00 structure file, or the old one.
       call prop_set(prop_ptr,'geometry', 'StructureFile',    trim(md_structurefile), 'File *.ini containing list of structures (pumps, weirs, gates and general structures)')
    end if
    call prop_set(prop_ptr, 'geometry', 'WaterLevIniFile',  trim(md_s1inifile),     'Initial water levels sample file *.xyz using flood fill algorithm')
    call prop_set(prop_ptr, 'geometry', 'LandBoundaryFile', trim(md_ldbfile),       'Land boundaries file *.ldb, used for visualization')
    call prop_set(prop_ptr, 'geometry', 'ThinDamFile',      trim(md_thdfile),       'Polyline file *_thd.pli, containing thin dams')
    call prop_set(prop_ptr, 'geometry', 'Cutcelllist',      trim(md_Cutcelllist),   'File with names of cutcell polygons, e.g. cutcellpolygons.lst')
    call prop_set(prop_ptr, 'geometry', 'FixedWeirFile',    trim(md_fixedweirfile), 'Polyline file *_fxw.pliz, containing fixed weirs with rows x, y, crest level, left ground level, right ground level')
    call prop_set(prop_ptr, 'geometry', 'PillarFile',       trim(md_pillarfile),    'Polyline file *_pillar.pliz, containing four colums with x, y, diameter and Cd coefficient')
    call prop_set(prop_ptr, 'geometry', 'Gulliesfile',      trim(md_gulliesfile), 'Polyline file *_gul.pliz, containing lowest bed level along talweg x, y, z level')
    call prop_set(prop_ptr, 'geometry', 'Roofsfile',        trim(md_roofsfile),   'Polyline file *_rof.pliz, containing roofgutter heights x, y, z level')
    call prop_set(prop_ptr, 'geometry', 'VertplizFile',     trim(md_vertplizfile),  'Vertical layering file *_vlay.pliz with rows x, y, Z, first Z, nr of layers, second Z, layer type')
    call prop_set(prop_ptr, 'geometry', 'ProflocFile',      trim(md_proflocfile),   'Channel profile location file *_proflocation.xyz with rows x, y, z, profile number ref'  )
    call prop_set(prop_ptr, 'geometry', 'ProfdefFile',      trim(md_profdeffile),   'Channel profile definition file *_profdefinition.def with definition for all profile numbers'  )
    call prop_set(prop_ptr, 'geometry', 'ProfdefxyzFile',   trim(md_profdefxyzfile),'Channel profile definition file _profdefinition.def with definition for all profile numbers'  )
    call prop_set(prop_ptr, 'geometry', 'IniFieldFile',     trim(md_inifieldfile),  'Initial values and parameter fields file'  )
    call prop_set(prop_ptr, 'geometry', 'UseCaching',       md_usecaching,          'Use caching for geometrical/network-related items (0: no, 1: yes)')


    call prop_set(prop_ptr, 'geometry', 'Uniformwidth1D',   wu1Duni,                'Uniform width for channel profiles not specified by profloc')
    if (writeall .or. hh1Duni .ne. 3d3) then
    call prop_set(prop_ptr, 'geometry', 'Uniformheight1D',  hh1Duni,                'Uniform height for channel profiles not specified by profloc')
    endif
    if (writeall .or. abs(iproftypuni) .ne. 3) then
    call prop_set(prop_ptr, 'geometry', 'Uniformtyp1D',  iproftypuni,               'Uniform type for channel profiles not specified by profloc')
    endif

    if (writeall .or. wu1Duni5 .ne. 0.2d0) then
    call prop_set(prop_ptr, 'geometry', 'Uniformwidth1Dstreetinlets',  wu1Duni5,   'Uniform width for street inlets')
    endif
    if (writeall .or. hh1Duni5 .ne. 0.1d0) then
    call prop_set(prop_ptr, 'geometry', 'Uniformheight1Dstreetinlets', hh1Duni5,   'Uniform height for street inlets')
    endif
    if (writeall .or. abs(iproftypuni5) .ne. -2) then
    call prop_set(prop_ptr, 'geometry', 'Uniformtyp1Dstreetinlets',  iproftypuni5, 'Uniform type street inlets')
    endif

    if (writeall .or. wu1Duni7 .ne. 0.1d0) then
    call prop_set(prop_ptr, 'geometry', 'Uniformwidth1Droofgutterpipes',  wu1Duni7,   'Uniform width for roof gutter pipes')
    endif
    if (writeall .or. hh1Duni7 .ne. 0.1d0) then
    call prop_set(prop_ptr, 'geometry', 'Uniformheight1Droofgutterpipes', hh1Duni7,   'Uniform height for roof gutter pipes')
    endif
    if (writeall .or. abs(iproftypuni7) .ne. -2) then
    call prop_set(prop_ptr, 'geometry', 'Uniformtyp1Droofgutterpipes',  iproftypuni7, 'Uniform type roof gutter pipes')
    endif
    if (writeall .or. len_trim(md_1d2dlinkfile) > 0) then
       call prop_set(prop_ptr, 'geometry', '1D2DLinkFile', trim(md_1d2dlinkfile), 'File *.ini containing custom parameters for 1D2D links'  )
    endif

    if (writeall .or. dxmin1D .ne. 1d-3) then
    call prop_set(prop_ptr, 'geometry', 'Dxmin1D',          Dxmin1D,                'Minimum 1D link length, (except for duikers) ')
    endif
    call prop_set( prop_ptr, 'geometry', 'Dxwuimin2D'    , Dxwuimin2D,             'Smallest fraction dx/wu , set dx > Dxwuimin2D*wu, Default = 0.1' )

    if (writeall .or. removesmalllinkstrsh .ne. 1d-1) then
    call prop_set(prop_ptr, 'geometry', 'Removesmalllinkstrsh',   removesmalllinkstrsh,  '0-1, 0= no removes')
    endif
    if (writeall .or. cosphiutrsh .ne. 5d-1) then
    call prop_set(prop_ptr, 'geometry', 'Cosphiutrsh',   cosphiutrsh,  '0-1, 1= no bad orthos')
endif

    if (writeall .or. ja1D2Dinternallinktype .ne. 1) then
       call prop_set(prop_ptr, 'geometry', '1D2Dinternallinktype', ja1D2Dinternallinktype, 'Link treatment method for type-3 internal links.')
    endif

    if ( len(md_pipefile) > 1) then
       call prop_set(prop_ptr, 'geometry', 'PipeFile',      trim(md_pipefile),      'File *.pliz containing pipe-based ''culverts'''  )
    endif

    if ( len(md_shipdeffile) > 1) then
       call prop_set(prop_ptr, 'geometry', 'ShipdefFile',   trim(md_shipdeffile),   'File *.shd containing ship definitions'  )
    endif

    call prop_set(prop_ptr, 'geometry', 'WaterLevIni',      sini,                   'Initial water levels sample file *.xyz using flood fill algorithm')
    if (writeall .or. waterdepthini1D .ne. dmiss) then
       call prop_set(prop_ptr,'geometry','waterdepthini1D', waterdepthini1D,        'Initial waterdepth in 1D ')
    endif

    if (writeall .or. uniformhu .ne. dmiss) then
       call prop_set(prop_ptr,'geometry','Uniformhu',       uniformhu,              'Waterdepth in rigid-lid-like solution')
    endif

    call prop_set(prop_ptr, 'geometry', 'BedlevUni'  ,      zkuni,                  'Uniform bed level used at missing z values if BedlevType > 2')
    if (writeall .or. bedslope .ne. 0d0) then
       call prop_set(prop_ptr, 'geometry', 'Bedslope',      bedslope,               'Bed slope inclination if BedlevType > 2')
    endif
    if (writeall .or. bedwaveamplitude .ne. 0d0) then
       call prop_set(prop_ptr, 'geometry', 'Bedwaveamplitude', bedwaveamplitude,    'Bed testcases')
    endif
    if (writeall .or. bedwavelength .ne. 0d0) then
       call prop_set(prop_ptr, 'geometry', 'Bedwavelength',    bedwavelength,       'Bed testcases')
    endif

    if (ibedlevmode /= BLMODE_DFM) then
       call prop_set(prop_ptr,'geometry', 'BedlevMode',     ibedlevmode, '1: Compute bed levels according to BedLevType, i.e., from vel. points or tiles.')
       call prop_set(prop_ptr,'geometry', ''          ,     ''         , '2: Compute bed levels as D3D, i.e., from corner point''s depth zk value (equiv. DPSOPT=MAX)')
    end if

    call prop_set(prop_ptr, 'geometry', 'BedlevType',       ibedlevtyp, 'Bathymetry specification')
    call prop_set(prop_ptr, 'geometry', ''          ,       ''        , '1: at cell centers (from Net/IniFieldFile)')
    call prop_set(prop_ptr, 'geometry', ''          ,       ''        , '2: at faces (from IniFieldFile)')
    call prop_set(prop_ptr, 'geometry', ''          ,       ''        , '3: at nodes, face levels mean of node values')
    call prop_set(prop_ptr, 'geometry', ''          ,       ''        , '4: at nodes, face levels min. of node values')
    call prop_set(prop_ptr, 'geometry', ''          ,       ''        , '5: at nodes, face levels max. of node values')
    call prop_set(prop_ptr, 'geometry', ''          ,       ''        , '6: at nodes, face levels max. of cell-center values')

    if (writeall .or. (blmeanbelow .ne. -999d0)) then
    call prop_set(prop_ptr, 'geometry', 'Blmeanbelow' , blmeanbelow, 'If not -999d0, below this level the cell center bed level is the mean of surrouding net nodes')
    call prop_set(prop_ptr, 'geometry', 'Blminabove'  , blminabove , 'If not -999d0, above this level the cell center bed level is the min. of surrouding net nodes')
    endif
    if (writeall .or. grounlayuni > 0d0) then
       call prop_set(prop_ptr, 'geometry', 'Groundlayerthickness' , grounlayuni, 'Only in pipes: groundlayer thickness (m) ')
    endif

    call prop_set(prop_ptr, 'geometry', 'PartitionFile',    trim(md_partitionfile), 'Domain partition polygon file *_part.pol for parallel run')

    call prop_set(prop_ptr, 'geometry', 'AngLat',  anglat,  'Angle of latitude S-N (deg), 0: no Coriolis')
    call prop_set(prop_ptr, 'geometry', 'AngLon',  anglon,  'Angle of longitude E-W (deg), 0: Greenwich, used in solar heat flux computation.')

    if (writeall .or. jahelmert > 0) then
       call prop_set(prop_ptr, 'geometry', 'Helmert',  jaHelmert,  'HELMERT yes/no, 1/0')
    endif

    call prop_set(prop_ptr, 'geometry', 'Conveyance2D', Jaconveyance2D, '-1: R=HU,0: R=H, 1: R=A/P, 2: K=analytic-1D conv, 3: K=analytic-2D conv')
    if (jaconveyance3D .ne. 0) then 
    call prop_set(prop_ptr, 'geometry', 'Conveyance3D', Jaconveyance3D, '-1: R=HU,0: R=H, 1: R=A/P, 2: K=analytic-1D conv, 3: K=analytic-2D conv')
    endif 
    if (writeall .or. nonlin2D .ne. 0) then
    call prop_set(prop_ptr, 'geometry', 'Nonlin2D', Nonlin2D, 'Non-linear 2D volumes, 1 = yes, only used if ibedlevtype=3 and Conveyance2D>=1')
    endif
    if (writeall .or. nonlin1D .ne. 0) then
    call prop_set(prop_ptr, 'geometry', 'Nonlin1D', Nonlin1D, 'Non-linear 1D volumes, 1 = Preisman slot, 2 = pipes closed (Nested Newton)')
    endif

    if (writeall .or. Slotw2D .ne. 1d-3) then
       call prop_set(prop_ptr, 'geometry', 'Slotw2D', Slotw2D, '-')
    endif
    if (writeall .or. Slotw1D .ne. 1d-3) then
       call prop_set(prop_ptr, 'geometry', 'Slotw1D', Slotw1D, '-')
    endif

    if (writeall .or. len_trim(md_fixedweirfile) > 0 ) then
       call prop_set(prop_ptr, 'geometry', 'Sillheightmin', Sillheightmin, 'Weir treatment only if both sills larger than this value (m)')
    endif

    if (writeall .or. (Makeorthocenters > 0)) then
       call prop_set(prop_ptr, 'geometry', 'Makeorthocenters', Makeorthocenters, 'Switch from circumcentres to orthocentres in geominit (i>=1: number of iterations, 0: do not use)')
    endif
    if (writeall .or. (Dcenterinside .ne. 1d0)) then
    call prop_set(prop_ptr, 'geometry', 'Dcenterinside', Dcenterinside, 'Limit cell center (1.0: in cell, 0.0: on c/g)')
    endif
    if (writeall .or. (bamin > 1d-6)) then
       call prop_set(prop_ptr, 'geometry', 'Bamin', Bamin, 'Minimum grid cell area, in combination with cut cells')
    endif
    if (writeall .or. (rrtol .ne. 3d0)) then !
       call prop_set(prop_ptr, 'geometry', 'OpenBoundaryTolerance',  rrtol, 'Search tolerance factor between boundary polyline and grid cells, in cell size units')
    endif
    if (writeall .or.  jaAllowBndAtBifurcation > 0) then
       call prop_set(prop_ptr, 'geometry', 'AllowBndAtBifurcation',  jaAllowBndAtBifurcation, 'Allow 1d boundary node when connecting branch leads to bifurcation (1: yes, 0: no)')
    endif
    if (writeall .or.  md_jamake1d2dlinks > 0) then
       call prop_set(prop_ptr, 'geometry', 'CreateLinks1D2D',  md_jamake1d2dlinks, 'Ruecksichtslos create links between 1D nodes and 2D cells when initializing model (1: yes, 0: no)')
    endif
    if (writeall) then
       call prop_set (prop_ptr, 'geometry', 'RenumberFlowNodes',  jarenumber, 'Renumber the flow nodes (1: yes, 0: no)') ! hidden option for testing renumbering
    end if
    if (writeall .or. .not. dxDoubleAt1DEndNodes) then
       call prop_set (prop_ptr, 'geometry', 'dxDoubleAt1DEndNodes',  merge(1, 0, dxDoubleAt1DEndNodes), 'Extend 1D end nodes by 0.5 dx (1: yes, 0: no).')
    endif
    if (writeall .or. changeVelocityAtStructures) then
      call prop_set( prop_ptr, 'geometry', 'ChangeVelocityAtStructures', merge(1, 0, changeVelocityAtStructures), 'Change the flow velocity at structures in the advection calculation')
    end if
    if (writeall .or. .not. changeStructureDimensions) then
       call prop_set( prop_ptr, 'geometry', 'ChangeStructureDimensions', merge(1, 0, changeStructureDimensions), 'Change the structure dimensions of (universal) weirs, orifices, bridges and general structures in case these dimensions exceed the dimensions of the channel')
    end if
    if (writeall .or. (kmx > 0)) then
       call prop_set(prop_ptr, 'geometry', 'Kmx' ,              kmx,               'Maximum number of vertical layers')
       call prop_set(prop_ptr, 'geometry', 'Layertype' ,        Layertype,         'Vertical layer type (1: all sigma, 2: all z, 3: use VertplizFile)')
       call prop_set(prop_ptr, 'geometry', 'Numtopsig' ,        Numtopsig,         'Number of sigma layers in top of z-layer model')
       if (writeall .or. janumtopsiguniform .ne. 1) then
           call prop_set(prop_ptr, 'geometry', 'Numtopsiguniform' , jaNumtopsiguniform,         'Number of sigma layers above z-layers in a z-sigma model')
       endif

       call prop_set(prop_ptr, 'geometry', 'SigmaGrowthFactor', sigmagrowthfactor, 'Layer thickness growth factor from bed up')
       if (writeall .or. dztop .ne. dmiss) then
          call prop_set(prop_ptr, 'geometry', 'Dztop', Dztop, 'Z-layer thickness of layers above level Dztopuniabovez')
       endif
       if (writeall .or. Toplayminthick .ne. 0.01d0) then
          call prop_set(prop_ptr, 'geometry', 'Toplayminthick', Toplayminthick, 'Minimum top layer thickness(m), only for Z-layers')
       endif

       call prop_set_integer( prop_ptr, 'geometry', 'StretchType'     , iStrchType, 'Type of layer stretching, 0 = uniform, 1 = user defined, 2 = fixed level double exponential')
       if( iStrchType == STRCH_USER ) then
          call prop_set( prop_ptr, 'geometry', 'StretchCoef'     , laycof(1:kmx), 'Layers thickness percentage' )
       else if( iStrchType == STRCH_EXPONENT ) then
          call prop_set( prop_ptr, 'geometry', 'StretchCoef'     , laycof(1:3), 'Interface percentage from bed, bottom layers growth fac, top layers growth fac')
       endif

       if (writeall .or. Floorlevtoplay .ne. dmiss) then
          call prop_set(prop_ptr, 'geometry', 'Floorlevtoplay', Floorlevtoplay, 'Floor level of top layer')
       endif

       if (writeall .or. jaorgFloorlevtoplaydef .ne. 0) then
          call prop_set(prop_ptr, 'geometry', 'OrgFloorlevtoplaydef', jaorgFloorlevtoplaydef, 'keep original definition of Floor level of top layer')
       endif

       if (writeall .or. zlaybot .ne. dmiss) then
          call prop_set(prop_ptr, 'geometry', 'ZlayBot', zlaybot, 'level of bottom layer in z-layers')
       endif

       if (writeall .or. zlaytop .ne. dmiss) then
          call prop_set(prop_ptr, 'geometry', 'ZlayTop', zlaytop, 'level of top layer in z-layers')
       endif

       if (writeall .or. dztopuniabovez .ne. dmiss) then
          call prop_set(prop_ptr, 'geometry', 'Dztopuniabovez', Dztopuniabovez,'Above this level layers will have uniform Dztop, below we use SigmaGrowthFactor')
       endif
       if (writeall .or. Tsigma .ne. 100d0) then
          call prop_set(prop_ptr, 'geometry', 'Tsigma', Tsigma ,'Sigma Adaptation period for Layertype==4 (s)')
       endif

       if (writeall .or. keepzlayeringatbed /= 2) then
          call prop_set(prop_ptr, 'geometry', 'Keepzlayeringatbed'  , keepzlayeringatbed, '0:possibly very thin layer at bed, 1:bedlayerthickness == zlayerthickness, 2=equal thickness first two layers')
       endif

       if (keepzlayeringatbed == 1) then
          call prop_set(prop_ptr, 'geometry', 'Keepzlay1bedvol'  , keepzlay1bedvol, 'only for keepzlayeringatbed=1: 1=correct bedcell volumes, 0=too large bedcell volumes') 
       endif

       if (writeall .or. ihuz .ne. 4) then
           call prop_set(prop_ptr, 'geometry', 'Ihuz'  , ihuz, 'if keepzlayeratbed>=2 : 1=central from bed til second, 2=all central, 3=from bed till highest equal levels')
       endif

       if (writeall .or. ihuzcsig .ne. 3) then
           call prop_set(prop_ptr, 'geometry', 'ihuzcsig'  , ihuzcsig, 'if keepzlayeratbed>=2 : 1,2,3=av,mx,mn of Leftsig,Rightsig,4=uniform')
       endif

       if (writeall .or. jaZlayeratubybob .ne. 0 .and. layertype .ne. 1) then
         call prop_set(prop_ptr, 'geometry', 'Zlayeratubybob', JaZlayeratubybob, 'Lowest connected cells governed by bob instead of by bL L/R' )
       endif
    endif

    ! 1D Volume tables
    if (writeall .or. useVolumeTables) then
       call prop_set (prop_ptr, 'volumeTables', 'useVolumeTables',  merge(1, 0, useVolumeTables), 'Use 1D volume tables (0: no, 1: yes).')
       if (useVolumeTables) then
          call prop_set(md_ptr, 'volumeTables', 'increment', tableIncrement, 'Desired increment for volume tables')
          call prop_set(md_ptr, 'volumeTables', 'useVolumeTableFile', merge(1, 0, useVolumeTableFile), 'Use volume table file (0: no, 1: yes)')
       endif
    endif

! Numerics
    call prop_set(prop_ptr, 'numerics', 'CFLMax',       cflmx,      'Maximum Courant number')
    !call prop_set(prop_ptr, 'numerics', 'CFLWaveFrac',  cflw,       'Wave velocity fraction, total courant vel = u + cflw*wavevelocity')
    if (writeall .or.  Lincontin .ne. 0) then
       call prop_set(prop_ptr, 'numerics', 'Lincontin',    Lincontin,  'Default 0; Set to 1 for linearizing d(Hu)/dx; link to AdvecType')
    endif
    call prop_set(prop_ptr, 'numerics', 'AdvecType',    iadvec,     'Advection type (0: none, 1: Wenneker, 2: Wenneker q(uio-u), 3: Perot q(uio-u), 4: Perot q(ui-u), 5: Perot q(ui-u) without itself), 33: as 3 using links (faster)')
    call prop_set(prop_ptr, 'numerics', 'AdvecCorrection1D2D', iadveccorr1D2D,     'Advection correction of 1D2D link volume (0: regular advection, 1: link volume au*dx, 2: advection on 1D2D switched off.')

    call prop_set(prop_ptr, 'numerics', 'TimeStepType', itstep,     'Time step handling (0: only transport, 1: transport + velocity update, 2: full implicit step-reduce, 3: step-Jacobi, 4: explicit)')
    if (writeall .or. maxNonlinearIterations /= 100) then
        call prop_set(prop_ptr, 'numerics', 'maxNonlinearIterations'    , maxNonlinearIterations, 'Maximal iterations in non-linear iteration loop before a time step reduction is applied')
    end if
    if (writeall .or. setHorizontalBobsFor1d2d) then
        call prop_set(prop_ptr, 'numerics', 'setHorizontalBobsFor1d2d', merge(1, 0, setHorizontalBobsFor1d2d), 'bobs are set to 2D bedlevel, to prevent incorrect storage in sewer system (0: no, 1:yes).')
    end if
    call prop_set(prop_ptr, 'numerics', 'Icoriolistype', icorio,    '0=No, 5=default, 3,4 no weights, 5-10 Kleptsova hu/hs, 25-30 Ham hs/hu, odd: 2D hs/hu, even: hsk/huk ')
    call prop_set(prop_ptr, 'numerics', 'Newcorio',      newcorio,  '0=prior to 27-11-2019, 1=no normal forcing on open bnds, plus 12 variants )')

    if (writeall .or. jacorioconstant .ne. 0) then  
        call prop_set(prop_ptr, 'numerics', 'Corioconstant', '0=default, 1=Coriolis constant in sferic models anyway,2=beta plane, both in cart. and spher. coord.')
    endif
    if (writeall .or. Corioadamsbashfordfac .ne. 0.5d0) then
       call prop_set(prop_ptr, 'numerics', 'Corioadamsbashfordfac', Corioadamsbashfordfac,    '0=No, 0.5d0=AdamsBashford, only for Newcorio=1)')
    endif
    if (writeall .or. hhtrshcor>0) then
       call prop_set(prop_ptr, 'numerics', 'Coriohhtrsh', hhtrshcor,    '0=default=no safety in hu/hus weightings, only for Newcorio=1)')
    endif ! change hhtrshcor to Coriohhtrsh

!   call prop_set_integer(prop_ptr, 'numerics', 'numoverlap', numoverlap, ' ')

    if (writeall .or. Limtyphu .ne. 0) then
       call prop_set(prop_ptr, 'numerics', 'Limtyphu',  limtyphu,   'Limiter type for waterdepth in continuity eqn. (0: none, 1: minmod, 2: van Leer, 3: Koren, 4: monotone central)')
    endif
    call prop_set(prop_ptr, 'numerics', 'Limtypmom',    limtypmom,  'Limiter type for cell center advection velocity (0: none, 1: minmod, 2: van Leer, 3: Koren, 4: monotone central)')
    call prop_set(prop_ptr, 'numerics', 'Limtypsa',     limtypsa,   'Limiter type for salinity transport (0: none, 1: minmod, 2: van Leer, 3: Koren, 4: monotone central)')
    if (writeall .or. (jawave.eq.4 .and. jajre.eq.1 .and. (.not. flowWithoutWaves) .and. swave.eq.1)) then
       call prop_set(prop_ptr, 'numerics', 'Limtypw'         , limtypw, 'Limiter type for wave action transport (0: none, 1: minmod, 2: van Leer, 3: Koren, 4: monotone central)')
    end if

    call prop_set(prop_ptr, 'numerics', 'TransportAutoTimestepdiff', jatransportautotimestepdiff,   'Auto Timestepdiff in Transport, 0 : lim diff, no lim Dt_tr, 1 : no lim diff, lim Dt_tr, 2: no lim diff, no lim Dt_tr, 3=implicit (only 2D)')
    call prop_set(prop_ptr, 'numerics', 'Implicitdiffusion2D', Implicitdiffusion2D,   '1 = Yes, 0 = No')
  
    call prop_set(prop_ptr, 'numerics', 'DiagnosticTransport' , jadiagnostictransport, 'Diagnostic ("frozen") transport (0: prognostic transport, 1: diagnostic transport)')

    call prop_set(prop_ptr, 'numerics', 'Vertadvtypsal', Javasal,   'Vertical advection type for salinity (0: none, 1: upwind explicit, 2: central explicit, 3: upwind implicit, 4: central implicit, 5: central implicit but upwind for neg. stratif., 6: higher order explicit, no Forester)')
    call prop_set(prop_ptr, 'numerics', 'Vertadvtyptem', Javatem,   'Vertical advection type for temperature (0: none, 1: upwind explicit, 2: central explicit, 3: upwind implicit, 4: central implicit, 5: central implicit but upwind for neg. stratif., 6: higher order explicit, no Forester)')
    call prop_set(prop_ptr, 'numerics', 'Vertadvtypmom', javau, 'Vertical advection type for u1: 0: No, 3: Upwind implicit, 4: Central implicit, 5: QUICKEST implicit., 6: centerbased upwind expl, 7=6 HO' )
    if (writeall .or. javau3onbnd .ne. 0) then
      call prop_set(prop_ptr, 'numerics', 'Vertadvtypmom3onbnd', javau3onbnd, 'vert. adv. u1 bnd UpwimpL: 0=follow javau , 1 = on bnd, 2= on and near bnd' )
    endif
    if (writeall .or. cffacver .ne. 0d0 ) then
      call prop_set(prop_ptr, 'numerics', 'Cffacver', Cffacver,   'Factor for including (1-CFL) in HO term vertical   (0d0: no, 1d0: yes)')
    endif
    if (writeall .or. cffachormom .ne. 1d0 ) then
      call prop_set(prop_ptr, 'numerics', 'Cffachormom', Cffachormom,   'Factor for including (1-CFL) in HO term horizontal mom (0d0: no, 1d0: yes)')
    endif
    if (writeall .or. cfexphormom .ne. 1d0 ) then
      call prop_set(prop_ptr, 'numerics', 'Cfexphormom', Cfexphormom,   'exponent for including (1-CFL) in HO term horizontal mom )')
    endif
    if (writeall .or. cfconhormom .ne. 0d0 ) then
      call prop_set(prop_ptr, 'numerics', 'Cfconhormom', Cfconhormom,   'constant for including (1-CFL) in HO term horizontal mom )')
    endif

    if (writeall .or. cffachu .ne. 1d0 ) then
      call prop_set(prop_ptr, 'numerics', 'Cffachu', Cffachu,   'Factor for including (1-CFL) in sethu (0d0: no, 1d0: yes)')
    endif
    if (writeall .or. cfexphu .ne. 1d0 ) then
      call prop_set(prop_ptr, 'numerics', 'Cfexphu', Cfexphu,   'exp for including (1-CFL) in sethu')
    endif
 
 
    if (writeall .or. jarhoxu .ne. 0 ) then
      call prop_set(prop_ptr, 'numerics', 'Jarhoxu', Jarhoxu,   'Include density gradient in advection term (0: no(strongly advised), 1: yes, 2: Also in barotropic and baroclinic pressure term)')
    endif
    if (writeall .or. (jahazlayer .ne. 0 .and. layertype .ne. 1)) then
      call prop_set(prop_ptr, 'numerics', 'Horadvtypzlayer', Jahazlayer, 'Horizontal advection treatment of z-layers (1: default, 2: sigma-like)')
    endif

    if (writeall .or. jaPure1D > 0) then
       call prop_set(prop_ptr, 'numerics', 'Pure1D'        , jaPure1D, 'along 1D channels: 0 = 2D Perot, 1 = 1D Perot using vol1_f, 2 = 1D Perot using vol1, 3 and 4 = 1D links')
       call prop_set(prop_ptr, 'numerics', 'Junction1D'    , jaJunction1D, 'at 1D junctions: 0 = org 1D advec, 1 = same as along 1D channels')
    endif

    if (writeall .or. jaZerozbndinflowadvection > 0) then
       call prop_set(prop_ptr, 'numerics', 'Zerozbndinflowadvection', jaZerozbndinflowadvection, 'On waterlevel boundaries set incoming advection velocity to zero (0=no, 1=on inflow, 2=also on outflow)')
    endif
    if (writeall .or. jaZlayercenterbedvel == 1) then
       call prop_set(prop_ptr, 'numerics', 'Zlayercenterbedvel', JaZlayercenterbedvel, 'reconstruction of center velocity at half closed bedcells (0=no, 1: copy bed link velocities)')
    endif
    if (writeall .or. jaStructurelayersactive == 1) then
       call prop_set(prop_ptr, 'numerics', 'Structurelayersactive', JaStructurelayersactive, '0=structure flow through all layers, 1=structure flow only through open layers ')
    endif

    call prop_set(prop_ptr, 'numerics', 'Icgsolver',       Icgsolver, 'Solver type (1: sobekGS_OMP, 2: sobekGS_OMPthreadsafe, 3: sobekGS, 4: sobekGS + Saadilud, 5: parallel/global Saad, 6: parallel/Petsc, 7: parallel/GS)')
    if (writeall .or. Maxdge .ne. 6) then
       call prop_set(prop_ptr, 'numerics', 'Maxdegree',  Maxdge,      'Maximum degree in Gauss elimination')
    end if
    if (writeall .or. Noderivedtypes > 0 ) then
       !call prop_set(prop_ptr, 'numerics', 'Noderivedtypes', Noderivedtypes,  '0=use der. types. , 1,2,3,4,5 etc = do use them')
    end if
    if (writeall .or. jposhchk /= 2) then
       call prop_set(prop_ptr, 'numerics', 'jposhchk',       jposhchk, 'Check for positive waterdepth (0: no, 1: 0.7*dts, just redo, 2: 1.0*dts, close all links, 3: 0.7*dts, close all links, 4: 1.0*dts, reduce au, 5: 0.7*dts, reduce au, 6: 1.0*dts, close outflowing links, 7: 0.7*dts, close outflowing links)')
    end if

    if (writeall .or. (len_trim(md_fixedweirfile) > 0)) then
       call prop_set(prop_ptr, 'numerics', 'FixedWeirScheme', ifixedweirscheme,            'Fixed weir scheme (0: none, 1: compact stencil, 2: whole tile lifted, full subgrid weir + factor)')
       call prop_set(prop_ptr, 'numerics', 'FixedWeirContraction', Fixedweircontraction,   'Fixed weir flow width contraction factor')
       call prop_set(prop_ptr, 'numerics', 'Fixedweirfrictscheme' , ifxedweirfrictscheme,  'Fixed weir friction scheme (0: friction based on hu, 1: friction based on subgrid weir friction scheme)')
       call prop_set(prop_ptr, 'numerics', 'Fixedweirtopwidth' , fixedweirtopwidth,        'Uniform width of the groyne part of fixed weirs')
       call prop_set(prop_ptr, 'numerics', 'Fixedweirtopfrictcoef' , fixedweirtopfrictcoef,'Uniform friction coefficient of the groyne part of fixed weirs')
       call prop_set(prop_ptr, 'numerics', 'Fixedweirtalud' , fixedweirtalud,              'Uniform talud slope of fixed weirs')
       call prop_set(prop_ptr, 'numerics', 'FixedweirRelaxationcoef' , waquaweirthetaw,    'Fixed weir relaxation coefficient for computation of energy loss')
       if (waquaweirthetaw .gt. 1.0d0) then
           waquaweirthetaw = 1.0d0
           call mess(LEVEL_INFO, 'WARNING: Fixed weir relaxation coefficient is reset to the maximum value of 1.0', '.')
       elseif (waquaweirthetaw .lt. 0.0d0) then 
           waquaweirthetaw = 0.0d0
           call mess(LEVEL_INFO, 'WARNING: Fixed weir relaxation coefficient is reset to the minimum value of 0.0', '.')
    endif
    endif
    if (writeall .or. (izbndpos > 0)) then
       call prop_set(prop_ptr, 'numerics', 'Izbndpos',  Izbndpos,   'Position of z boundary (0: D3Dflow, 1: on net boundary, 2: on specified polyline)')
    endif

    call prop_set(prop_ptr, 'numerics', 'Tlfsmo'            , Tlfsmo,             'Fourier smoothing time (s) on water level boundaries')
    if (writeall .or. keepstbndonoutflow > 0) then
       call prop_set(prop_ptr, 'numerics', 'Keepstbndonoutflow', keepstbndonoutflow, 'Keep sal and tem signals on bnd also at outflow, 1=yes, 0=no=default=copy inside value on outflow')
    endif

    if (writeall .or. jadiffusiononbnd == 0 ) then
       call prop_set(prop_ptr, 'numerics', 'Diffusiononbnd', jadiffusiononbnd, 'On open boundaries, 0 switches off horizontal diffusion Default = 1')
    endif

    if (writeall .or. Tspinupturblogprof > 0d0) then
       call prop_set(prop_ptr, 'numerics', 'Tspinupturblogprof', Tspinupturblogprof, 'During Tspinup force log profiles to avoid almost zero vertical eddy viscosity')
    endif
    if ( writeall .or. jaLogprofatubndin .ne. 1) then
       call prop_set(prop_ptr, 'numerics', 'Logprofatubndin', jaLogprofatubndin, 'ubnds inflow: 0=uniform U1, 1 = log U1, 2 = user3D')
    endif
    if ( writeall .or. jaLogprofkepsbndin .ne. 1) then
       call prop_set(prop_ptr, 'numerics', 'Logprofkepsbndin', jaLogprofkepsbndin, 'inflow: 0=0 keps, 1 = log keps inflow, 2 = log keps in and outflow')
    endif

    call prop_set(prop_ptr, 'numerics', 'Slopedrop2D', Slopedrop2D, 'Apply drop losses only if local bed slope > Slopedrop2D, (<=0: no drop losses)')

    if (Drop1D) then
       help = 1
    else
       help = 0
    endif
    call prop_set(prop_ptr, 'numerics', 'Drop1D', help, 'Apply drop losses in 1D (0: no, 1:yes)')

    if (writeall .or. Drop3D .ne. 1d0) then
       call prop_set(prop_ptr, 'numerics', 'Drop3D'   , Drop3D, 'Apply droplosses in 3D if z upwind below bob + 2/3 hu*drop3D')
    endif

    if (writeall .or. Chkadvd .ne. 0.1d0) then
       call prop_set(prop_ptr, 'numerics', 'Chkadvd'  , Chkadvd, 'Check advection terms if depth < chkadvdp, => less setbacks')
    endif

    if (writeall .or. Linkdriedmx .ne. 0) then
       call prop_set(prop_ptr, 'numerics', 'Linkdriedmx'    , Linkdriedmx, 'Nr of Au reduction steps after having dried')
    endif

    if (writeall .or. Huweirregular .ne. 0d0) then
       call prop_set(prop_ptr, 'numerics', 'Huweirregular'  , Huweirregular, 'For villemonte and Tabellenboek, regular hu below Huweirregular')
    endif

    if ( writeall .or. Chkdifd .ne. 0.01d0 .and. jatransportautotimestepdiff == 1) then
       call prop_set(prop_ptr, 'numerics', 'Chkdifd'  , Chkdifd, 'Check diffusion terms if depth < chkdifd, only if jatransportautotimestepdiff==1')
    endif

    if (writeall .or. trsh_u1Lb .ne. 0.0d0) then
       call prop_set(prop_ptr, 'numerics', 'Trsh_u1Lb'  , trsh_u1Lb, '2D bedfriction in 3D below this threshold (m)')
    endif

    if (writeall .or. epshstem .ne. 0.001d0) then
       call prop_set(prop_ptr, 'numerics', 'Epshstem',  epshstem , 'Only compute heatflx + evap if depth > epshstem')
    endif

    if (writeall .or. Zwsbtol .ne. 0d0) then
       call prop_set(prop_ptr, 'numerics', 'Zwsbtol'  , zwsbtol, 'tolerance for zws(kb-1) at bed')
    endif

    if (writeall .or. Teta0 .ne. 0.55d0) then
       call prop_set(prop_ptr, 'numerics', 'Teta0'    , Teta0,   'Theta of time integration (0.5 < theta < 1)')
    endif

    if (writeall .or. jbasqbnddownwindhs > 0) then
       call prop_set(prop_ptr, 'numerics', 'Jbasqbnddownwindhs'    , jbasqbnddownwindhs, 'Water depth scheme at discharge boundaries (0: original hu, 1: downwind hs)')
    endif

    call prop_set(prop_ptr, 'numerics', 'cstbnd'  , jacstbnd, 'Delft-3D type velocity treatment near boundaries for small coastal models (1: yes, 0: no)')

    if (writeall .or. kmx > 0) then
       call prop_set(prop_ptr, 'numerics', 'Maxitverticalforestersal' , Maxitverticalforestersal, 'Forester iterations for salinity (0: no vertical filter for salinity, > 0: max nr of iterations)')
    endif

    if (writeall .or. (kmx > 0 .and. jatem > 0)) then
       call prop_set(prop_ptr, 'numerics', 'Maxitverticalforestertem' , Maxitverticalforestertem, 'Forester iterations for temperature (0: no vertical filter for temperature, > 0: max nr of iterations)')
    endif

    if (writeall .or. kmx > 0) then
       call prop_set(prop_ptr, 'numerics', 'Turbulencemodel' , Iturbulencemodel, 'Turbulence model (0: none, 1: constant, 2: algebraic, 3: k-epsilon, 4: k-tau)')
    endif

    if (writeall .or. (javakeps .ne. 3  .and. kmx > 0) ) then
       call prop_set(prop_ptr, 'numerics', 'Turbulenceadvection' , javakeps, 'Turbulence advection (0: none, 3: horizontally explicit and vertically implicit)')
    endif

    if (writeall .or. (jadrhodz .ne. 1  .and. kmx > 0) ) then
       call prop_set(prop_ptr, 'numerics', 'Jadrhodz' , jadrhodz, '(1:central org, 2:centralnew, 3:upw cell, 4:most stratf. cell, 5:least stratf. cell)')
    endif

    if (writeall .or. Eddyviscositybedfacmax > 0 .and. kmx > 0) then
       call prop_set(prop_ptr, 'numerics', 'Eddyviscositybedfacmax' , Eddyviscositybedfacmax, 'Limit eddy viscosity at bed )')
    endif

    if( writeall .or. (jacreep == 1 .and. kmx > 0) ) then
        call prop_set(prop_ptr, 'numerics', 'AntiCreep', jacreep, 'Include anti-creep calculation (0: no, 1: yes)')
    endif

    if( jaorgbarockeywords == 1) then
        call prop_set(prop_ptr, 'numerics', 'orgbarockeywords', jaorgbarockeywords, '(1=yes)')
        call prop_set(prop_ptr, 'numerics', 'Barocterm'       , jabarocterm       , '(     )')
        call prop_set(prop_ptr, 'numerics', 'Baroctimeint'    , jabaroctimeint    , '(     )')
    endif

    if (writeall .or. Jabaroczlaybed .ne. 0) then
       call prop_set(prop_ptr, 'numerics', 'Baroczlaybed' , jabaroczlaybed ,'Use fix in barocp for zlaybed 0,1, 1=default)')
    endif
    if (writeall .or. Jabarocponbnd .ne. 0) then
       call prop_set(prop_ptr, 'numerics', 'Barocponbnd'  , jabarocponbnd  ,'Use fix in barocp for zlaybed 0,1, 1=default)')
    endif
   if (writeall .or. Maxitpresdens .ne. 1) then
       call prop_set(prop_ptr, 'numerics', 'Maxitpresdens', Maxitpresdens  ,'Max nr of iterations in pressure-density coupling, only used if idensform > 10 )')
    endif
    if (writeall .or. jarhointerfaces .ne. 0) then
       call prop_set(prop_ptr, 'numerics', 'Rhointerfaces', jarhointerfaces,'Evaluate rho at interfaces, 0=org at centers, 1=at interfaces )')
    endif
  
    if ( icgsolver.eq.8 ) then   ! for parms solver
       do i=1,NPARMS_INT
          call prop_set_integer(prop_ptr, 'numerics', trim(iparmsnam(i)), iparms(i), '0: parms-default')
       end do
       do i=1,NPARMS_DBL
          call prop_set_double(prop_ptr, 'numerics', trim(dparmsnam(i)), dparms(i), '0d0: parms-default')
       end do
    end if

    if (writeall .or. (s01max > 0d0)) then
        call prop_set(prop_ptr, 'numerics', 'Maxwaterleveldiff', s01max, 'upper bound (in m) on water level changes (<= 0: no bounds). Run will abort when violated.')
    end if

    if (writeall .or. (u01max > 0d0)) then
        call prop_set(prop_ptr, 'numerics', 'Maxvelocitydiff', u01max, 'upper bound (in m/s) on velocity changes (<= 0: no bounds). Run will abort when violated.')
    endif

    if (writeall .or. (umagmax > 0d0)) then
        call prop_set(prop_ptr, 'numerics', 'Maxvelocity', umagmax, 'upper bound (in m/s) on velocity (<= 0: no bounds). Run will abort when violated.')
    endif

    if (writeall .or. (s01warn > 0d0)) then
        call prop_set(prop_ptr, 'numerics', 'Waterlevelwarn', s01warn, 'warning level (in m) on water level (<= 0: no check).')
    end if

    if (writeall .or. (u01warn > 0d0)) then
        call prop_set(prop_ptr, 'numerics', 'Velocitywarn', u01warn, 'warning level (in m/s) on velocity u1 (<= 0: no check).')
    endif

    if (writeall .or. (umagwarn > 0d0)) then
        call prop_set(prop_ptr, 'numerics', 'Velmagnwarn', umagwarn, 'warning level (in m/s) on velocity magnitude (<= 0: no check).')
    endif

    if (writeall .or. (dtminbreak > 0d0)) then
        call prop_set(prop_ptr, 'numerics', 'MinTimestepBreak', dtminbreak,  'smallest allowed timestep (in s), checked on a sliding average of several timesteps. Run will abort when violated.')
    end if

    if ((writeall .or. (sscmax > 0d0)) .and. jased==4) then
        call prop_set(prop_ptr, 'numerics', 'MaxSSC', sscmax, 'upper bound (in kg/m3) on SSC (<= 0: no bounds). Run will abort when violated.')
    end if

    call prop_set(prop_ptr, 'numerics', 'Epshu' , epshu, 'Threshold water depth for wet and dry cells')

    if (writeall .or. (sbkdfm_umin > 0d0)) then
        call prop_set(prop_ptr, 'numerics', 'SobekDFM_umin', sbkdfm_umin, 'Minimal velocity treshold for weir losses in Sobek-DFM coupling.')
        call prop_set(prop_ptr, 'numerics', 'SobekDFM_umin_method', sbkdfm_umin_method, 'Method for minimal velocity treshold for weir losses in Sobek-DFM coupling.')
    end if
    if (writeall .or. (minimal_1d2d_embankment > 0.01d0)) then
       call prop_set(prop_ptr, 'numerics', 'SobekDFM_Minimal_1d2d_Embankment', minimal_1d2d_embankment, 'Minimal crest height of 1D2D SOBEK-DFM embankments.')
    end if

    if (writeall .or. (sbkdfm_relax /= 0.1d0)) then
       call prop_set(prop_ptr, 'numerics', 'sobekDFM_relax', sbkdfm_relax, 'Relaxation factor for SOBEK-DFM coupling algorithm.')
    endif


    call prop_set_integer(prop_ptr, 'numerics', 'jaupwindsrc', jaupwindsrc, '1st-order upwind advection at sources/sinks (1) or higher-order (0)')

    call prop_set_integer(prop_ptr, 'numerics', 'jasfer3D', jasfer3D, 'corrections for spherical coordinates')

    if (writeall .or. jabarrieradvection /= 1) then
       call prop_set_integer(prop_ptr, 'numerics', 'BarrierAdvection', jabarrieradvection, '1 = no correction, 2 = advection correction')
    endif

    if (writeall .or. jafilter.ne.0 ) then
       call prop_set(prop_ptr, 'numerics', 'HorizontalMomentumFilter',           jafilter, 'apply horizontal filter (1:explicit, 2,3:implicit) or not (0)')
       !call prop_set(prop_ptr, 'numerics', 'filterorder',      filterorder,    'order of filter (1, 2 or 3)')
    end if

    if (writeall .or. jacheckmonitor.eq.1 ) then
       call prop_set(prop_ptr, 'numerics', 'checkerboardmonitor', jacheckmonitor,    'compute and output checkerboard monitor (1) or not (0)')
    end if

    if (writeall .or. locsaltlev /= 1d0) then
       call prop_set (prop_ptr, 'numerics', 'LocSaltLev', locsaltlev, 'salinity level for case of lock exchange'  )
    endif
    if (writeall .or. locsaltmin /= 5d0) then
       call prop_set (prop_ptr, 'numerics', 'LocSaltMin', locsaltmin, 'minimum salinity for case of lock exchange')
    endif
    if (writeall .or. locsaltmax /= 10d0) then
       call prop_set (prop_ptr, 'numerics', 'LocSaltMax', locsaltmax, 'maximum salinity for case of lock exchange')
    endif
    if (writeall .or. numlimdt_baorg > 0) then
       call prop_set (prop_ptr, 'numerics', 'Numlimdt_baorg', Numlimdt_baorg, 'if previous numlimdt > Numlimdt_baorg keep original cell area ba in cutcell')
    endif
    if (writeall .or. baorgfracmin > 0) then
       call prop_set (prop_ptr, 'numerics', 'Baorgfracmin', Baorgfracmin, 'Cell area = max(orgcellarea*Baorgfracmin, cutcell area) ')
    endif
    if (writeall .or. sdu_update_s1 > 0) then
       call prop_set (prop_ptr, 'numerics', 'SubsUplUpdateS1', sdu_update_s1, 'Update water levels (S1) due to subsidence / uplift')
    endif

    if (writeall .or. epsmaxlev .ne. 1d-8) then
    call prop_set(prop_ptr, 'numerics', 'EpsMaxlev',    epsmaxlev,  'Stop criterium for non linear iteration')
    endif
    if (writeall .or. epsmaxlevm .ne. 1d-8) then
    call prop_set(prop_ptr, 'numerics', 'EpsMaxlevm',   epsmaxlevm, 'Stop criterium for Nested Newton loop in non linear iteration')
    endif

    if (Oceaneddyamp > 0d0) then 
       call prop_set(prop_ptr, 'numerics', 'Oceaneddysizefrac'   , Oceaneddysizefrac)
       call prop_set(prop_ptr, 'numerics', 'Oceaneddysize'       , Oceaneddysize)
       call prop_set(prop_ptr, 'numerics', 'Oceaneddyamp'        , Oceaneddyamp)
       call prop_set(prop_ptr, 'numerics', 'Oceaneddyvel'        , Oceaneddyvel)
       call prop_set(prop_ptr, 'numerics', 'Oceaneddyyoff'       , Oceaneddyyoff)
       call prop_set(prop_ptr, 'numerics', 'Oceaneddyxoff'       , Oceaneddyxoff)
    endif

    call prop_set(prop_ptr, 'numerics', 'Testdryingflooding', testdryflood,   'Test for drying flooding algoritm (0: D-Flow FM, 1: Delft3D-FLOW)')
    call prop_set(prop_ptr, 'numerics', 'Testfixedweirs', testfixedweirs, 'Test for fixed weir algoritms (0 = Sieben2010, 1 = Sieben2007 )')

! Physics
    call prop_set(prop_ptr, 'physics', 'UnifFrictCoef',     frcuni,      'Uniform friction coefficient (0: no friction)')
    call prop_set(prop_ptr, 'physics', 'UnifFrictType',     ifrctypuni,  'Uniform friction type (0: Chezy, 1: Manning, 2: White-Colebrook, 3: idem, WAQUA style)')
    call prop_set(prop_ptr, 'physics', 'UnifFrictCoef1D',   frcuni1D,    'Uniform friction coefficient in 1D links (0: no friction)')
    call prop_set(prop_ptr, 'physics', 'UnifFrictCoef1D2D', frcuni1D2D,  'Uniform friction coefficient in 1D links (0: no friction)')
    call prop_set(prop_ptr, 'physics', 'UnifFrictCoefLin',  frcunilin,   'Uniform linear friction coefficient (0: no friction)')
    if (writeall .or. frcuni1Dgrounlay > 0) then
       call prop_set(prop_ptr, 'physics', 'UnifFrictCoef1DgrLay',  frcuni1Dgrounlay,   'Uniform ground layer friction coefficient for ocean models (m/s) (0: no friction)')
    endif
    if (writeall) then
       call prop_set(prop_ptr, 'physics', 'Umodlin',       umodlin,      'Linear friction umod, for ifrctyp=4,5,6')
    end if
    call prop_set(prop_ptr, 'physics', 'Vicouv',           vicouv,       'Uniform horizontal eddy viscosity (m2/s)')
    call prop_set(prop_ptr, 'physics', 'Dicouv',           dicouv,       'Uniform horizontal eddy diffusivity (m2/s)')
    if (writeall .or. (kmx > 0)) then
       call prop_set(prop_ptr, 'physics', 'Vicoww',        vicoww,       'Uniform vertical eddy viscosity (m2/s)')
       call prop_set(prop_ptr, 'physics', 'Dicoww',        dicoww,       'Uniform vertical eddy diffusivity (m2/s)')
       if (writeall .or. (vicwminb > 0d0)) then
          call prop_set(prop_ptr, 'physics', 'Vicwminb',   Vicwminb,     'Minimum visc in prod and buoyancy term (m2/s)')
       endif
       call prop_set(prop_ptr, 'physics', 'Xlozmidov',     xlozmidov,    'Ozmidov length scale (m), default=0.0, no contribution of internal waves to vertical diffusion')
    endif
    call prop_set(prop_ptr, 'physics', 'Smagorinsky',      Smagorinsky,  'Smagorinsky factor in horizontal turbulence, e.g. 0.15')
    call prop_set(prop_ptr, 'physics', 'Elder',            Elder,        'Elder factor in horizontal turbulence')
    call prop_set(prop_ptr, 'physics', 'irov',             irov,         '0=free slip, 1 = partial slip using wall_ks')
    call prop_set(prop_ptr, 'physics', 'wall_ks',          wall_ks,      'Wall roughness type (0: free slip, 1: partial slip using wall_ks)')
    call prop_set(prop_ptr, 'physics', 'Rhomean',          rhomean,      'Average water density (kg/m3)')
  
    if (writeall .or. idensform .ne. 1) then
       call prop_set(prop_ptr, 'physics', 'Idensform',     idensform,    'Density calulation (0: uniform, 1: Eckart, 2: UNESCO, 3=UNESCO83, 13=3+pressure)')
    endif
  
    call prop_set(prop_ptr, 'physics', 'Ag'     ,          ag ,          'Gravitational acceleration')
    call prop_set(prop_ptr, 'physics', 'TidalForcing',     jatidep,      'Tidal forcing, if jsferic=1 (0: no, 1: yes)')
    call prop_set(prop_ptr, 'physics', 'SelfAttractionLoading',  jaselfal,     'Self attraction and loading (0=no, 1=yes, 2=only self attraction)')
    call prop_set(prop_ptr, 'physics', 'SelfAttractionLoading_correct_wl_with_ini',  jaSELFALcorrectWLwithIni,     'correct water level with initial water level in Self attraction and loading (0=no, 1=yes)')

    call prop_set(prop_ptr, 'physics', 'ITcap',  ITcap,     'Upper limit on internal tides dissipation (W/m^2)')

    if (writeall .or. jatidep > 0 .and. jsferic == 1) then
       call prop_set(prop_ptr, 'physics', 'Doodsonstart',  Doodsonstart, 'TRIWAQ: 55.565, D3D: 57.555'     )
       call prop_set(prop_ptr, 'physics', 'Doodsonstop',   Doodsonstop,  'TRIWAQ: 375.575, D3D: 275.555'     )
       call prop_set(prop_ptr, 'physics', 'Doodsoneps',    Doodsoneps,   'TRIWAQ = 0.0  400 cmps , D3D = 0.03   60 cmps')
    endif
    call prop_set(prop_ptr, 'physics', 'VillemonteCD1', VillemonteCD1,   'Calibration coefficient for Villemonte. Default = 1.0.')
    call prop_set(prop_ptr, 'physics', 'VillemonteCD2', VillemonteCD2,   'Calibration coefficient for Villemonte. Default = 10.0.')
    call prop_set(prop_ptr,    'physics', 'Salinity',      jasal,        'Include salinity, (0: no, 1: yes)' )
    if (writeall .or. (jasal > 0)) then
       call prop_set(prop_ptr, 'physics','InitialSalinity',salini,       'Uniform initial salinity concentration (ppt)')
       if (writeall .or. (Sal0abovezlev .ne. dmiss)) then
           call prop_set(prop_ptr, 'physics', 'Sal0abovezlev', Sal0abovezlev, 'Vertical level (m) above which salinity is set 0')
       endif
       if (writeall .or. (deltasalinity .ne. dmiss)) then
           call prop_set(prop_ptr, 'physics', 'DeltaSalinity', Deltasalinity, 'for testcases')
       endif
       if (writeall .or. (salimax .ne. dmiss .or. salimin .ne. 0d0)) then
          call prop_set(prop_ptr, 'physics', 'Salimax',  Salimax , 'Limit the salinity')
          call prop_set(prop_ptr, 'physics', 'Salimin',  Salimin , 'Limit the salinity')
       endif
    endif

    if (writeall .or. (jasal == 0 .and. (jatem > 0 .or. jased>0 )) ) then
       call prop_set(prop_ptr, 'physics', 'Backgroundsalinity', Backgroundsalinity, 'Background salinity for eqn. of state (psu) if salinity not computed')
    endif

    if (writeall .or. (jatem == 0 .and. (jasal > 0 .or. jased>0 )) ) then
       call prop_set(prop_ptr, 'physics', 'Backgroundwatertemperature', Backgroundwatertemperature, 'Background water temperature for eqn. of state (deg C) if temperature not computed')
    endif

    ! call prop_set(prop_ptr, 'physics', 'Jadelvappos'     , Jadelvappos, 'Only positive forced evaporation fluxes')           ! RL666

    call prop_set(prop_ptr, 'physics', 'Temperature'     , jatem,       'Include temperature (0: no, 1: only transport, 3: excess model of D3D, 5: composite (ocean) model)')
    if (writeall .or. (jatem > 0)) then
       call prop_set(prop_ptr, 'physics', 'InitialTemperature', temini, 'Uniform initial water temperature (degC)')
       call prop_set(prop_ptr, 'physics', 'Secchidepth', Secchidepth, 'Water clarity parameter (m)')
       if (Secchidepth2 > 0) then
       call prop_set(prop_ptr, 'physics', 'Secchidepth2', Secchidepth2, 'Water clarity parameter 2 (m), only used if > 0')
       call prop_set(prop_ptr, 'physics', 'Secchidepth2fraction', Secchidepth2fraction, 'Fraction of total absorbed by profile 2')
       endif

       call prop_set(prop_ptr, 'physics', 'Stanton', Stanton, 'Coefficient for convective heat flux, if negative, Ccon = abs(Stanton)*Cdwind')
       call prop_set(prop_ptr, 'physics', 'Dalton',  Dalton , 'Coefficient for evaporative heat flux, if negative, Ceva = abs(Dalton)*Cdwind')

       if (writeall .or. (tempmax .ne. dmiss .or. tempmin .ne. 0d0)) then
          call prop_set(prop_ptr, 'physics', 'Tempmax',  Tempmax , 'Limit the temperature')
          call prop_set(prop_ptr, 'physics', 'Tempmin',  Tempmin , 'Limit the temperature, if -999, tempmin=(-0.0575d0 - 2.154996d-4*sal)*sal')
       endif
       if (writeall .or. Jaallowcoolingbelowzero .ne. 0) then
          call prop_set(prop_ptr, 'physics', 'Allowcoolingbelowzero',  Jaallowcoolingbelowzero , '0 = no, 1 = yes')
       endif
       if (writeall .or. surftempsmofac > 0d0) then
          call prop_set(prop_ptr, 'physics', 'Surftempsmofac',  Surftempsmofac , 'Hor . Smoothing factor for surface water in heatflx comp. (0.0-1.0), 0=no')
       endif
       if (writeall .or. Soiltempthick .ne. 0d0) then
          call prop_set(prop_ptr, 'physics', 'Soiltempthick',  Soiltempthick , 'Use soil temperature buffer if > 0, e.g. 0.2 (m)' )
       endif
       if (writeall .or. jaheat_eachstep > 0) then
          call prop_set(prop_ptr, 'physics', 'Heat_eachstep' , jaheat_eachstep,  '1=heat each timestep, 0=heat each usertimestep')
       endif
       if (writeall .or. jaroro > 0) then
          call prop_set(prop_ptr, 'physics', 'RhoairRhowater' , jaroro        ,  'windstress rhoa/rhow: 0=Rhoair/Rhomean, 1=Rhoair/rhow(), 2=rhoa0()/rhow(), 3=rhoa10()/Rhow()')
       endif

       if (writeall .or.  janudge > 0 .or. jainiwithnudge > 0 ) then
          call prop_set_double(prop_ptr, 'physics', 'Nudgetimeuni', Tnudgeuni, 'Uniform nudge relaxation time')
          call prop_set_integer(prop_ptr, 'physics', 'IniWithNudge', jainiwithnudge, 'Initialize salinity and temperature with nudge variables')
       end if

    endif

! secondary flow
! output to mdu file
    call prop_set(prop_ptr, 'physics', 'SecondaryFlow'  , jasecflow, 'Secondary flow (0: no, 1: yes)')

    if (writeall .or. (jasecflow > 0)) then
       call prop_set(prop_ptr, 'physics', 'BetaSpiral'     , spirbeta , 'Weight factor of the spiral flow intensity on flow dispersion stresses (0d0 = disabled)')
    endif
    if (writeall .or. jaequili > 0) then
       call prop_set(prop_ptr, 'physics', 'Equili'     , jaequili , 'Equilibrium spiral flow intensity (0: no, 1: yes)')
    endif

    if (writeall .or. jased > 0) then
       call prop_set(prop_ptr, 'sediment', 'Sedimentmodelnr'    ,  jased, 'Sediment model nr, (0=no, 1=Krone, 2=SvR2007, 3=E-H, 4=MorphologyModule)')
       call prop_set(prop_ptr, 'sediment', 'Implicitfallvelocity', jaimplicitfallvelocity, '1=Impl., 0 = Expl.')
       
       if (jased==4) then
          call prop_set(prop_ptr, 'sediment', 'SedFile'    , trim(md_sedfile), 'Sediment characteristics file (*.sed)')
          call prop_set(prop_ptr, 'sediment', 'MorFile'    , trim(md_morfile), 'Morphology settings file (*.mor)')
          call prop_set(prop_ptr, 'sediment', 'DredgeFile'    , trim(md_dredgefile), 'Dredging/dumping settings file (*.dad)')
          call prop_set(prop_ptr, 'sediment', 'MorphoPol', md_morphopol, 'Only apply bed updating wihtin specified polygon (*.pol)')
          call prop_set(prop_ptr, 'sediment', 'MorCFL'    , jamorcfl, 'Use CFL-like condition for morphologic updating (0=no, 1=yes) (default yes)')
          call prop_set(prop_ptr, 'sediment', 'DzbDtMax', dzbdtmax, 'Maximum bed level change (m) per time step for the case MorCFL=1 (default=0.1 m)')
          call prop_set(prop_ptr, 'sediment', 'InMorphoPol', inmorphopol, 'Value of the update inside MorphoPol (0=inside polygon no update, 1=inside polygon yes update)')
       end if

       if (jased .ne. 4) then
          call prop_set(prop_ptr, 'sediment', 'Nr_of_sedfractions' ,  mxgr , 'Nr of sediment fractions, (specify the next parameters for each fraction) ')
          call prop_set(prop_ptr, 'sediment', 'MxgrKrone'  ,  MxgrKrone    , 'Highest fraction index treated by Krone ')
          call prop_set(prop_ptr, 'sediment', 'Seddenscoupling',  jaseddenscoupling     , 'Sed rho coupling (0=no, 1=yes')

          if (Mxgr > 0) then

             call prop_set(prop_ptr, 'sediment', 'D50'       ,              D50,   'Mean Sandgrain diameter (m), e.g. 0.0001')
             call prop_set(prop_ptr, 'sediment', 'Rhosed'    ,           rhosed,   'Mean Sandgrain rho (kg/m3) , e.g. 2650')

             if (MxgrKrone > 0) then
                call prop_set(prop_ptr, 'sediment', 'Ws'         ,             Ws(1:mxgrKrone),   'Fall velocity (m/s), e.g. 0.0005 m/s')
                call prop_set(prop_ptr, 'sediment', 'Erosionpar' ,     erosionpar(1:mxgrKrone),   'Krone Partheniades erosion parameter, e.g. 0.0001  (kg/(m2s)')
                call prop_set(prop_ptr, 'sediment', 'Taucre'     ,rhomean*Ustcre2(1:mxgrKrone),   'Critical shear stress for erosion    (N/m2), e.g. 0.3')
             end if

             call prop_set(prop_ptr, 'sediment', 'InitialSedimentConcentration', sedini, 'Initial Sediment Concentration in jased==3 (kg/m3)  ')
             call prop_set(prop_ptr, 'sediment', 'UniformErodablethickness', Uniformerodablethickness, 'Uniform erodable layer thickness (m)')
             call prop_set(prop_ptr, 'sediment', 'Numintverticaleinstein', Numintverticaleinstein,     'Number of vertical intervals in Einstein integrals ( ) ')
             call prop_set(prop_ptr, 'sediment', 'Jaceneqtr', jaceneqtr,     '1=equilibriumtransport at cell centre, 2= at netnode (default) ( ) ')
             call prop_set(prop_ptr, 'sediment', 'Morfac ',  Dmorfac,        'Morphological acceleration factor (), bottom updates active for morfac > 0, 1d0=realtime, etc')
             call prop_set(prop_ptr, 'sediment', 'TMorfspinup', Tmorfspinup, 'Spin up time for morphological adaptations (s)')
             call prop_set(prop_ptr, 'sediment', 'Alfabed', alfabed, 'Calibration par bed      load, default=1d0 ( ) ')
             call prop_set(prop_ptr, 'sediment', 'Alfasus', alfasus, 'Calibration par suspended load, default=1d0 ( ) ')
             call prop_set(prop_ptr, 'sediment', 'Crefcav', crefcav, 'Calibration par only in jased==3, default=20d0 ( ) ')
          end if
       endif
    endif

    if (writeall .or. javeg > 0) then
       call prop_set(prop_ptr, 'veg', 'Vegetationmodelnr', javeg      , 'Vegetation model nr, (0=no, 1=Baptist DFM)')
       call prop_set(prop_ptr, 'veg', 'Clveg'          , Clveg        , 'Stem distance factor, default 0.8 ()' )
       call prop_set(prop_ptr, 'veg', 'Cdveg'          , Cdveg        , 'Stem Cd coefficient , default 0.7 ()' )
       call prop_set(prop_ptr, 'veg', 'Cbveg'          , Cbveg        , 'Stem stiffness coefficient , default 0.0 ()' )
       call prop_set(prop_ptr, 'veg', 'Rhoveg'         , Rhoveg       , 'Stem Rho, if > 0, -> bouyant stick procedure, default 0.0 (kg/m3)' )
       call prop_set(prop_ptr, 'veg', 'Stemheightstd'  , Stemheightstd, 'Stem height standard deviation fraction, e.g. 0.1  ()' )
       if (kmx == 0) then
          call prop_set(prop_ptr, 'veg', 'Densvegminbap'  , Densvegminbap, 'Minimum vegetation density in Baptist formula  (1/m2)' )
       endif
       if (expchistem < 0) then
          call prop_set (prop_ptr, 'veg'    , 'Expchistem'     , expchistem , '( )' )
          call prop_set (prop_ptr, 'veg'    , 'Expchileaf'     , expchileaf , '( )' )
          call prop_set (prop_ptr, 'veg'    , 'Uchistem'       , uchistem   , '(m/s)' )
          call prop_set (prop_ptr, 'veg'    , 'Uchileaf'       , uchileaf   , '(m/s)' )
          call prop_set (prop_ptr, 'veg'    , 'Arealeaf'       , arealeaf   , '(m2)   (per plant) ')
          call prop_set (prop_ptr, 'veg'    , 'Cdleaf'         , Cdleaf     , '( )' )
       endif
    endif

    call prop_set(prop_ptr,    'wind', 'ICdtyp',               ICdtyp,   'Wind drag coefficient type (1: Const, 2: Smith&Banke (2 pts), 3: S&B (3 pts), 4: Charnock 1955, 5: Hwang 2005, 6: Wuest 2005, 7: Hersbach 2010 (2 pts), 8: 4+viscous).' )
    if (ICdtyp == 1 .or. ICdtyp == 4 .or. ICdtyp == 5 .or. ICdtyp == 6) then
       call prop_set(prop_ptr, 'wind', 'Cdbreakpoints',        Cdb(1:1), 'Wind drag coefficient ')
    else if (ICdtyp == 2) then
       call prop_set(prop_ptr, 'wind', 'Cdbreakpoints',        Cdb(1:2), 'Wind drag coefficient break points')
       call prop_set(prop_ptr, 'wind', 'Windspeedbreakpoints', Wdb(1:2), 'Wind speed break points (m/s)')
    else if (ICdtyp == 3) then
       call prop_set(prop_ptr, 'wind', 'Cdbreakpoints',        Cdb(1:3), 'Wind drag coefficient break points')
       call prop_set(prop_ptr, 'wind', 'Windspeedbreakpoints', Wdb(1:3), 'Wind speed break points (m/s)')
   else 
       call prop_set(prop_ptr, 'wind', 'Cdbreakpoints',        Cdb(1:2), 'Wind drag coefficients')
    endif
    if (writeall .or. relativewind > 0d0) then
       call prop_set(prop_ptr,    'wind', 'Relativewind',      relativewind,   'Wind speed relative to top-layer water speed*relativewind, 0d0=no relative wind, 1d0=using full top layer speed)')
    endif
    if (writeall .or. kmx == 0 .and. jawindhuorzwsbased == 0 .or. kmx > 0 .and. jawindhuorzwsbased == 0) then
       call prop_set(prop_ptr, 'wind', 'Windhuorzwsbased', jawindhuorzwsbased,   'Wind hu or zws based , 0 = hu, 1 = zws ' )
    endif
    if (writeall .or. jawindpartialdry == 0) then
       call prop_set(prop_ptr, 'wind', 'Windpartialdry', jawindpartialdry, 'Reduce windstress on water if link partially dry, only for bedlevtyp=3, 0 = no, 1 = yes = default ' )
    endif

    call prop_set(prop_ptr, 'wind', 'Rhoair',               Rhoair, 'Air density (kg/m3)')
    call prop_set(prop_ptr, 'wind', 'PavBnd',               PavBnd, 'Average air pressure on open boundaries (N/m2) (only applied if > 0)')
    call prop_set(prop_ptr, 'wind', 'Pavini',               PavIni, 'Average air pressure for initial water level correction (N/m2) (only applied if > 0)')
    if (writeall .or. jastresstowind == 1) then
    call prop_set(prop_ptr, 'wind', 'Stresstowind', jastresstowind, 'Convert EC windstress to wind yes/no (),  1/0, default 0')
    endif

   
    if (writeall .or. jagrw > 0 .or. infiltrationmodel /= DFM_HYD_NOINFILT) then
       call prop_set(prop_ptr, 'grw', 'groundwater'        , jagrw,             '0=No (horizontal) groundwater flow, 1=With groundwater flow')
       write (tmpstr, '(a,5(i0,": ",a),a)') 'Infiltration method (', DFM_HYD_NOINFILT, 'No infiltration', 1, 'Interception layer', &
          DFM_HYD_INFILT_CONST, 'Constant infiltration capacity', DFM_HYD_INFILT_DARCY, 'model unsaturated/saturated (with grw)', DFM_HYD_INFILT_HORTON, 'Horton', &
          ')'
       call prop_set(prop_ptr, 'grw', 'Infiltrationmodel'  , Infiltrationmodel, trim(tmpstr))
       call prop_set(prop_ptr, 'grw', 'Hinterceptionlayer' , Hinterceptionlayer,'Intercept this amount of rain (m)' )
       call prop_set(prop_ptr, 'grw', 'UnifInfiltrationCapacity' , infiltcapuni, 'Uniform maximum infiltration capacity (m/s)' )
       call prop_set(prop_ptr, 'grw', 'Conductivity'       , Conductivity,      'Non-dimensionless K conductivity   saturated (m/s), Q = K*A*i (m3/s)' )

       call prop_set(prop_ptr, 'grw', 'h_aquiferuni'       , h_aquiferuni,      'bgrw = bl - h_aquiferuni (m), if negative, bgrw = bgrwuni '  )
       if (bgrwuni .ne. dmiss) then
       call prop_set(prop_ptr, 'grw', 'bgrwuni'            , bgrwuni,           'Uniform level of impervious layer, only used if h_aquiferuni is negative')
       endif

       call prop_set(prop_ptr, 'grw', 'h_unsatini'         , h_unsatini,        'Initial level groundwater is bedlevel - h_unsatini (m), if negative, sgrw = sgrwini')
       if (sgrwini .ne. dmiss) then
       call prop_set(prop_ptr, 'grw', 'sgrwini'            , sgrwini,           'Initial groundwater level, if h_unsatini < 0')
       endif
    endif

    !
    ! Hydrology (takes over some functionality that used to be under [grw])
    !
    if (writeall .or. interceptionmodel /= DFM_HYD_NOINTERCEPT) then
       call prop_set(prop_ptr, 'hydrology', 'InterceptionModel', interceptionmodel, 'Interception model (0: none, 1: on, via layer thickness)')
    end if


   ! JRE -> aanvullen, kijken wat aangeleverd wordt
    if (writeall .or. jawave > 0) then
       call prop_set(prop_ptr, 'waves', 'Wavemodelnr',         jawave,         'Wave model nr. (0: none, 1: fetch/depth limited hurdlestive, 2: Young-Verhagen, 3: SWAN, 5: uniform, 6: SWAN-NetCDF')
       call prop_set(prop_ptr, 'waves', 'Rouwav',              rouwav,         'Friction model for wave induced shear stress: FR84 (default) or: MS90, HT91, GM79, DS88, BK67, CJ85, OY88, VR04')
       call prop_set(prop_ptr, 'waves', 'Gammax',              gammax,         'Maximum wave height/water depth ratio')
       call prop_set(prop_ptr, 'waves', 'uorbfac',             jauorb,         'Orbital velocities: 0=D3D style; 1=Guza style')
       call prop_set(prop_ptr, 'waves', 'jahissigwav',         jahissigwav,    '1: sign wave height on his output; 0: hrms wave height on his output. Default=1.')
       call prop_set(prop_ptr, 'waves', 'jamapsigwav',         jamapsigwav,    '1: sign wave height on map output; 0: hrms wave height on map output. Default=0 (legacy behaviour).')
       call prop_set(prop_ptr, 'waves', 'hminlw',              hminlw,         'Cut-off depth for application of wave forces in momentum balance')
       ! prop_set_logical is not yet implemented call prop_set(prop_ptr, 'waves', 'FlowWithoutWaves',    flowWithoutWaves, 'True: Do not use Wave data in the flow computations, it will only be passed through to D-WAQ')
       if (writeall .or. hwavuni .ne. 0d0) then
          call prop_set(prop_ptr, 'waves', 'Hwavuni'  ,        hwavuni,        'root mean square wave height (m)')
          call prop_set(prop_ptr, 'waves', 'Twavuni'  ,        twavuni,        'root mean square wave period (s)')
          call prop_set(prop_ptr, 'waves', 'Phiwavuni',        phiwavuni,      'root mean square wave direction, (deg), math convention')
       endif
       if (writeall .or. jawaveswartdelwaq .ne. 0) then
          call prop_set(prop_ptr, 'waves', 'WaveSwartDelwaq'  , jaWaveSwartDelwaq, 'if WaveSwartDelwaq == 1 .and. Tiwaq > 0 then increase tauwave to Delwaq with 0.5rho*fw*uorbuorb' )
       endif
       if (writeall .or. jawave == 1 .or. jawave == 2) then
          call prop_set(prop_ptr, 'waves', 'Tifetchcomp'      , Tifetch,           'Time interval fetch comp (s) in wavemodel 1,2')
       endif
       if (writeall .or. kmx>0) then
          call prop_set(prop_ptr, 'waves', '3Dstokesprofile'     , jawaveStokes    ,'Stokes profile. 0: no, 1:uniform over depth, 2: 2nd order Stokes theory; 3: 2, with vertical stokes gradient in adve ')
          call prop_set(prop_ptr, 'waves', '3Dwavestreaming'     , jawavestreaming ,'Influence of wave streaming. 0: no, 1: added to adve                                                                 ')
          call prop_set(prop_ptr, 'waves', '3Dwaveboundarylayer' , jawavedelta     ,'Boundary layer formulation. 1: Sana                                                                                  ')
       endif

    endif

    if (writeall .or. jasedtrails>0) then
       call prop_set(prop_ptr, 'sedtrails', 'SedtrailsGrid',  trim(md_sedtrailsfile), 'Grid file for sedtrails output locations on corners')
       call prop_set(prop_ptr, 'sedtrails', 'SedtrailsAnalysis',  trim(sedtrails_analysis), 'Sedtrails analysis. Should be all, transport, flowvelocity or soulsby.')
       ti_st_array(1) = ti_st
       ti_st_array(2) = ti_sts
       ti_st_array(3) = ti_ste
       call prop_set(prop_ptr, 'sedtrails', 'SedtrailsInterval', ti_st_array, 'Sedtrails output times (s), interval, starttime, stoptime (s), if starttime, stoptime are left blank, use whole simulation period')
       call prop_set(prop_ptr, 'sedtrails', 'SedtrailsOutputFile', trim(md_avgsedtrailsfile), 'Sedtrails time-avgd output file')
    endif   

    ! Time
    call prop_set(prop_ptr, 'time', 'RefDate',                 refdat,         'Reference date (yyyymmdd)')
    call prop_set(prop_ptr, 'time', 'Tzone',                   Tzone,          'Time zone assigned to input time series')
    call prop_set(prop_ptr, 'time', 'DtUser',                  dt_user,        'Time interval (s) for external forcing update')
    if (writeall .or. dt_nodal > 0d0) then
        call prop_set(prop_ptr, 'time', 'DtNodal',             dt_nodal,       'Time interval (s) for updating nodal factors in astronomical boundary conditions')
    endif
    call prop_set(prop_ptr, 'time', 'DtMax',                   dt_max,         'Maximum computation timestep (s)')
    call prop_set(prop_ptr, 'time', 'Dtfacmax',                dtfacmax,       'Max timestep increase factor ( )')
    call prop_set(prop_ptr, 'time', 'DtInit',                  dt_init,        'Initial computation timestep (s)')

    call prop_set(prop_ptr, 'time', 'Timestepanalysis',        jatimestepanalysis, '0=no, 1=see file *.steps')

    if (writeall .or. ja_timestep_auto .ne. 1) then
        call prop_set(prop_ptr, 'time', 'AutoTimestep',  ja_timestep_auto, '0 = no, 1 = 2D (hor. out), 3=3D (hor. out), 5 = 3D (hor. inout + ver. inout), smallest dt')
    endif
    if (writeall .and. ja_timestep_auto_visc .ne. 1) then
        call prop_set(prop_ptr, 'time', 'Autotimestepvisc' , ja_timestep_auto_visc, '0 = no, 1 = yes (Time limitation based on explicit diffusive term)' )
    endif

    if (writeall .or. ja_timestep_nostruct .ne. 0) then
        call prop_set(prop_ptr, 'time', 'AutoTimestepNoStruct' , ja_timestep_nostruct, '0 = no, 1 = yes (Exclude structure links (and neighbours) from time step limitation)' )
    endif

    if (writeall .or. ja_timestep_noqout /= 1) then
       call prop_set(prop_ptr, 'time', 'AutoTimestepNoQout' , ja_timestep_noqout, '0 = no, 1 = yes (Exclude negative qin terms from time step limitation)' )
    endif

    call prop_set(prop_ptr, 'time', 'Tunit',                   md_tunit,        'Time unit for start/stop times (D, H, M or S)')
    ! Also in readMDU, but Interacter may have changed md_tunit
    select case (md_tunit)
    case('D')
        tfac = 3600d0*24d0
    case('H')
        tfac = 3600d0
    case('M')
        tfac = 60d0
    case default
        tfac = 1d0
    end select
    call prop_set(prop_ptr, 'time', 'TStart',             tstart_user/tfac,       'Start time w.r.t. RefDate (in TUnit)')
    call prop_set(prop_ptr, 'time', 'TStop',              tstop_user/tfac,        'Stop  time w.r.t. RefDate (in TUnit)')

    if (len_trim(Startdatetime) > 0) then
    call prop_set(prop_ptr, 'time', 'Startdatetime', trim(Startdatetime),  'Computation Startdatetime (yyyymmddhhmmss), when specified, overrides Tstart')
    endif

    if (len_trim(Stopdatetime) > 0) then
    call prop_set(prop_ptr, 'time', 'Stopdatetime',  trim(Stopdatetime),   'Computation Stopdatetime  (yyyymmddhhmmss), when specified, overrides Tstop')
    endif

    if (writeall .or. Dt_UpdateRoughness /= 86400d0) then
       call prop_set (prop_ptr, 'time', 'UpdateRoughnessInterval', Dt_UpdateRoughness, 'Update interval for time dependent roughness parameters (in s)')
    end if

! Restart settings
    call prop_set(prop_ptr, 'restart', 'RestartFile',     trim(md_restartfile) ,  'Restart netcdf-file, either *_rst.nc or *_map.nc')
    call prop_set(prop_ptr, 'restart', 'RestartDateTime', trim(restartdatetime),  'Restart date and time (yyyymmddhhmmss) when restarting from *_map.nc')
    call prop_set(prop_ptr, 'restart', 'RstIgnoreBl',     jarstignorebl,          'Flag indicating whether bed level from restart should be ignored (0=no (default), 1=yes)')

! External forcings
    call prop_set(prop_ptr, 'external forcing', 'ExtForceFile',    trim(md_extfile),     'Old format for external forcings file *.ext, link with tim/cmp-format boundary conditions specification')
    call prop_set(prop_ptr, 'external forcing', 'ExtForceFileNew', trim(md_extfile_new), 'New format for external forcings file *.ext, link with bc-format boundary conditions specification')
    if (writeall .or. jarain > 0) then
       call prop_set(prop_ptr, 'external forcing', 'Rainfall', jarain, 'Include rainfall, (0=no, 1=yes)')
    end if
    if (writeall .or. jaQext > 0) then
       call prop_set(prop_ptr, 'external forcing', 'QExt', jaQext, 'Include user Qin/out, externally provided, (0=no, 1=yes)')
    end if
    if (writeall .or. jaevap > 0) then
       call prop_set(prop_ptr, 'external forcing', 'Evaporation', jaevap, 'Include evaporation in water balance, (0=no, 1=yes)')
    end if
    if (writeall .or. jawind > 0) then
       call prop_set(prop_ptr, 'external forcing', 'WindExt', jawind, 'Include wind, externally provided, (0=no, 1=reserved for EC, 2=yes)')
    end if

    if (nmode .ne. 0) then
       call prop_set ( prop_ptr, 'equatorial', 'Ampfreeleft'     , amm)
       call prop_set ( prop_ptr, 'equatorial', 'Ampfreeright'    , app)
       call prop_set ( prop_ptr, 'equatorial', 'Ampforcedzerofr' , ztyp)
       call prop_set ( prop_ptr, 'equatorial', 'Nmode' , Nmode)
       call prop_set ( prop_ptr, 'equatorial', 'Nfreq' , Nfreq)
    endif

 ! Trachytopes
 ! Trachytopes are read in rdtrt() reusing the functionality of Delft3D, and not in the usual way.
 ! MK: added reading the file names and DtTrt into the 'normal' reading for partitioning
    if (writeall .or. jatrt == 1) then
        call prop_set(prop_ptr, 'trachytopes', 'TrtRou'          , trim(md_trtrfile)                               , 'Include alluvial and vegetation roughness (trachytopes) (Y: yes, N: no)' )
        call prop_set(prop_ptr, 'trachytopes', 'TrtDef'          , trim(md_trtdfile)    , 'File (*.ttd) including trachytope definitions')
        call prop_set(prop_ptr, 'trachytopes', 'TrtL'            , trim(md_trtlfile) , 'File (*.arl) including distribution of trachytope definitions')
        call prop_set(prop_ptr, 'trachytopes', 'DtTrt'           , dt_trach               , 'Trachytope roughness update time interval (s)')
        call prop_set(prop_ptr, 'trachytopes', 'TrtMxR'          , md_mxrtrach          , 'Maximum recursion level for composite trachytope definitions')
        call prop_set(prop_ptr, 'trachytopes', 'TrtCll'          , trim(md_trtcllfile)  , 'Calibration factor file for roughness from trachytopes (see also [calibration] block)')
        call prop_set(prop_ptr, 'trachytopes', 'TrtMnH'          , md_mnhtrach          , 'Minimum water depth for roughness computations')
        call prop_set(prop_ptr, 'trachytopes', 'TrtMth'          , md_mthtrach          , 'Area averaging method, (1=Nikuradse k based, 2=Chezy C based (parallel and serial))')
    end if

 ! Calibration factor
    if (writeall .or. jacali == 1) then
        call prop_set(prop_ptr, 'calibration', 'UseCalibration'   , jacali              , 'Activate calibration factor friction multiplier (1 = yes, 0 = no)')    ! Could be updated to check if both strings are empty or filled
        call prop_set(prop_ptr, 'calibration', 'DefinitionFile'   , trim(md_cldfile)    , 'File (*.cld) containing calibration definitions')
        call prop_set(prop_ptr, 'calibration', 'AreaFile'         , trim(md_cllfile)    , 'File (*.cll) containing area distribution of calibration definitions')
    end if

! Output
    call prop_set(prop_ptr, 'output', 'OutputDir',   trim(md_OutputDir),    'Output directory of map-, his-, rst-, dat- and timings-files, default: DFM_OUTPUT_<modelname>. Set to . for current dir.')
    call prop_set(prop_ptr, 'output', 'FlowGeomFile',trim(md_flowgeomfile), 'Flow geometry NetCDF *_flowgeom.nc')

    call prop_set(prop_ptr, 'output', 'ObsFile',     trim(md_obsfile), 'Points file *.xyn with observation stations with rows x, y, station name')
    call prop_set(prop_ptr, 'output', 'CrsFile',     trim(md_crsfile), 'Polyline file *_crs.pli defining observation cross sections')
    call prop_set(prop_ptr, 'output', 'RugFile',     trim(md_rugfile), 'Polyline file *_rug.pli defining runup gauges')
    call prop_set(prop_ptr, 'output', 'FouFile',     trim(md_foufile), 'Fourier analysis input file *.fou')
    call prop_set(prop_ptr, 'output', 'FouUpdateStep', md_fou_step,    'Fourier update step type: 0=every user time step, 1=every computational timestep, 2=same as history output.')

    call prop_set(prop_ptr, 'output', 'HisFile',     trim(md_hisfile), 'HisFile name *_his.nc')
    call prop_set(prop_ptr, 'output', 'MapFile',     trim(md_mapfile), 'MapFile name *_map.nc')

    ti_his_array(1) = ti_his
    ti_his_array(2) = ti_hiss
    ti_his_array(3) = ti_hise
    call prop_set(prop_ptr, 'output', 'HisInterval', ti_his_array, 'History times (s), interval, starttime, stoptime (s), if starttime, stoptime are left blank, use whole simulation period')
    call prop_set(prop_ptr, 'output', 'XLSInterval', ti_xls,           'Interval (s) XLS history' )

    ti_map_array(1) = ti_map
    ti_map_array(2) = ti_maps
    ti_map_array(3) = ti_mape
    call prop_set(prop_ptr, 'output', 'MapInterval', ti_map_array, 'Map times (s), interval, starttime, stoptime (s), if starttime, stoptime are left blank, use whole simulation period')

    ti_rst_array(1) = ti_rst
    ti_rst_array(2) = ti_rsts
    ti_rst_array(3) = ti_rste
    call prop_set(prop_ptr, 'output', 'RstInterval', ti_rst_array, 'Restart times (s), interval, starttime, stoptime (s), if starttime, stoptime are left blank, use whole simulation period')
    call prop_set(prop_ptr, 'output', 'MbaInterval', ti_mba, 'Mass balance area output interval (s)')

    call prop_set(prop_ptr, 'output', 'MbaWriteCsv', jambawritecsv, 'Write mass balance area output to a csv-file (1: yes, 0: no)')

    call prop_set(prop_ptr, 'output', 'MbaLumpFromToMba', jambalumpmba, 'Lump MBA from/to other areas mass balance terms (1: yes, 0: no)')
    call prop_set(prop_ptr, 'output', 'MbaLumpBoundaries', jambalumpbnd, 'Lump MBA boundary mass balance terms (1: yes, 0: no)')
    call prop_set(prop_ptr, 'output', 'MbaLumpSourceSinks', jambalumpsrc, 'Lump MBA source/sink mass balance terms (1: yes, 0: no)')
    call prop_set(prop_ptr, 'output', 'MbaLumpProcesses', jambalumpproc, 'Lump MBA processes mass balance terms (1: yes, 0: no)')

!    call prop_set(prop_ptr, 'output', 'WaqFileBase', trim(md_waqfilebase), 'Basename (without extension) for all Delwaq files to be written.')
    call prop_set(prop_ptr, 'output', 'WaqOutputDir',   trim(md_waqoutputdir),    'Output directory of WAQ communication files (flowgeom, vol, flo, etc.), default: DFM_DELWAQ_<modelname>. Set to . for current dir.')

    ti_waq_array(1) = ti_waq
    ti_waq_array(2) = ti_waqs
    ti_waq_array(3) = ti_waqe
    call prop_set(prop_ptr, 'output', 'WaqInterval', ti_waq_array,         'DELWAQ output times, given as "interval" "start period" "end period" (s)')
    call prop_set(prop_ptr, 'output', 'WaqHorAggr',  trim(md_waqhoraggr),  'DELWAQ output horizontal aggregation file (*.dwq)')
    call prop_set(prop_ptr, 'output', 'WaqVertAggr', trim(md_waqvertaggr), 'DELWAQ output vertical aggregation file (*.vag)')

    ti_classmap_array = [ti_classmap, ti_classmaps, ti_classmape]
    call prop_set(prop_ptr, 'output', 'ClassMapInterval', ti_classmap_array, 'Class map times (s), interval, starttime, stoptime (s), if starttime, stoptime are left blank, use whole simulation period')
    call prop_set(prop_ptr, 'output', 'ClassMapFile',     trim(md_classmapfile), 'ClassMapFile name *_clm.nc')
    if (allocated(map_classes_s1)) then
       call prop_set(prop_ptr, 'output', 'WaterlevelClasses', map_classes_s1, 'Class map''s list of class values for water levels')
    else if (writeall) then
       call prop_set(prop_ptr, 'output', 'WaterlevelClasses', '', 'Class map''s list of class values for water levels')
    end if
    if (allocated(map_classes_hs)) then
       call prop_set(prop_ptr, 'output', 'WaterDepthClasses', map_classes_hs, 'Class map''s list of class values for water depths')
    else if (writeall) then
       call prop_set(prop_ptr, 'output', 'WaterDepthClasses', '', 'Class map''s list of class values for water depths')
    end if
    if (allocated(map_classes_ucmag)) then
       call prop_set(prop_ptr, 'output', 'VelocityMagnitudeClasses', map_classes_ucmag, 'Class map''s list of class values for velocity magnitudes')
    else if (writeall) then
       call prop_set(prop_ptr, 'output', 'VelocityMagnitudeClasses', '', 'Class map''s list of class values for velocity magnitudes')
    end if
    if (map_classes_ucdirstep > 0d0) then
       call prop_set(prop_ptr, 'output', 'VelocityDirectionClassesInterval', map_classes_ucdirstep, 'Class map''s step size of class values for velocity direction')
    else if (writeall) then
       call prop_set(prop_ptr, 'output', 'VelocityDirectionClassesInterval', '', 'Class map''s step size of class values for velocity direction')
    end if

    call prop_set(prop_ptr, 'output', 'StatsInterval', ti_stat,        'Screen step output interval in seconds simulation time, if negative in seconds wall clock time')

  ! call prop_set(prop_ptr, 'output', 'SnapshotDir', trim(md_snapshotdir), 'Directory where snapshots/screendumps are saved.')

    call prop_set(prop_ptr, 'output', 'TimingsInterval', ti_timings, 'Timings statistics output interval')
    helptxt = ' '
    write (helptxt,'(i0,a1,a1)'), int(ti_split), ' ', ti_split_unit
    call prop_set(prop_ptr, 'output', 'TimeSplitInterval', trim(helptxt), 'Time splitting interval, after which a new output file is started. value+unit, e.g. ''1 M'', valid units: Y,M,D,h,m,s.')


    write(helptxt,"('Map file format ')")
    do i=1,NUMFORMATS
       write(helptxt, "(A, ', ', I0, ': ', A)") trim(helptxt), i, trim(SFORMATNAMES(i))
    end do
    call prop_set(prop_ptr, 'output', 'MapFormat', md_mapformat, trim(helptxt))

    call prop_set(prop_ptr, 'output', 'NcFormat',  md_ncformat, 'Format for all NetCDF output files (3: classic, 4: NetCDF4+HDF5)')

    if (writeall .or. unc_nounlimited /= 0) then
       call prop_set(prop_ptr, 'output', 'NcNoUnlimited',  unc_nounlimited, 'Write full-length time-dimension instead of unlimited dimension (1: yes, 0: no). (Might require NcFormat=4.)')
    end if

    if (writeall .or. unc_noforcedflush /= 0) then
       call prop_set(prop_ptr, 'output', 'NcNoForcedFlush',  unc_noforcedflush, 'Do not force flushing of map-like files every output timestep (1: yes, 0: no).')
    end if

    if (writeall .or. unc_writeopts /= UG_WRITE_NOOPTS) then
       if (iand(unc_writeopts, UG_WRITE_LATLON) == UG_WRITE_LATLON) then
          ibuf = 1
       else
          ibuf = 0
       end if
       call prop_set(prop_ptr, 'output', 'NcWriteLatLon', ibuf, 'Write extra lat-lon coordinates for all projected coordinate variables in each NetCDF file (for CF-compliancy).')
    end if
    if (writeall .or. len_trim(unc_metadatafile) > 0) then
       call prop_set(prop_ptr, 'output', 'MetaDataFile', unc_metadatafile, 'Metadata NetCDF file with user-defined global dataset attributes (*_meta.nc).')
    end if
    if (writeall .or. unc_uuidgen /= 0) then
       call prop_set(prop_ptr, 'output', 'GenerateUUID', unc_uuidgen, 'Generate UUID as unique dataset identifier and include in output NetCDF files.')
    end if

    if (writeall .or. jahisbal /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_balance', jahisbal, 'Write mass balance totals to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahissourcesink /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_sourcesink', jahissourcesink, 'Write sources-sinks statistics to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahiscgen /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_structure_gen', jahiscgen, 'Write general structure parameters to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahiscdam /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_structure_dam', jahiscdam, 'Write dam parameters to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahispump /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_structure_pump', jahispump, 'Write pump parameters to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahisgate /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_structure_gate', jahisgate, 'Write gate parameters to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahisweir /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_structure_weir', jahisweir, 'Write weir parameters to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahisorif /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_structure_orifice', jahisorif, 'Write orifice parameters to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahisbridge /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_structure_bridge', jahisbridge, 'Write bridge parameters to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahisculv /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_structure_culvert', jahisculv, 'Write culvert parameters to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahisdambreak /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_structure_damBreak', jahisdambreak, 'Write dam break parameters to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahisuniweir /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_structure_uniWeir', jahisuniweir, 'Write universal weir parameters to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahiscmpstru /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_structure_compound', jahiscmpstru, 'Write compound structure parameters to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahislongculv /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_structure_longculvert', jahislongculv, 'Write long culvert parameters to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahistur /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_turbulence', jahistur, 'Write k, eps and vicww to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahiswind /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_wind', jahiswind, 'Write wind velocities to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahisrain /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_rain', jahisrain, 'Write precipitation to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahisinfilt /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_infiltration', jahisinfilt, 'Write infiltration to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahistem /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_temperature', jahistem, 'Write temperature to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahiswav /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_waves', jahiswav, 'Write wave data to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahisheatflux /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_heat_fluxes', jahisheatflux, 'Write heat fluxes to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahissal /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_salinity', jahissal, 'Write salinity to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahisrho /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_density', jahisrho, 'Write density to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahiswatlev /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_waterlevel_s1', jahiswatlev, 'Write water level to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahisbedlev == 0) then
       call prop_set(prop_ptr, 'output', 'Wrihis_bedlevel', jahisbedlev, 'Write bed level to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahiswatdep /= 0) then
       call prop_set(prop_ptr, 'output', 'Wrihis_waterdepth', jahiswatdep, 'Write water depth to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahisvelvec /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_velocity_vector', jahisvelvec, 'Write velocity vectors to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahisww /= 0) then
       call prop_set(prop_ptr, 'output', 'Wrihis_upward_velocity_component', jahisww, 'Write upward velocity to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahissed /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_sediment', jahissed, 'Write sediment transport to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahisconst /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_constituents', jahisconst, 'Write tracers to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahiszcor /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_zcor', jahiszcor, 'Write vertical coordinates to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahislateral /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_lateral', jahislateral, 'Write lateral data to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahistaucurrent /= 1) then
       call prop_set(prop_ptr, 'output', 'Wrihis_taucurrent', jahistaucurrent, 'Write mean bed shear stress to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahisvelocity /= 0) then
       call prop_set(prop_ptr, 'output', 'Wrihis_velocity', jahisvelocity, 'Write velocity magnitude to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jahisdischarge /= 0) then
       call prop_set(prop_ptr, 'output', 'Wrihis_discharge', jahisdischarge, 'Write discharge magnitude to his file (1: yes, 0: no)' )
    endif
    if (writeall .or. jamaps0 /= 1) then
        call prop_set(prop_ptr, 'output', 'Wrimap_waterlevel_s0', jamaps0, 'Write water levels for previous time step to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamaps1 /= 1) then
        call prop_set(prop_ptr, 'output', 'Wrimap_waterlevel_s1', jamaps1, 'Write water levels to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamapFlowAnalysis /= 0) then
       call prop_set(prop_ptr, 'output', 'Wrimap_flow_analysis', jamapFlowAnalysis, 'Write flow analysis data to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamapevap /= 0) then
        call prop_set(prop_ptr, 'output', 'Wrimap_evaporation', jamapevap, 'Write evaporation to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamapvol1 > 0) then
        call prop_set(prop_ptr, 'output', 'Wrimap_volume1', jamapvol1, 'Write volumes to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamaphs /= 1) then
        call prop_set(prop_ptr, 'output', 'Wrimap_waterdepth', jamaphs, 'Write water depths to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamaphu > 0) then
        call prop_set(prop_ptr, 'output', 'Wrimap_waterdepth_hu', jamaphu, 'Write water depths on u-points to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamapanc > 0) then
        call prop_set(prop_ptr, 'output', 'Wrimap_ancillary_variables', jamapanc, 'Write ancillary_variables attributes to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamapau > 0) then
        call prop_set(prop_ptr, 'output', 'Wrimap_flowarea_au', jamapau, 'Write flow areas au to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamapu0 /= 1) then
        call prop_set(prop_ptr, 'output', 'Wrimap_velocity_component_u0', jamapu0, 'Write velocity component for previous time step to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamapu1 /= 1) then
        call prop_set(prop_ptr, 'output', 'Wrimap_velocity_component_u1', jamapu1, 'Write velocity component to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamapucvec /= 1) then
        call prop_set(prop_ptr, 'output', 'Wrimap_velocity_vector', jamapucvec, 'Write cell-center velocity vectors to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamapucmag /= 1) then
        call prop_set(prop_ptr, 'output', 'Wrimap_velocity_magnitude', jamapucmag, 'Write cell-center velocity vector magnitude to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamapucqvec /= 0) then
        call prop_set(prop_ptr, 'output', 'Wrimap_velocity_vectorq', jamapucqvec, 'Write cell-center velocity vectors (discharge-based) to map file (1: yes, 0: no)')
    endif
    if(writeall .or. jamapww1 /= 1) then
        call prop_set(prop_ptr, 'output', 'Wrimap_upward_velocity_component', jamapww1, 'Write upward velocity component on cell interfaces (1: yes, 0: no)')
    endif
    if (writeall .or. jamaprho /= 1) then
        call prop_set(prop_ptr, 'output', 'Wrimap_density_rho', jamaprho, 'Write flow density to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamapviu /= 1) then
        call prop_set(prop_ptr, 'output', 'Wrimap_horizontal_viscosity_viu', jamapviu, 'Write horizontal viscosity to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamapdiu /= 1) then
        call prop_set(prop_ptr, 'output', 'Wrimap_horizontal_diffusivity_diu', jamapdiu, 'Write horizontal diffusivity to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamapq1 /= 1) then
        call prop_set(prop_ptr, 'output', 'Wrimap_flow_flux_q1', jamapq1, 'Write flow flux to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamapq1main /= 0) then
        call prop_set(prop_ptr, 'output', 'Wrimap_flow_flux_q1_main', jamapq1main, 'Write flow flux in main channel to map file (1: yes, 0: no)')
    endif
    if (jamapfw /= 0) then
        call prop_set(prop_ptr, 'output', 'Wrimap_fixed_weir_energy_loss', jamapfw, 'Write fixed weir energy loss to map file (1: yes, 0: no)')
    endif
    if ( jasecflow > 0 .and. (writeall .or. jamapspir /= 1)) then
        call prop_set(prop_ptr, 'output', 'Wrimap_spiral_flow', jamapspir, 'Write spiral flow to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamapnumlimdt /= 1) then
        call prop_set(prop_ptr, 'output', 'Wrimap_numlimdt', jamapnumlimdt, 'Write the number times a cell was Courant limiting to map file (1: yes, 0: no). (Consider using Wrimap_flow_analysis instead.)')
    endif
    if (writeall .or. jamaptaucurrent /= 1) then
        call prop_set(prop_ptr, 'output', 'Wrimap_taucurrent', jamaptaucurrent, 'Write the shear stress to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamap_chezy_elements /= 0) then
        call prop_set(prop_ptr, 'output', 'Wrimap_chezy', jamap_chezy_elements, 'Write the chezy values in flow elements to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamap_chezy_links /= 0) then
        call prop_set(prop_ptr, 'output', 'Wrimap_chezy_on_flow_links', jamap_chezy_links, 'Write the chezy values on flow links to map file (1: yes, 0: no)')
    endif
    if (writeall .or. jamap_chezy_input /= 0) then
        call prop_set(prop_ptr, 'output', 'Wrimap_input_roughness', jamap_chezy_input, 'Write the input roughness on flow links to map file (1: yes, 0: no)')
    endif
    if(jasal > 0 .and. (writeall .or. jamapsal /= 1)) then
        call prop_set(prop_ptr, 'output', 'Wrimap_salinity', jamapsal, 'Write salinity to map file (1: yes, 0: no)')
    endif
    if(jatem > 0 .and. (writeall .or. jamaptem /= 1)) then
        call prop_set(prop_ptr, 'output', 'Wrimap_temperature', jamaptem, 'Write temperature to map file (1: yes, 0: no)')
    endif
    if ((jased > 0 .or. stm_included) .and. (writeall .or. jamapsed /= 1) ) then
        call prop_set(prop_ptr, 'output', 'Wrimap_sediment', jamapsed, 'Write sediment fractions to map file (1: yes, 0: no)')
    endif
    if (kmx > 0 .or. (writeall .or. jamaptur /= 1) ) then
        call prop_set(prop_ptr, 'output', 'Wrimap_turbulence', jamaptur, 'Write vicww, k and eps to map file (1: yes, 0: no)')
    endif
    if(ITRA1 > 0 .and. (writeall .or. jamapconst /= 1) ) then
        call prop_set(prop_ptr, 'output', 'Wrimap_constituents', jamapconst, 'Write constituents to map file (1: yes, 0: no)')
    endif
    if(jatrt > 0 .and. (writeall .or. jamaptrachy /= 1)) then
        call prop_set(prop_ptr, 'output', 'Wrimap_trachytopes', jamaptrachy, 'Write trachytope roughnesses to map file (1: yes, 0: no)')
    endif
    if(jacali > 0 .and. (writeall .or. jamapcali /= 1)) then
        call prop_set(prop_ptr, 'output', 'Wrimap_calibration', jamapcali, 'Write roughness calibration factors to map file (1: yes, 0: no)')
    endif
    if(writeall .or. jamaprain /= 1) then
        call prop_set(prop_ptr, 'output', 'Wrimap_rain', jamaprain, 'Write rainfall rates to map file (1: yes, 0: no)')
    endif
    if (writeall .or. (interceptionmodel /= DFM_HYD_NOINTERCEPT .and. jamapicept /= 0)) then
        call prop_set(prop_ptr, 'output', 'Wrimap_interception', jamapicept, 'Write interception to map file (1: yes, 0: no)')
    endif
    if(writeall .or. jamapwind /= 1) then
        call prop_set(prop_ptr, 'output', 'Wrimap_wind', jamapwind, 'Write wind velocities to map file (1: yes, 0: no)')
    endif
    if(writeall .or. jamapwindstress /= 0) then
        call prop_set(prop_ptr, 'output', 'Wrimap_windstress', jamapwindstress, 'Write wind stress to map file (1: yes, 0: no)')
    endif
    if(writeall .or. jatekcd /= 0) then
        call prop_set(prop_ptr, 'output', 'Writek_CdWind', jatekcd, 'Write wind friction coeffs to tek file (1: yes, 0: no)')
    endif
    if (jatem > 1 .and. (writeall .or. jamapheatflux > 0)) then
       call prop_set(prop_ptr, 'output', 'Wrimap_heat_fluxes', jamapheatflux, 'Write heat fluxes to map file (1: yes, 0: no)' )
    endif

    if(writeall .or. jamapdtcell /= 0) then
        call prop_set(prop_ptr, 'output', 'Wrimap_DTcell', jamapdtcell, 'Write time step per cell based on CFL (1: yes, 0: no)')
    endif


    if (writeall .or. epswetout /= 0.1d0) then
       call prop_set(prop_ptr, 'output', 'Wrimap_wet_waterdepth_threshold', epswetout, 'Waterdepth threshold above which a grid point counts as ''wet''. Used for Wrimap_time_water_on_ground.')
    end if

    if (writeall .or. jamapTimeWetOnGround /= 0) then
        call prop_set(prop_ptr, 'output', 'Wrimap_time_water_on_ground', jamapTimeWetOnGround, 'Write cumulative time when water is above ground level to map file, only for 1D nodes (1: yes, 0: no)')
    endif

    if (writeall .or. jamapFreeboard /= 0) then
       call prop_set(prop_ptr, 'output', 'Wrimap_freeboard', jamapFreeboard, 'Write freeboard to map file, only for 1D nodes (1: yes, 0: no)')
    end if

    if (writeall .or. jamapDepthOnGround /= 0) then
        call prop_set(prop_ptr, 'output', 'Wrimap_waterdepth_on_ground', jamapDepthOnGround, 'Write waterdepth that is above ground level to map file, only for 1D nodes (1: yes, 0: no)')
    endif

    if (writeall .or. jamapVolOnGround /= 0) then
        call prop_set(prop_ptr, 'output', 'Wrimap_volume_on_ground', jamapVolOnGround, 'Write volume that is above ground level to map file, only for 1D nodes (1: yes, 0: no)')
    endif

    if (writeall .or. jamapTotalInflow1d2d /= 0) then
        call prop_set(prop_ptr, 'output', 'Wrimap_total_net_inflow_1d2d', jamapTotalInflow1d2d, 'Write current total 1d2d net inflow (discharge) and cumulative total 1d2d net inflow (volume) to map file, only for 1D nodes (1: yes, 0: no)')
    endif

    if (writeall .or. jamapTotalInflowLat /= 0) then
        call prop_set(prop_ptr, 'output', 'Wrimap_total_net_inflow_lateral', jamapTotalInflowLat, 'Write current total lateral net inflow (discharge) and cumulative total net lateral inflow (volume) to map file, only for 1D nodes (1: yes, 0: no)')
    endif

    if (writeall .or. jamapS1Gradient /= 0) then
        call prop_set(prop_ptr, 'output', 'Wrimap_water_level_gradient', jamapS1Gradient, 'Write water level gradient to map file, only on 1D links (1: yes, 0: no)')
    endif

    if (jatidep > 0 .and. (writeall .or. jamaptidep /= 1)) then
       call prop_set(prop_ptr, 'output', 'Wrimap_tidal_potential', jamaptidep, 'Write tidal potential to map file (1: yes, 0: no)')
    end if

    if (jaselfal > 0 .and. (writeall .or. jamapselfal /= 1)) then
       call prop_set(prop_ptr, 'output', 'Wrimap_sal_potential', jamapselfal, 'Write self attraction and loading potential to map file (1: yes, 0: no)')
    end if

    if (jaFrcInternalTides2D > 0 .and. (writeall .or. jamapIntTidesDiss /=1 )) then
       call prop_set(prop_ptr, 'output', 'Wrimap_internal_tides_dissipation', jamapIntTidesDiss, 'Write internal tides dissipation to map file (1: yes, 0: no)')
    end if

    call prop_set(prop_ptr, 'output', 'Writepart_domain', japartdomain, 'Write partition domain info. for postprocessing')

    if ((jasal > 0 .or. jatem > 0 .or. jased > 0) .and. (writeall .or. jaRichardsononoutput > 0)) then
        call prop_set(prop_ptr, 'output', 'Richardsononoutput', jaRichardsononoutput, 'Write Richardson numbers (1: yes, 0: no)' )
    endif

    if (writeall .or. jashp_crs /= 0) then
       call prop_set(prop_ptr, 'output', 'Wrishp_crs', jashp_crs, 'Write grid-snapped cross sections to shapefile (1: yes, 0: no)')
    end if
    if (writeall .or. jashp_obs /= 0) then
       call prop_set(prop_ptr, 'output', 'Wrishp_obs', jashp_obs, 'Write grid-snapped observation stations to shapefile (1: yes, 0: no)')
    end if
    if (writeall .or. jashp_weir /= 0) then
       call prop_set(prop_ptr, 'output', 'Wrishp_weir', jashp_weir, 'Write grid-snapped weirs to shapefile (1: yes, 0: no)')
    end if
    if (writeall .or. jashp_thd /= 0) then
       call prop_set(prop_ptr, 'output', 'Wrishp_thd', jashp_thd, 'Write grid-snapped thin dams to shapefile (1: yes, 0: no)')
    end if
    if (writeall .or. jashp_gate /= 0) then
       call prop_set(prop_ptr, 'output', 'Wrishp_gate', jashp_gate, 'Write grid-snapped gates to shapefile (1: yes, 0: no)')
    end if
    if (writeall .or. jashp_emb /= 0) then
       call prop_set(prop_ptr, 'output', 'Wrishp_emb', jashp_emb, 'Write grid-snapped 1d2d embankments to shapefile (1: yes, 0: no)')
    end if
    if (writeall .or. jashp_fxw /= 0) then
       call prop_set(prop_ptr, 'output', 'Wrishp_fxw', jashp_fxw, 'Write grid-snapped fixed weirs to shapefile (1: yes, 0: no)')
    end if
    if (writeall .or. jashp_src /= 0) then
       call prop_set(prop_ptr, 'output', 'Wrishp_src', jashp_src, 'Write grid-snapped source-sinks to shapefile (1: yes, 0: no)')
    end if
    if (writeall .or. jashp_pump /= 0) then
       call prop_set(prop_ptr, 'output', 'Wrishp_pump', jashp_pump, 'Write grid-snapped pumps to shapefile (1: yes, 0: no)')
    end if
    if( writeall .or. jashp_dry > 0 ) then
       call prop_set(prop_ptr, 'output', 'Wrishp_dryarea', jashp_dry, 'Write a shape file for dry areas')
    end if
    if( writeall .or. jashp_genstruc > 0 ) then
       call prop_set(prop_ptr, 'output', 'wrishp_genstruc', jashp_genstruc, 'Write a shape file for general structures')
    end if

    call prop_set(prop_ptr, 'output', 'WriteDFMinterpretedvalues', jaWriteDFMinterpretedvalues, 'Write DFMinterpretedvalues (1: yes, 0: no)' )

    call prop_set(prop_ptr, 'output', 'WriteDetailedTimers', jawriteDetailedTimers, 'Write detailed timers output file (1: yes, 0: no)' )

    call prop_set(prop_ptr, 'output', 'MapOutputTimeVector',  trim(md_mptfile), 'File (*.mpt) containing fixed map output times (s) w.r.t. RefDate')
    call prop_set(prop_ptr, 'output', 'FullGridOutput', jafullgridoutput, 'Full grid output mode for layer positions (0: compact, 1: full time-varying grid layer data)')
    call prop_set(prop_ptr, 'output', 'EulerVelocities', jaeulervel, 'Euler velocities output (0: GLM, 1: Euler velocities)')
    call prop_set(prop_ptr, 'output', 'Wrirst_bnd', jarstbnd, 'Write waterlevel, bedlevel and coordinates of boundaries to restart files')
    if (jamapbnd > 0 .or. writeall) then
       call prop_set(prop_ptr, 'output', 'Wrimap_bnd', jamapbnd, 'Write boundary points to map file (1: yes, 0: no)')
    endif
    if (jamapqin > 0 .or. writeall) then
      call prop_set(prop_ptr, 'output', 'Wrimap_Qin', jamapqin, 'Write sum of all influxes to map file (1: yes, 0: no)')
    endif
    if (jaeverydt > 0 .or. writeall) then
      call prop_set(prop_ptr, 'output', 'Wrimap_every_dt', jaeverydt, 'Write output to map file every dt, based on start and stop from MapInterval, 0=no (default), 1=yes')
    endif

    if ( get_japart() .or. writeall ) then
!      particles
       call prop_set_string(prop_ptr, 'particles', 'ParticlesFile', md_partfile, ' ')
       call prop_set_string(prop_ptr, 'particles', 'ParticlesReleaseFile', md_partrelfile, ' ')
       call prop_set_integer(prop_ptr, 'particles', 'AddTracer', md_partjatracer, 'add tracer (1) or not (other)')
       call prop_set_double (prop_ptr, 'particles', 'StartTime', md_partstarttime, 'starttime (if >0)')
       call prop_set_double (prop_ptr, 'particles', 'TimeStep', md_parttimestep, 'time step (>0) or every computational time step')
       call prop_set_integer(prop_ptr, 'particles', '3Dtype', md_part3Dtype, '3D type: depth averaged (0) or free surface (1)')
    end if


!  processes (WAQ)
   call prop_set_string (prop_ptr, 'processes', 'SubstanceFile', trim(md_subfile), 'substance file')
   call prop_set_string (prop_ptr, 'processes', 'AdditionalHistoryOutputFile', trim(md_ehofile), 'extra history output file')
   call prop_set_string (prop_ptr, 'processes', 'StatisticsFile', trim(md_sttfile), 'statistics file')
   call prop_set_double (prop_ptr, 'processes', 'ThetaVertical', md_thetav_waq, 'theta vertical for waq')
   call prop_set_double (prop_ptr, 'processes', 'DtProcesses', md_dt_waqproc, 'waq processes time step')
   call prop_set_double (prop_ptr, 'processes', 'DtMassBalance', md_dt_waqbal, 'waq mass balance output time step')
   call prop_set_integer(prop_ptr, 'processes', 'ProcessFluxIntegration', md_flux_int, 'Process fluxes integration option (1: WAQ, 2: D-Flow FM)')
   call prop_set_integer(prop_ptr, 'processes', 'Wriwaqbot3Doutput', md_wqbot3D_output, 'Write 3D water quality bottom variables (1: yes, 0: no)')
   call prop_set_double (prop_ptr, 'processes', 'VolumeDryThreshold', waq_vol_dry_thr, 'Volume below which segments are marked as dry. (m3)')
   call prop_set_double (prop_ptr, 'processes', 'DepthDryThreshold', waq_dep_dry_thr, 'Water depth below which segments are marked as dry. (m)')

    call datum(rundat)
    write(mout, '(a,a)') '# Generated on ', trim(rundat)
    write(mout, '(a,a)') '# ', trim(version_full)
    call getbranch_dflowfm(msgbuf)
    write(mout, '(a,a)') '# Source:', trim(msgbuf)
    call prop_write_inifile(mout, prop_ptr, istat)
    call tree_destroy(prop_ptr)

end subroutine writeMDUFilepointer


subroutine setmd_ident(filename)
use m_partitioninfo
USE MessageHandling
use string_module, only: get_dirsep
use unstruc_netcdf, only: unc_meta_md_ident

character(*),     intent(inout) :: filename !< Name of file to be read (in current directory or with full path).
                                            !! in case of parallel computing, the partition number is inserted.
integer                      :: L1, L2



! Set model identifier based on .mdu basename
L1 = index(filename, get_DIRSEP(),.true.) + 1
L2 = index(filename, '.', .true.)
if (L2 == 0) then
    md_ident = ' '
    md_ident_sequential = trim(md_ident)                  ! needed for parallel outputdir
else

    md_ident = filename(L1:L2-1) ! TODO: strip off path [AvD]

    md_ident_sequential = trim(md_ident)                  ! needed for parallel outputdir

    if ( JAMPI.eq.1 .and. numranks.gt.1 ) then
        md_ident = trim(md_ident) // '_' // sdmn          ! add rank number to ident
        filename = trim(md_ident) // filename(L2:)
    end if

   !  NOTE: The switch_dia_file() call is now in loadModel().
end if

! Pass a copy to unstruc_netcdf to avoid cyclic dependency.
unc_meta_md_ident = md_ident

end subroutine setmd_ident


!> Switched to the model-specific .dia file.
!! An .mdu must have been loaded, such that the possible model-configured
!! OutputDir has been read already.
subroutine switch_dia_file()
implicit none
integer                      :: mdia2, mdia, ierr
character(len=256)           :: rec
logical                      :: line_copied
external :: getmdia, setmdia

    call makedir(getoutputdir()) ! No problem if it exists already.

!   SPvdP : check status of file, mostly copied from inidia
    open (newunit=MDIA2, FILE = trim(getoutputdir())//trim(md_ident)//'.dia', action='readwrite', IOSTAT=IERR)

    line_copied = .false.
    if ( ierr.eq.0 ) then

       call getmdia(mdia)

       if ( mdia /= 0 ) then ! rename diagnostic file to md_ident.dia
          rewind(mdia)

10        read (mdia, '(a)', end = 20) rec
          write(mdia2,'(a)') trim(rec)
          line_copied = .true.
          goto 10

20        continue

          close(mdia)
       end if
       mdia    = mdia2

       call setmdia(mdia)
       call initMessaging(mdia)

       if (line_copied) WRITE(MDIA,*) 'Until here copy of previous diagnostic file'
    end if

end subroutine switch_dia_file

!> get output directory
function getoutputdir(dircat)
   use m_flowtimes
   use string_module, only: get_dirsep
   implicit none

   character(len=*), optional, intent(in) :: dircat !< (optional) The type of the directory: currently supported only 'waq'.
   character(len=255)         :: getoutputdir


   character(len=16) :: dircat_

   if (present(dircat)) then
      dircat_ = dircat
   else
      dircat_ = ''
   end if

   call datum2(rundat2)
   select case(trim(dircat_))
   case ('waq')
      if (len_trim(md_waqoutputdir) == 0) then
         getoutputdir = 'DFM_DELWAQ_'//trim(md_ident_sequential)//trim(rundat2)
      else
         getoutputdir = trim(md_waqoutputdir)//get_dirsep()
      end if

   case default
      if ( len_trim(md_outputdir)==0 ) then
   !     default
         if ( len_trim(md_ident_sequential) > 0 ) then
            getoutputdir = 'DFM_OUTPUT_'//trim(md_ident_sequential)//trim(rundat2)
         else
            getoutputdir = 'DFM_OUTPUT_'//trim(rundat2)
         end if
      else
         getoutputdir = trim(md_outputdir)//get_dirsep()
      end if
   end select

   return
end function getoutputdir

subroutine getOutputTimeArrays(ti_output, ti_outs, ti_out, ti_oute, success)

use m_flowtimes

implicit none

real(kind=hp), intent(in)  :: ti_output(3)
real(kind=hp), intent(out) :: ti_outs, ti_out, ti_oute
logical      , intent(inout)  :: success

if (success) then
    ti_out        = ti_output(1)
    if (ti_output(2).eq.0d0) then
        ti_outs   = tstart_user
    else
        ti_outs   = ti_output(2)
    endif
    if (ti_output(3).eq.0d0) then
        ti_oute   = tstop_user
    else
        ti_oute   = ti_output(3)
    endif
else
    ! ti_out stays at default, only set output start/end time to current simulation start/end
    ti_outs       = tstart_user
    ti_oute       = tstop_user
end if

end subroutine getOutputTimeArrays

   end module unstruc_model



