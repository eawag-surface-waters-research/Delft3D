========================================================================
    Fortran/C++ Library : "D-Flow Flexible Mesh" Project Overview
========================================================================

The Deltares can-do-everything Wizard has created this

	"D-Flow Flexible Mesh" 

install project for you as a starting point.

This file contains a summary of D-FlowFM project.

D-Flow Flexible Mesh (D-Flow FM) is a hydrodynamic simulation program developed by Deltares.
It is part of Deltares’ unique, fully integrated computer software suite for a multi-disciplinary
approach and 1D, 2D and 3D computations for coastal, river and estuarine areas. 
It can carry out simulations of hydrodynamic
flow, waves, water quality and ecology.

It has been designed for experts and non-experts alike. 
D-Flow FM is a multi-dimensional (1D, 2D and 3D) hydrodynamic (and transport) simulation
program which calculates non-steady flow and transport phenomena that result from tidal
and meteorological forcing on structured and unstructured, boundary fitted grids. 

The term Flexible Mesh in the name refers to the flexible combination of unstructured grids 
consisting of triangles, quadrangles, pentagons and hexagons. In 3D simulations the vertical 
grid is using the σ co-ordinate approach. As an alternative a fixed z layers approach is also 
possible. The 2D functionality in D-Flow FM has been released, while the functionality for 
3D and 1D is in development.

/////////////////////////////////////////////////////////////////////////////
Areas of application:
- Tide and wind-driven flows (i.e., storm surges).
- Stratified and density driven flows.
- River flow simulations.
- Rural channel networks.
- Rainfall runoff in urban environments.
- Simulation of tsunamis, hydraulic jumps, bores and flood waves.
- Fresh-water river discharges in bays.
- Salt intrusion.
- Cooling water intakes and waste water outlets.
- Transport of dissolved material and pollutants.
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
Standard features:
- Tidal forcing.
- The effect of the Earth’s rotation (Coriolis force).
- Density driven flows (pressure gradients terms in the momentum equations).
- Advection-diffusion solver included to compute density gradients.
- Space and time varying wind and atmospheric pressure.
- Advanced turbulence models to account for the vertical turbulent viscosity and diffusivity
based on the eddy viscosity concept. Four options are provided: 1) constant, 2) algebraic,
3) k-ε and 4) k-τ model.
- Time varying sources and sinks (e.g., river discharges).
- Simulation of the thermal discharge, effluent discharge and the intake of cooling water at
any location and any depth.
- Robust simulation of drying and flooding of inter-tidal flats and river winter beds.
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
Special features:
- Built-in automatic switch converting 2D bottom-stress coefficient to 3D coefficient.
- Built-in anti-creep correction to suppress artificial vertical diffusion and artificial flow due
to σ-grids.
- Heat exchange through the free water surface.
- Wave induced stresses and mass fluxes.
- Influence of waves on the bed shear stress.
- Optional facility to calculate the intensity of the spiral motion phenomenon in the flow
(e.g., in river bends) which is especially important in sedimentation and erosion studies
(for depth averaged — 2DH — computations only).
- Non-linear iterations in the solver can be enabled for accurate flooding results.
- Optional facility for tidal analysis of output parameters.
- Optional facility for special structures such as pumping stations, bridges, fixed weirs and
controllable barriers (1D, 2D and 3D)
- Default advection scheme suitable for various flow regimes, from bore propagation to eddy
shedding.
- Domain partitioning for parallelized runs on MPI-based High Performance Computing clusters.
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
Other notes:
- The 3D modelling is a β-functionality
- You can find the user manual here : 
	https://content.oss.deltares.nl/delft3d/manuals/D-Flow_FM_User_Manual.pdf
/////////////////////////////////////////////////////////////////////////////
