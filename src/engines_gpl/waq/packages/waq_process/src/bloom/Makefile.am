#-------------------------------------------------------------------------------
#   Makefile for delwaq
#
#   arjen.markus@deltares.nl
#   21 august 2008
#-------------------------------------------------------------------------------

include $(top_srcdir)/common.am
noinst_LTLIBRARIES = libbloom.la

#-------------------------------------------------------------------------------
#Includes
FMODULES += \
        -I$(top_builddir)/engines_gpl/waq/include \
         -I$(top_builddir)/engines_gpl/waq/packages/waq_process/src/waq_process \
         -I$(top_builddir)/utils_lgpl/deltares_common/packages/deltares_common/src

#-------------------------------------------------------------------------------
#Sources
libbloom_la_SOURCES = blclmort.f \
       blfile.f \
       blinit.f \
       blinpu.f \
       blmort.f \
       bloom.f \
       bloutc.f \
       blprim.f \
       bvect.f \
       change.f \
       chhelp.f \
       constr.f \
       cspgro.f \
       cspsto.f \
       cvrblm.f \
       dayeu.f \
       diel.f \
       dynrun.f \
       ebcalc.f \
       edit.f \
       exclud.f \
       fixinf.f \
       grazin.f \
       hdrblm.f \
       headin.f \
       iblbal.f \
       inptdt.f \
       inptnm.f \
       input2.f \
       lenst.f \
       maxgro.f \
       maxprd.f \
       natmor.f \
       nutfea.f \
       option.f \
       pools.f \
       prinma.f \
       prinsu.f \
       print6.f \
       prinun.f \
       promes.f \
       prompt.f \
       qslp.f \
       setabc.f \
       setuni.f \
       solvlp.f \
       spcsd.f \
       spindi.f 
#-------------------------------------------------------------------------------
# Libraries
