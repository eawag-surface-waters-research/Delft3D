set(BUILD_LIBRARIES
   ${CMAKE_INSTALL_PREFIX}/lib/libdflowfm.so
   ${CMAKE_INSTALL_PREFIX}/lib/libio_netcdf_dll.so
   ${CMAKE_INSTALL_PREFIX}/share/libesmf.so
)
set(THIRDPARTY_x64_LIB_FOLDERS
  ${CMAKE_INSTALL_PREFIX}
  ${CMAKE_INSTALL_PREFIX}/bin
  ${CMAKE_INSTALL_PREFIX}/lib
  ${CMAKE_INSTALL_PREFIX}/share
  ${ESMF_SO_FOLDER}
)
function(gp_resolved_file_type_override resolved_file type_var)
  set(${type_var} local PARENT_SCOPE)
endfunction()

function(gp_item_default_embedded_path_override item default_embedded_path_var)
  if(item MATCHES "libesmf.so")
    set(path "@executable_path/../share" PARENT_SCOPE)
    set( overridden 1 PARENT_SCOPE )
  elseif(item MATCHES ".so")
    set(path "@executable_path/../lib" PARENT_SCOPE)
    set( overridden 1 PARENT_SCOPE )
  endif()
endfunction(gp_item_default_embedded_path_override)

include(BundleUtilities)

set(BU_CHMOD_BUNDLE_ITEMS 1)

fixup_bundle("${CMAKE_INSTALL_PREFIX}/bin/dflowfm-cli" "${BUILD_LIBRARIES}" "${THIRDPARTY_x64_LIB_FOLDERS}")
fixup_bundle("${CMAKE_INSTALL_PREFIX}/bin/dfmoutput" "${BUILD_LIBRARIES}" "${THIRDPARTY_x64_LIB_FOLDERS}")

execute_process(COMMAND find "${CMAKE_INSTALL_PREFIX}/bin" -type f -exec bash -c "patchelf --set-rpath '$ORIGIN/../lib' $1" _ {} \; -exec echo "patched rpath of: " {} \;)
execute_process(COMMAND find "${CMAKE_INSTALL_PREFIX}/lib" -type f -exec echo "patched rpath of: "  {} \; -exec bash -c "patchelf --set-rpath '$ORIGIN' $1" _ {} \;)
execute_process(COMMAND find "${CMAKE_INSTALL_PREFIX}/share" -type f -exec echo "patched rpath of: " {} \; -exec bash -c "patchelf --set-rpath '$ORIGIN/../lib:$ORIGIN' $1" _ {} \;)
execute_process(COMMAND find "${CMAKE_INSTALL_PREFIX}/lib" -type l -exec echo "remove destination of symlink:" {} \; -exec bash -c "cp --remove-destination $(readlink {}) {};"  {} \; WORKING_DIRECTORY "${CMAKE_INSTALL_PREFIX}/lib" )

file(RENAME ${CMAKE_INSTALL_PREFIX}/bin/dflowfm-cli ${CMAKE_INSTALL_PREFIX}/bin/dflowfm)
# The following files are left-overs from the traditional build. "libdimr.so_from_traditional_build" is already replaced by "libdimr.so_from_CMake_build".
file(REMOVE ${CMAKE_INSTALL_PREFIX}/lib/libdimr.so.0 ${CMAKE_INSTALL_PREFIX}/lib/libdimr.so.0.0.0)

