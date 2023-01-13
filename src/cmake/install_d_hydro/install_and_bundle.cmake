set(BUILD_LIBRARIES
   ${CMAKE_INSTALL_PREFIX}/lib/libd_hydro_lib.so
)
set(THIRDPARTY_x64_LIB_FOLDERS
  ${CMAKE_INSTALL_PREFIX}
  ${CMAKE_INSTALL_PREFIX}/bin
  ${CMAKE_INSTALL_PREFIX}/lib
)
function(gp_resolved_file_type_override resolved_file type_var)
  set(${type_var} local PARENT_SCOPE)
endfunction()

function(gp_item_default_embedded_path_override item default_embedded_path_var)
  if(item MATCHES ".so")
    set(path "@executable_path/../lib" PARENT_SCOPE)
    set( overridden 1 PARENT_SCOPE )
  endif()
endfunction(gp_item_default_embedded_path_override)

include(BundleUtilities)
include(${CMAKE_CURRENT_SOURCE_DIR}/../src/cmake/functions.cmake)

set(BU_CHMOD_BUNDLE_ITEMS 1)

fixup_bundle("${CMAKE_INSTALL_PREFIX}/bin/d_hydro" "${BUILD_LIBRARIES}" "${THIRDPARTY_x64_LIB_FOLDERS}")

# Ugly way to rename file "libd_hydro_lib.so" to "libd_hydro.so", repairing the reference in d_hydro
message("Renaming libd_hydro_lib.so to libd_hydro.so, repairing reference in d_hydro")
file(RENAME ${CMAKE_INSTALL_PREFIX}/lib/libd_hydro_lib.so ${CMAKE_INSTALL_PREFIX}/lib/libd_hydro.so)
execute_process(COMMAND bash -c "patchelf --replace-needed libd_hydro_lib.so libd_hydro.so ${CMAKE_INSTALL_PREFIX}/bin/d_hydro")


set_rpath("${CMAKE_INSTALL_PREFIX}/bin" "$ORIGIN:$ORIGIN/../lib")
set_rpath("${CMAKE_INSTALL_PREFIX}/lib" "$ORIGIN")
execute_process(COMMAND find "${CMAKE_INSTALL_PREFIX}/lib" -type l -exec echo "remove destination of symlink:" {} \; -exec bash -c "cp --remove-destination $(readlink {}) {};"  {} \; WORKING_DIRECTORY "${CMAKE_INSTALL_PREFIX}/lib" )

