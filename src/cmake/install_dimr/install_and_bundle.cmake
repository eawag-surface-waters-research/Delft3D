set(BUILD_LIBRARIES
   ${CMAKE_INSTALL_PREFIX}/lib/libdimr_lib.so
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

set(BU_CHMOD_BUNDLE_ITEMS 1)

fixup_bundle("${CMAKE_INSTALL_PREFIX}/bin/dimr" "${BUILD_LIBRARIES}" "${THIRDPARTY_x64_LIB_FOLDERS}")

execute_process(COMMAND find "${CMAKE_INSTALL_PREFIX}/bin" -type f -exec echo "patched rpath of: " {} \; -exec bash -c "patchelf --set-rpath '$ORIGIN:$ORIGIN/../lib' $1" _ {} \;)
execute_process(COMMAND find "${CMAKE_INSTALL_PREFIX}/lib" -type f -exec echo "patched rpath of: " {} \; -exec bash -c "patchelf --set-rpath '$ORIGIN' $1" _ {} \;)
execute_process(COMMAND find "${CMAKE_INSTALL_PREFIX}/lib" -type l -exec echo "remove destination of symlink:" {} \; -exec bash -c "cp --remove-destination $(readlink {}) {};"  {} \; WORKING_DIRECTORY "${CMAKE_INSTALL_PREFIX}/lib" )

# Ugly way to copy file "libdimr_lib.so" to "libdimr.so" in the same directory. They are currently both needed in the testbench
file(COPY ${CMAKE_INSTALL_PREFIX}/lib/libdimr_lib.so DESTINATION ${CMAKE_INSTALL_PREFIX})
file(RENAME ${CMAKE_INSTALL_PREFIX}/libdimr_lib.so ${CMAKE_INSTALL_PREFIX}/lib/libdimr.so)
