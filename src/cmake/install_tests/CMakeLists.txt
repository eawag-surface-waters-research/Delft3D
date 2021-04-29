set(ESMF_SO_FOLDER ${checkout_src_root}/third_party_open/esmf/lnx64/bin)
set(ESMF_SO_FILE ${ESMF_SO_FOLDER}/libesmf.so)
install(PROGRAMS ${ESMF_SO_FILE} DESTINATION share)

install(SCRIPT "${checkout_src_root}/cmake/install_tests/install_and_bundle.cmake")
if(NOT DEFINED COMMIT_VERSION)
  set(COMMIT_VERSION "0")
endif()
configure_package_installer("Tests" "${checkout_src_root}/cmake/install_tests/ReadMe.txt" "1" "0" "${COMMIT_VERSION}" "STGZ;TGZ")

