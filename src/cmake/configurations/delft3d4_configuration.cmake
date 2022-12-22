# project(delft3d4)

# Specify the modules to be included

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/dwaq_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/dwaves_configuration.cmake)

# include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/rtc_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/flow2d3d_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/d_hydro_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/dimr_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/tools_gpl_configuration.cmake)

project(delft3d4)
