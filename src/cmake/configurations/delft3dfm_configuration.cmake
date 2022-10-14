# project(delft3dfm)

# Specify the modules to be included

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/dflowfm_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/dwaq_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/dwaves_configuration.cmake)

# Not officially supported yet: include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/drr_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/dimr_configuration.cmake)

# Additional includes

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/tools_delft3dfm_configuration.cmake)

project(delft3dfm)
