set(CDOMNLAYER_SOURCES
  BgcReactionscdomECACnpType.F90
  PlantSoilBgccdomCnpType.F90
)

  include_directories(${CMAKE_BINARY_DIR}/src/betr/betr_util)
  include_directories(${CMAKE_BINARY_DIR}/src/betr/betr_math)
  include_directories(${CMAKE_BINARY_DIR}/src/betr/betr_bgc)
  include_directories(${CMAKE_BINARY_DIR}/src/betr/betr_grid)
  include_directories(${CMAKE_BINARY_DIR}/src/betr/betr_dtype)
  include_directories(${CMAKE_BINARY_DIR}/src/betr/betr_core)
  include_directories(${CMAKE_BINARY_DIR}/src/Applications/soil-farm/bgcfarm_util)
  include_directories(${CMAKE_BINARY_DIR}/src/Applications/soil-farm/cdom_ecacnp/cdomPara)
  include_directories(${CMAKE_BINARY_DIR}/src/Applications/soil-farm/cdom_ecacnp/cdom1layer)
  include(add_betr_library)
  add_betr_library(cdomNlayer ${CDOMNLAYER_SOURCES})

  set(BETR_LIBRARIES cdomNlayer;${BETR_LIBRARIES} PARENT_SCOPE)
  set(BETR_LIBRARIES cdomNlayer;${BETR_LIBRARIES})

#X#add_subdirectory(tests)

  if (NOT CMAKE_INSTALL_PREFIX STREQUAL "INSTALL_DISABLED")
    install(TARGETS cdomNlayer DESTINATION lib)
    file(GLOB HEADERS *.h)
    install(FILES ${HEADERS} DESTINATION include/soil-farm/cdom_ecacnp/cdomNlayer)
  endif()
