add_subdirectory(bgcfarm_util)
add_subdirectory(CENT_ECACNP)
add_subdirectory(cdom_ecacnp)
if (UGM)
  add_subdirectory(UGM-M)
endif()
#add_subdirectory(CENT_ECADCNP)
# FIXME(bja, 201603) is this manual list necessary with the subdir
# adding to parent scope?

#set(BETR_LIBRARIES bgcfarm_util;cent_ecadcnp;${BETR_LIBRARIES} PARENT_SCOPE)
if (UGM)
set(BETR_LIBRARIES bgcfarm_util;UGM-M;${BETR_LIBRARIES} PARENT_SCOPE)
else()
set(BETR_LIBRARIES bgcfarm_util;${BETR_LIBRARIES} PARENT_SCOPE)
endif()
