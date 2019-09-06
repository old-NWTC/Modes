
add_executable(modes Eigen.for GenUse.f90 ModCVF.f90 Modes.f90 Modules.f90 SysCVF.f90)

install(TARGETS modes
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib )
