#----------------------------------------------------------------
# Generated CMake target import file for configuration "Release".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "ML32i4" for configuration "Release"
set_property(TARGET ML32i4 APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(ML32i4 PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELEASE "CXX"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/CompilerAdditions/libML32i4.a"
  )

list(APPEND _IMPORT_CHECK_TARGETS ML32i4 )
list(APPEND _IMPORT_CHECK_FILES_FOR_ML32i4 "${_IMPORT_PREFIX}/CompilerAdditions/libML32i4.a" )

# Import target "ML32i3" for configuration "Release"
set_property(TARGET ML32i3 APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(ML32i3 PROPERTIES
  IMPORTED_LINK_INTERFACE_LANGUAGES_RELEASE "CXX"
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/CompilerAdditions/libML32i3.a"
  )

list(APPEND _IMPORT_CHECK_TARGETS ML32i3 )
list(APPEND _IMPORT_CHECK_FILES_FOR_ML32i3 "${_IMPORT_PREFIX}/CompilerAdditions/libML32i3.a" )

# Import target "ML32i4dyn" for configuration "Release"
set_property(TARGET ML32i4dyn APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(ML32i4dyn PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/CompilerAdditions/libML32i4.so"
  IMPORTED_SONAME_RELEASE "libML32i4.so"
  )

list(APPEND _IMPORT_CHECK_TARGETS ML32i4dyn )
list(APPEND _IMPORT_CHECK_FILES_FOR_ML32i4dyn "${_IMPORT_PREFIX}/CompilerAdditions/libML32i4.so" )

# Import target "ML32i3dyn" for configuration "Release"
set_property(TARGET ML32i3dyn APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(ML32i3dyn PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/CompilerAdditions/libML32i3.so"
  IMPORTED_SONAME_RELEASE "libML32i3.so"
  )

list(APPEND _IMPORT_CHECK_TARGETS ML32i3dyn )
list(APPEND _IMPORT_CHECK_FILES_FOR_ML32i3dyn "${_IMPORT_PREFIX}/CompilerAdditions/libML32i3.so" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
