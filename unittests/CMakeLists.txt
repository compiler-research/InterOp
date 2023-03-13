set(CTEST_BUILD_NAME
  ${CMAKE_SYSTEM_NAME}-${CMAKE_HOST_SYSTEM_PROCESSOR}-${CMAKE_BUILD_TYPE})
enable_testing()

# LLVM builds (not installed llvm) provides gtest.
if (NOT TARGET gtest)
  include(GoogleTest)
endif()

add_custom_target(InterOpUnitTests)
set_target_properties(InterOpUnitTests PROPERTIES FOLDER "InterOp tests")
add_dependencies(InterOpUnitTests clangInterOp)

set (TIMEOUT_VALUE 2400)
function(add_interop_unittest name)
  add_executable(${name} ${ARGN})
  add_dependencies(InterOpUnitTests ${name})
  target_include_directories(${name} PUBLIC ${CMAKE_CURRENT_BINARY_DIR} ${GTEST_INCLUDE_DIR})
  set_property(TARGET ${name} PROPERTY RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

  target_link_libraries(${name} PUBLIC ${ARG_LIBRARIES} gtest gtest_main)

  add_test(NAME interop-${name} COMMAND ${name})
  set_tests_properties(interop-${name} PROPERTIES
                       TIMEOUT "${TIMEOUT_VALUE}"
                       LABELS
                       DEPENDS)
  # FIXME: Just call gtest_add_tests this function is available.
  #gtest_add_tests(${name} "${Arguments}" AUTO)
endfunction()
 
add_subdirectory(InterOp)

add_custom_target(check-interop COMMAND ${CMAKE_CTEST_COMMAND} -V
  DEPENDS InterOpUnitTests WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

set_target_properties(check-interop PROPERTIES FOLDER "InterOp tests")
