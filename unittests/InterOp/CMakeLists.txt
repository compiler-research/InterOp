set(LLVM_LINK_COMPONENTS
  Support
  )

add_interop_unittest(InterOpTests
  Utils.cpp
  ScopeReflectionTest.cpp
  FunctionReflectionTest.cpp
  VariableReflectionTest.cpp
  TypeReflectionTest.cpp
  EnumReflectionTest.cpp
  )

set(clingInterpreter "clangInterpreter")
if (USE_CLING)
  set(clingInterpreter "clingInterpreter")
endif()


target_link_libraries(InterOpTests
  PRIVATE
  ${clingInterpreter}
  clangInterOp
  clangAST
  clangBasic
  clangLex
  )
