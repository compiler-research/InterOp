#ifndef CLING_UNITTESTS_LIBINTEROP_UTILS_H
#define CLING_UNITTESTS_LIBINTEROP_UTILS_H

#include <memory>
#include <vector>
namespace clang {
  class Decl;
}
namespace cling {
  class Interpreter;
}
namespace TestUtils {
//**//  extern std::unique_ptr<cling::Interpreter> Interp;
  extern std::unique_ptr<InterOp::Interpreter> Interp;
  void GetAllTopLevelDecls(const std::string& code, std::vector<clang::Decl*>& Decls);
  void GetAllSubDecls(clang::Decl *D, std::vector<clang::Decl*>& SubDecls);
} // end namespace TestUtils

#endif // CLING_UNITTESTS_LIBINTEROP_UTILS_H

