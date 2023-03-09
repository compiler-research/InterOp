//--------------------------------------------------------------------*- C++ -*-
// InterOp Compatibility
// author:  Alexander Penev <alexander_penev@yahoo.com>
//------------------------------------------------------------------------------

#ifndef INTEROP_COMPATIBILITY
#define INTEROP_COMPATIBILITY

//namespace cing_compat {

//using namespace clang;
//using namespace llvm;


#ifdef USE_CLING


#endif


#ifdef USE_REPL


//  #define cling clang

#include "CompilationOptions.h"

#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Error.h"

#include "clang/AST/Decl.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/GlobalDecl.h"
#include "clang/AST/Mangle.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendOptions.h"
//**//#include "clang/Interpreter/IncrementalExecutor.h"
//**//#include "clang/Interpreter/IncrementalParser.h"
#include "clang/Interpreter/Interpreter.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"

namespace cling = clang;

namespace clang {

//  namespace utils = clang;

  class ASTContext;
  class Expr;
  class Decl;
  class DeclContext;
  class DeclarationName;
  class FunctionDecl;
  class GlobalDecl;
  class IntegerLiteral;
  class LookupResult;
  class NamedDecl;
  class NamespaceDecl;
  class NestedNameSpecifier;
  class QualType;
  class Sema;
  class TagDecl;
  class TemplateDecl;
  class Type;
  class TypedefNameDecl;

//**//
  class IncrementalParser;
}

namespace {
  template<typename D>
  static D* LookupResult2Decl(clang::LookupResult& R)
  {
    if (R.empty())
      return nullptr;

    R.resolveKind();

    if (R.isSingleResult())
      return llvm::dyn_cast<D>(R.getFoundDecl());
    return (D*)-1;
  }
}

namespace clang {
  namespace utils {
    namespace Lookup {

using namespace clang;
using namespace llvm;

  NamespaceDecl* Namespace(Sema* S, const char* Name,
                                   const DeclContext* Within) {
    DeclarationName DName = &(S->Context.Idents.get(Name));
    LookupResult R(*S, DName, SourceLocation(),
                   Sema::LookupNestedNameSpecifierName);
    R.suppressDiagnostics();
    if (!Within)
      S->LookupName(R, S->TUScope);
    else {
      if (const TagDecl* TD = dyn_cast<TagDecl>(Within)) {
        if (!TD->getDefinition()) {
          // No definition, no lookup result.
          return nullptr;
        }
      }
      S->LookupQualifiedName(R, const_cast<DeclContext*>(Within));
    }

    if (R.empty())
      return nullptr;

    R.resolveKind();

    return dyn_cast<NamespaceDecl>(R.getFoundDecl());
  }

  void Named(Sema* S, LookupResult& R,\
                          const DeclContext* Within = nullptr) {
    R.suppressDiagnostics();
    if (!Within)
      S->LookupName(R, S->TUScope);
    else {
      const DeclContext* primaryWithin = nullptr;
      if (const TagDecl *TD = dyn_cast<TagDecl>(Within)) {
        primaryWithin = dyn_cast_or_null<DeclContext>(TD->getDefinition());
      } else {
        primaryWithin = Within->getPrimaryContext();
      }
      if (!primaryWithin) {
        // No definition, no lookup result.
        return;
      }
      S->LookupQualifiedName(R, const_cast<DeclContext*>(primaryWithin));
    }
  }

  NamedDecl* Named(Sema* S, const DeclarationName& Name,
                           const DeclContext* Within = nullptr) {
    LookupResult R(*S, Name, SourceLocation(), Sema::LookupOrdinaryName,
               Sema::ForVisibleRedeclaration);
    Named(S, R, Within);
    return LookupResult2Decl<NamedDecl>(R);
  }

  NamedDecl* Named(Sema* S, StringRef Name,
                           const DeclContext* Within = nullptr) {
    DeclarationName DName = &S->Context.Idents.get(Name);
    return Named(S, DName, Within);
  }

  NamedDecl* Named(Sema* S, const char* Name,
                           const DeclContext* Within = nullptr) {
    return Named(S, StringRef(Name), Within);
  }

    }
  }
}

namespace InterOp {

using namespace clang;
using namespace llvm;
using namespace std;

  void maybeMangleDeclName(const GlobalDecl& GD, string& mangledName) {
    // copied and adapted from CodeGen::CodeGenModule::getMangledName

    NamedDecl* D = cast<NamedDecl>(const_cast<Decl*>(GD.getDecl()));
    unique_ptr<MangleContext> mangleCtx;
    mangleCtx.reset(D->getASTContext().createMangleContext());
    if (!mangleCtx->shouldMangleDeclName(D)) {
      IdentifierInfo *II = D->getIdentifier();
      assert(II && "Attempt to mangle unnamed decl.");
      mangledName = II->getName().str();
      return;
    }

    raw_string_ostream RawStr(mangledName);

#if defined(_WIN32)
    // MicrosoftMangle.cpp:954 calls llvm_unreachable when mangling Dtor_Comdat
    if (isa<CXXDestructorDecl>(GD.getDecl()) &&
        GD.getDtorType() == Dtor_Comdat) {
      if (const IdentifierInfo* II = D->getIdentifier())
        RawStr << II->getName();
    } else
#endif
      mangleCtx->mangleName(GD, RawStr);
    RawStr.flush();
  }

class IncrementalParserFaildeError : public ErrorInfo<IncrementalParserFaildeError> { };

class Interpreter: public clang::Interpreter {

  bool isInSyntaxOnlyMode() const {
    return getCompilerInstance()->getFrontendOpts().ProgramAction
      == frontend::ParseSyntaxOnly;
  }
/*
  void* getAddressOfGlobal(StringRef SymName,
                                        bool* fromJIT /*=0* /) const {
    // Return a symbol's address, and whether it was jitted.
    if (isInSyntaxOnlyMode())
      return nullptr;
    //* //return IncrExecutor->getAddressOfGlobal(SymName, fromJIT);
    //*?//return IncrExecutor->getSymbolAddress(SymName, fromJIT ? IncrementalExecutor::SymbolNameKind::IRNamee : IncrementalExecutor::SymbolNameKind::LinkerName);
    return nullptr;
  }

  void* getAddressOfGlobal(const GlobalDecl& GD,
                            bool* fromJIT /*=0* /) const {
    // Return a symbol's address, and whether it was jitted.
    string mangledName;
    maybeMangleDeclName(GD, mangledName);
#if defined(_WIN32)
    // For some unknown reason, Clang 5.0 adds a special symbol ('\01') in front
    // of the mangled names on Windows, making them impossible to find
    // TODO: remove this piece of code and try again when updating Clang
    string mncp = mangledName;
    // use corrected symbol for "external" lookup
    if (mncp.size() > 2 && mncp[1] == '?' &&
        mncp.compare(1, 14, string("?__cling_Un1Qu"))) {
      mncp.erase(0, 1);
    }
    void *addr = getAddressOfGlobal(mncp, fromJIT);
    if (addr)
      return addr;
    // if failed, proceed with original symbol for lookups in JIT tables
#endif
    return getAddressOfGlobal(mangledName, fromJIT);
  }
*/

  CompilationOptions makeDefaultCompilationOpts() {
    InterOp::CompilationOptions CO;
    CO.DeclarationExtraction = 0;
    CO.EnableShadowing = 0;
    CO.ValuePrinting = CompilationOptions::VPDisabled;
    CO.CodeGeneration = IncrParser->hasCodeGenerator();
    CO.DynamicScoping = isDynamicLookupEnabled();
    CO.Debug = isPrintingDebug();
    CO.IgnorePromptDiags = !isRawInputEnabled();
    CO.CheckPointerValidity = !isRawInputEnabled();
    CO.OptLevel = getDefaultOptLevel();
    return CO;
  }

  llvm::Error
  DeclareInternal(const string& input,
                  const CompilationOptions& CO //**//,
//**//                  Transaction** T /* = 0 */) const {
                 ) const {
    assert(CO.DeclarationExtraction == 0
           && CO.ValuePrinting == 0
           && CO.ResultEvaluation == 0
           && "Compilation Options not compatible with \"declare\" mode.");

//*?*//    StateDebuggerRAII stateDebugger(this);

    IncrementalParser::ParseResultTransaction PRT
      = IncrParser->Compile(input, CO);
    if (PRT.getInt() == IncrementalParser::kFailed)
      return IncrementalParserFaildeError(); //**// Interpreter::kFailure;

//**//    if (T)
//**//      *T = PRT.getPointer();
    return llvm::ErrorSuccess(); //**//Interpreter::kSuccess;
  }

//**//  Interpreter::CompilationResult
  llvm::Error
  declare(const string& input) { //**//, Transaction** T/*=0 */) {
//**//    if (!isInSyntaxOnlyMode() && m_Opts.CompilerOpts.CUDAHost)
//**//      m_CUDACompiler->declare(input);

    InterOp::CompilationOptions CO = makeDefaultCompilationOpts();
    CO.DeclarationExtraction = 0;
    CO.ValuePrinting = 0;
    CO.ResultEvaluation = 0;
    CO.CheckPointerValidity = 0;

    return DeclareInternal(input, CO); //**//, T);
  }


  ///\brief Compile the function definition and return its Decl.
  ///
  ///\param[in] name - name of the function, used to find its Decl.
  ///\param[in] code - function definition, starting with 'extern "C"'.
  ///\param[in] withAccessControl - whether to enforce access restrictions.
  ///\param[out] T - The cling::Transaction of the input
  const FunctionDecl* DeclareCFunction(StringRef name,
                                       StringRef code,
                                       bool withAccessControl) const {//**//,
//**//                                       Transaction*& T) const {
    /*
    In CallFunc we currently always (intentionally and somewhat necessarily)
    always fully specify member function template, however this can lead to
    an ambiguity with a class template.  For example in
    roottest/cling/functionTemplate we get:

    input_line_171:3:15: warning: lookup of 'set' in member access expression
    is ambiguous; using member of 't'
    ((t*)obj)->set<int>(*(int*)args[0]);
               ^
    roottest/cling/functionTemplate/t.h:19:9: note: lookup in the object type
    't' refers here
    void set(T targ) {
         ^
    /usr/include/c++/4.4.5/bits/stl_set.h:87:11: note: lookup from the
    current scope refers here
    class set
          ^
    This is an intention warning implemented in clang, see
    http://llvm.org/viewvc/llvm-project?view=revision&revision=105518

    which 'should have been' an error:

    C++ [basic.lookup.classref] requires this to be an error, but,
    because it's hard to work around, Clang downgrades it to a warning as
    an extension.</p>

    // C++98 [basic.lookup.classref]p1:
    // In a class member access expression (5.2.5), if the . or -> token is
    // immediately followed by an identifier followed by a <, the identifier
    // must be looked up to determine whether the < is the beginning of a
    // template argument list (14.2) or a less-than operator. The identifier
    // is first looked up in the class of the object expression. If the
    // identifier is not found, it is then looked up in the context of the
    // entire postfix-expression and shall name a class or function template. If
    // the lookup in the class of the object expression finds a template, the
    // name is also looked up in the context of the entire postfix-expression
    // and
    // -- if the name is not found, the name found in the class of the
    // object expression is used, otherwise
    // -- if the name is found in the context of the entire postfix-expression
    // and does not name a class template, the name found in the class of the
    // object expression is used, otherwise
    // -- if the name found is a class template, it must refer to the same
    // entity as the one found in the class of the object expression,
    // otherwise the program is ill-formed.

    See -Wambiguous-member-template

    An alternative to disabling the diagnostics is to use a pointer to
    member function:

    #include <set>
    using namespace std;

    extern "C" int printf(const char*,...);

    struct S {
    template <typename T>
    void set(T) {};

    virtual void virtua() { printf("S\n"); }
    };

    struct T: public S {
    void virtua() { printf("T\n"); }
    };

    int main() {
    S *s = new T();
    typedef void (S::*Func_p)(int);
    Func_p p = &S::set<int>;
    (s->*p)(12);

    typedef void (S::*Vunc_p)(void);
    Vunc_p q = &S::virtua;
    (s->*q)(); // prints "T"
    return 0;
    }
    */

//**//    DiagnosticsEngine& Diag = getDiagnostics();
//**//    Diag.setSeverity(clang::diag::ext_nested_name_member_ref_lookup_ambiguous,
//**//                     clang::diag::Severity::Ignored, SourceLocation());

    LangOptions& LO = const_cast<LangOptions&>(getCompilerInstance()->getLangOpts());
    bool savedAccessControl = LO.AccessControl;
    LO.AccessControl = withAccessControl;
//**//    T = nullptr;
//**//    cling::Interpreter::CompilationResult CR = declare(code.str(), &T);
    llvm::Error CR = declare(code.str());
    LO.AccessControl = savedAccessControl;

//**//    Diag.setSeverity(clang::diag::ext_nested_name_member_ref_lookup_ambiguous,
//**//                     clang::diag::Severity::Warning, SourceLocation());

//**//    if (CR != cling::Interpreter::kSuccess)
      if (CR) {
        CR.setChecked(true);
        return nullptr;
      }
//**//
    for (cling::Transaction::const_iterator I = T->decls_begin(),
           E = T->decls_end(); I != E; ++I) {
      if (I->m_Call != cling::Transaction::kCCIHandleTopLevelDecl)
        continue;
      if (const LinkageSpecDecl* LSD
          = dyn_cast<LinkageSpecDecl>(*I->m_DGR.begin())) {
        DeclContext::decl_iterator DeclBegin = LSD->decls_begin();
        if (DeclBegin == LSD->decls_end())
          continue;
        if (const FunctionDecl* D = dyn_cast<FunctionDecl>(*DeclBegin)) {
          const IdentifierInfo* II = D->getDeclName().getAsIdentifierInfo();
          if (II && II->getName() == name)
            return D;
        }
      }
    }
    return nullptr;
  }

  void* compileFunction(StringRef name, StringRef code,
                       bool ifUnique, bool withAccessControl) {
    //
    //  Compile the wrapper code.
    //

    if (isInSyntaxOnlyMode())
      return nullptr;

    if (ifUnique) {
//*?*//      if (void* Addr = (void*)getAddressOfGlobal(name)) {
      if (void* Addr = (void*)(getSymbolAddress(name).get())) {
        return Addr;
      }
    }

//**//    Transaction* T = nullptr;
    const FunctionDecl* FD = DeclareCFunction(name, code, withAccessControl); //**//, T);
//**//    if (!FD || !T)
      if (!FD)
      return nullptr;

    //  Get the wrapper function pointer from the ExecutionEngine (the JIT).
    return m_Executor->getPointerToGlobalFromJIT(name);
  }


  };
}

#endif

//} // namespace cling_compat

#endif //INTEROP_COMPATIBILITY
