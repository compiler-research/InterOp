//--------------------------------------------------------------------*- C++ -*-
// InterOp Compatibility
// author:  Alexander Penev <alexander_penev@yahoo.com>
//------------------------------------------------------------------------------

#ifndef INTEROP_COMPATIBILITY
#define INTEROP_COMPATIBILITY


#ifdef USE_CLING


#endif //USE_CLING


#ifdef USE_REPL

#include "CompilationOptions.h"
#include "DynamicLibraryManager.h"
#include "Paths.h"

#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Error.h"

#include "clang/AST/Decl.h"
#include "clang/AST/DeclarationName.h"
#include "clang/AST/GlobalDecl.h"
#include "clang/AST/Mangle.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendOptions.h"
#include "clang/Interpreter/Interpreter.h"
#include "clang/Interpreter/PartialTranslationUnit.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Sema/Lookup.h"
#include "clang/Sema/Sema.h"

namespace clang {

/*  class ASTContext;
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
*/

//**//
  class CompilerInstance;
//  class IncrementalExecutor;
//  class IncrementalParser;
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

namespace InterOp {
  namespace utils {
    namespace Lookup {

  using namespace clang;
  using namespace llvm;

  inline NamespaceDecl* Namespace(Sema* S, const char* Name,
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

  inline void Named(Sema* S, LookupResult& R,
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

  inline NamedDecl* Named(Sema* S, const DeclarationName& Name,
                           const DeclContext* Within = nullptr) {
    LookupResult R(*S, Name, SourceLocation(), Sema::LookupOrdinaryName,
               Sema::ForVisibleRedeclaration);
    Named(S, R, Within);
    return LookupResult2Decl<NamedDecl>(R);
  }

  inline NamedDecl* Named(Sema* S, StringRef Name,
                           const DeclContext* Within = nullptr) {
    DeclarationName DName = &S->Context.Idents.get(Name);
    return Named(S, DName, Within);
  }

  inline NamedDecl* Named(Sema* S, const char* Name,
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

  inline void maybeMangleDeclName(const GlobalDecl& GD, string& mangledName) {
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

// clang::Interpreter:
//  llvm::Expected<PartialTranslationUnit &> Parse(llvm::StringRef Code);
//  llvm::Error Execute(PartialTranslationUnit &T);
//  llvm::Error ParseAndExecute(llvm::StringRef Code) {
//  llvm::Error Undo(unsigned N = 1);
//  llvm::Expected<llvm::JITTargetAddress> getSymbolAddress(GlobalDecl GD) const;
//  llvm::Expected<llvm::JITTargetAddress> getSymbolAddress(llvm::StringRef IRName) const;
//  llvm::Expected<llvm::JITTargetAddress> getSymbolAddressFromLinkerName(llvm::StringRef LinkerName) const;
class /*InterOp::*/Interpreter {
  std::unique_ptr<clang::Interpreter> inner;
public:
  Interpreter(int argc, const char* const *argv,
              const char* llvmdir = 0,
              const std::vector<std::shared_ptr<clang::ModuleFileExtension>>& moduleExtensions = {},
              void *extraLibHandle = 0, bool noRuntime = true) {
    std::vector<const char *> vargs(argv + 1, argv + argc);
    if (auto ciOrErr = clang::IncrementalCompilerBuilder::create(vargs)) {
      if (auto innerOrErr = clang::Interpreter::create(std::move(*ciOrErr))) {
        inner = std::move(*innerOrErr);
      } else {
        reportErr("Failed to build Interpreter: ", innerOrErr.takeError());
      }
    } else {
      reportErr("Failed to build Incremental compiler: ", ciOrErr.takeError());
    }
  }

  ~Interpreter() {}

  ///\brief Describes the return result of the different routines that do the
  /// incremental compilation.
  ///
  enum CompilationResult {
    kSuccess,
    kFailure,
    kMoreInputExpected
  };

  static void reportErr(StringRef message, llvm::Error err) {
    llvm::SmallVector<char, 64> Buffer;
    raw_svector_ostream OS(Buffer);
    OS << err;
    StringRef Str = OS.str();
    llvm::report_fatal_error(llvm::Twine(message) + Str);
  }

  const CompilerInstance *getCompilerInstance() const {
    return inner->getCompilerInstance();
  }

  const llvm::orc::LLJIT *getExecutionEngine() const {
    return inner->getExecutionEngine();
  }

  llvm::Expected<PartialTranslationUnit &> Parse(llvm::StringRef Code) {
    return inner->Parse(Code);
  }

  llvm::Error Execute(PartialTranslationUnit &T) {
    return inner->Execute(T);
  }

  llvm::Error ParseAndExecute(llvm::StringRef Code) {
    return inner->ParseAndExecute(Code);
  }

  llvm::Error Undo(unsigned N = 1) {
    return inner->Undo(N);
  }

  /// \returns the \c JITTargetAddress of a \c GlobalDecl. This interface uses
  /// the CodeGenModule's internal mangling cache to avoid recomputing the
  /// mangled name.
  llvm::Expected<llvm::JITTargetAddress> getSymbolAddress(GlobalDecl GD) const {
    return inner->getSymbolAddress(GD);
  }

  /// \returns the \c JITTargetAddress of a given name as written in the IR.
  llvm::Expected<llvm::JITTargetAddress>
  getSymbolAddress(llvm::StringRef IRName) const {
    return inner->getSymbolAddress(IRName);
  }

  /// \returns the \c JITTargetAddress of a given name as written in the object
  /// file.
  llvm::Expected<llvm::JITTargetAddress>
  getSymbolAddressFromLinkerName(llvm::StringRef LinkerName) const {
    return inner->getSymbolAddressFromLinkerName(LinkerName);
  }


/*
  IncrementalAction(CompilerInstance &CI, llvm::LLVMContext &LLVMCtx, llvm::Error &Err)
      : WrapperFrontendAction([&]() {
          llvm::ErrorAsOutParameter EAO(&Err);
          std::unique_ptr<FrontendAction> Act;
          switch (CI.getFrontendOpts().ProgramAction) {
          default:
            Err = llvm::createStringError(
                std::errc::state_not_recoverable,
                "Driver initialization failed. "
                "Incremental mode for action %d is not supported",
                CI.getFrontendOpts().ProgramAction);
            return Act;
          case frontend::ASTDump:
            LLVM_FALLTHROUGH;
          }
          return Act;
        }()) {}
*/
//    std::vector<const char *> ClingArgv = {"-resource-dir", ResourceDir.c_str(),
//                                           "-std=c++14"};
//    ClingArgv.insert(ClingArgv.begin(), MainExecutableName.c_str());
//**//    return new cling::Interpreter(ClingArgv.size(), &ClingArgv[0]);
//    return new InterOp::Interpreter(ClingArgv.size(), &ClingArgv[0]);
//      std::vector<std::string> vargs(argv + 1, argv + argc);
//      auto ci = IncrementalCompilerBuilder::create(vargs);
//      llvm::Error Err = llvm::Error::success();
//      clang::Interpreter(ci, &Err);
//      assert(Err && "Can't create wrapped clang interpreter.");


/*
    ///\brief Pushes a new transaction, which will collect the decls that came
    /// within the scope of the RAII object. Calls commit transaction at
    /// destruction.
    class PushTransactionRAII {
    private:
      Transaction* m_Transaction;
      const Interpreter* m_Interpreter;
    public:
      PushTransactionRAII(const Interpreter* i) : m_Interpreter(i) {
        CompilationOptions CO = m_Interpreter->makeDefaultCompilationOpts();
        CO.ResultEvaluation = 0;
        CO.DynamicScoping = 0;
        m_Transaction = m_Interpreter->m_IncrParser->beginTransaction(CO);
      }
      ~PushTransactionRAII() {
        pop();
      }
      void pop() const {
        if (m_Transaction->getState() == Transaction::kRolledBack)
          return;
        IncrementalParser::ParseResultTransaction PRT
          = m_Interpreter->m_IncrParser->endTransaction(m_Transaction);
        if (PRT.getPointer()) {
          assert(PRT.getPointer()==m_Transaction && "Ended different transaction?");
          m_Interpreter->m_IncrParser->commitTransaction(PRT);
        }
      }
    };
*/

  bool isInSyntaxOnlyMode() const {
    return getCompilerInstance()->getFrontendOpts().ProgramAction
      == frontend::ParseSyntaxOnly;
  }

/* clang impl
llvm::Expected<llvm::JITTargetAddress>
Interpreter::getSymbolAddress(GlobalDecl GD) const {
  if (!IncrExecutor)
    return llvm::make_error<llvm::StringError>("Operation failed. "
                                               "No execution engine",
                                               std::error_code());
  llvm::StringRef MangledName = IncrParser->GetMangledName(GD);
  return getSymbolAddress(MangledName);
}
*/

/**/  void* getAddressOfGlobal(const GlobalDecl& GD, bool* fromJIT=0) const {
    auto result = getSymbolAddress(GD);
    if (fromJIT)
      *fromJIT = true; //Fix
    return (void*)(result.get());
/* cling impl
// Return a symbol's address, and whether it was jitted.
    std::string mangledName;
    utils::Analyze::maybeMangleDeclName(GD, mangledName);
#if defined(_WIN32)
    // For some unknown reason, Clang 5.0 adds a special symbol ('\01') in front
    // of the mangled names on Windows, making them impossible to find
    // TODO: remove this piece of code and try again when updating Clang
    std::string mncp = mangledName;
    // use corrected symbol for "external" lookup
    if (mncp.size() > 2 && mncp[1] == '?' &&
        mncp.compare(1, 14, std::string("?__cling_Un1Qu"))) {
      mncp.erase(0, 1);
    }
    void *addr = getAddressOfGlobal(mncp, fromJIT);
    if (addr)
      return addr;
    // if failed, proceed with original symbol for lookups in JIT tables
#endif
    return getAddressOfGlobal(mangledName, fromJIT);
*/
  }

/* clang impl
llvm::Expected<llvm::JITTargetAddress>
Interpreter::getSymbolAddress(llvm::StringRef IRName) const {
  if (!IncrExecutor)
    return llvm::make_error<llvm::StringError>("Operation failed. "
                                               "No execution engine",
                                               std::error_code());

  return IncrExecutor->getSymbolAddress(IRName, IncrementalExecutor::IRName);
}

llvm::Expected<llvm::JITTargetAddress>
Interpreter::getSymbolAddressFromLinkerName(llvm::StringRef Name) const {
  if (!IncrExecutor)
    return llvm::make_error<llvm::StringError>("Operation failed. "
                                               "No execution engine",
                                               std::error_code());

  return IncrExecutor->getSymbolAddress(Name, IncrementalExecutor::LinkerName);
}
*/

/* cling impl
  void* getAddressOfGlobal(llvm::StringRef SymName, bool* fromJIT=0) const {
    // Return a symbol's address, and whether it was jitted.
    if (isInSyntaxOnlyMode())
      return nullptr;
    return m_Executor->getAddressOfGlobal(SymName, fromJIT);
  }
*/

/* cling executor
  void* address = m_JIT->getSymbolAddress(symbolName, includeHostSymbols);

  // FIXME: If we split the loaded libraries into a separate JITDylib we should
  // be able to delete this code and use something like:
  //   if (IncludeHostSymbols) {
  //   if (auto Sym = lookup(<HostSymbolsJD>, Name)) {
  //     fromJIT = false;
  //     return Sym;
  //   }
  // }
  // if (auto Sym = lookup(<REPLJD>, Name)) {
  //   fromJIT = true;
  //   return Sym;
  // }
  // fromJIT = false;
  // return nullptr;
  if (fromJIT) {
    // FIXME: See comments on DLSym below.
    // We use dlsym to just tell if somethings was from the jit or not.
#if !defined(_WIN32)
    void* Addr = llvm::sys::DynamicLibrary::SearchForAddressOfSymbol(symbolName.str());
#else
    void* Addr = const_cast<void*>(platform::DLSym(symbolName.str()));
#endif
    *fromJIT = !Addr;
  }

  return address;
*/

/**/  void* getAddressOfGlobal(StringRef SymName, bool* fromJIT = 0) const {
    // Return a symbol's address, and whether it was jitted.
    if (isInSyntaxOnlyMode())
      return nullptr;
    //**//return IncrExecutor->getAddressOfGlobal(SymName, fromJIT);
    auto result = getSymbolAddress(SymName); //? getSymbolAddressFromLinkerName(SymName);
    if (fromJIT)
      *fromJIT = true; //TODO: Fix this to wor corect
    return (void*)(result.get());
  }

/*  CompilationOptions makeDefaultCompilationOpts() {
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
*/

/*
  llvm::Error
  DeclareInternal(const string& input,
                  const CompilationOptions& CO //** //,
//** //                  Transaction** T /* = 0 * /) const {
                 ) const {
    assert(CO.DeclarationExtraction == 0
           && CO.ValuePrinting == 0
           && CO.ResultEvaluation == 0
           && "Compilation Options not compatible with \"declare\" mode.");

//*?* //    StateDebuggerRAII stateDebugger(this);

    IncrementalParser::ParseResultTransaction PRT
      = IncrParser->Compile(input, CO);
    if (PRT.getInt() == IncrementalParser::kFailed)
      return IncrementalParserFaildeError(); //** // Interpreter::kFailure;

//** //    if (T)
//** //      *T = PRT.getPointer();
    return llvm::ErrorSuccess(); //** //Interpreter::kSuccess;
  }
*/

  CompilationResult
  declare(const string& input, PartialTranslationUnit **PTU = nullptr) {
//**//    , Transaction** T/*=0 */) {
//**//    if (!isInSyntaxOnlyMode() && m_Opts.CompilerOpts.CUDAHost)
//**//      m_CUDACompiler->declare(input);

    if (auto resultPTUorErr = Parse(input)) {
      if (PTU)
        *PTU = &*resultPTUorErr;
      return Interpreter::kSuccess;
    }
    // Hangle Err
    return Interpreter::kFailure;
/*

    InterOp::CompilationOptions CO = makeDefaultCompilationOpts();
    CO.DeclarationExtraction = 0;
    CO.ValuePrinting = 0;
    CO.ResultEvaluation = 0;
    CO.CheckPointerValidity = 0;

    return DeclareInternal(input, CO); //** //, T);
*/
  }

  ///\brief Maybe transform the input line to implement cint command line
  /// semantics (declarations are global) and compile to produce a module.
  ///
  CompilationResult
  process(const std::string& input, Value* V = 0,
          PartialTranslationUnit **PTU = nullptr,
//**//    Transaction** T /* = 0 */,
          bool disableValuePrinting = false) {

    llvm::Error Err = Execute(**PTU);
    if (Err) {
      // Handle Err
      return Interpreter::kFailure;
    }
    return Interpreter::kSuccess;

/*
//** //    if (!isInSyntaxOnlyMode() && m_Opts.CompilerOpts.CUDAHost)
//** //      m_CUDACompiler->process(input);

    std::string wrapReadySource = input;
    size_t wrapPoint = std::string::npos;
    if (!isRawInputEnabled())
      wrapPoint = utils::getWrapPoint(wrapReadySource, getCI()->getLangOpts());

    CompilationOptions CO = makeDefaultCompilationOpts();
    CO.EnableShadowing = m_RuntimeOptions.AllowRedefinition && !isRawInputEnabled();

    if (isRawInputEnabled() || wrapPoint == std::string::npos) {
      CO.DeclarationExtraction = 0;
      CO.ValuePrinting = 0;
      CO.ResultEvaluation = 0;
      return DeclareInternal(input, CO, T);
    }

    CO.DeclarationExtraction = 1;
    CO.ValuePrinting = disableValuePrinting ? CompilationOptions::VPDisabled
      : CompilationOptions::VPAuto;
    CO.ResultEvaluation = (bool)V;
    // CO.IgnorePromptDiags = 1; done by EvaluateInternal().
    CO.CheckPointerValidity = 1;
    if (EvaluateInternal(wrapReadySource, CO, V, T, wrapPoint)
                                                     == Interpreter::kFailure) {
      return Interpreter::kFailure;
    }

    return Interpreter::kSuccess;
*/
  }


  ///\brief Compile the function definition and return its Decl.
  ///
  ///\param[in] name - name of the function, used to find its Decl.
  ///\param[in] code - function definition, starting with 'extern "C"'.
  ///\param[in] withAccessControl - whether to enforce access restrictions.
  ///\param[out] T - The cling::Transaction of the input
//  const FunctionDecl* DeclareCFunction(StringRef name,
//                                       StringRef code,
//                                       bool withAccessControl) const {//**//,
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
//
//    LangOptions& LO = const_cast<LangOptions&>(getCompilerInstance()->getLangOpts());
//    bool savedAccessControl = LO.AccessControl;
//    LO.AccessControl = withAccessControl;
//**//    T = nullptr;
//**//    CompilationResult CR = declare(code.str(), &T);
//    llvm::Error CR = declare(code.str());
//    LO.AccessControl = savedAccessControl;
//
//**//    Diag.setSeverity(clang::diag::ext_nested_name_member_ref_lookup_ambiguous,
//**//                     clang::diag::Severity::Warning, SourceLocation());
//
//**//    if (CR != cling::Interpreter::kSuccess)
//      if (CR) {
//        CR.setChecked(true);
//        return nullptr;
//      }
//**//
//    for (cling::Transaction::const_iterator I = T->decls_begin(),
//           E = T->decls_end(); I != E; ++I) {
//      if (I->m_Call != cling::Transaction::kCCIHandleTopLevelDecl)
//        continue;
//      if (const LinkageSpecDecl* LSD
//          = dyn_cast<LinkageSpecDecl>(*I->m_DGR.begin())) {
//        DeclContext::decl_iterator DeclBegin = LSD->decls_begin();
//        if (DeclBegin == LSD->decls_end())
//          continue;
//        if (const FunctionDecl* D = dyn_cast<FunctionDecl>(*DeclBegin)) {
//          const IdentifierInfo* II = D->getDeclName().getAsIdentifierInfo();
//          if (II && II->getName() == name)
//            return D;
//        }
//      }
//    }
//    return nullptr;
//  }

  void* compileFunction(StringRef name, StringRef code,
                        bool ifUnique, bool withAccessControl) {
    //
    //  Compile the wrapper code.
    //

    if (isInSyntaxOnlyMode())
      return nullptr;

    if (ifUnique) {
      if (void* Addr = (void*)getAddressOfGlobal(name)) {
        return Addr;
      }
    }

//**//    Transaction* T = nullptr;
//**//    const FunctionDecl* FD = DeclareCFunction(name, code, withAccessControl); //**//, T);
//**//    if (!FD || !T)
//**//      return nullptr;
//**//
//**//    //  Get the wrapper function pointer from the ExecutionEngine (the JIT).
//**//    return m_Executor->getPointerToGlobalFromJIT(name);

//  llvm::Expected<PartialTranslationUnit &> Parse(llvm::StringRef Code);
//  llvm::Error Execute(PartialTranslationUnit &T);
//  llvm::Error ParseAndExecute(llvm::StringRef Code) {
    auto PTU = Parse(code); //???
    return nullptr; //PTU???
  }

  const CompilerInstance *getCI() const {
    return getCompilerInstance();
  }

  Sema& getSema() const {
    return getCI()->getSema();
  }

  const DynamicLibraryManager* getDynamicLibraryManager() const {
    assert(inner->getExecutionEngine() && "We must have an executor");
    static const DynamicLibraryManager *DLM = new DynamicLibraryManager();
    return DLM; //TODO: Add DLM to InternalExecutor and use executor->getDML()
    //**//return inner->getExecutionEngine()->getDynamicLibraryManager();
  }

  DynamicLibraryManager* getDynamicLibraryManager() {
    return const_cast<DynamicLibraryManager*>(
      const_cast<const Interpreter*>(this)->getDynamicLibraryManager()
    );
  }

  ///\brief Adds multiple include paths separated by a delimter.
  ///
  ///\param[in] PathsStr - Path(s)
  ///\param[in] Delim - Delimiter to separate paths or NULL if a single path
  ///
  void AddIncludePaths(llvm::StringRef PathsStr, const char* Delim = ":") {
    const CompilerInstance* CI = getCompilerInstance();
    HeaderSearchOptions& HOpts = (HeaderSearchOptions&)CI->getHeaderSearchOpts();

    // Save the current number of entries
    size_t Idx = HOpts.UserEntries.size();
    InterOp::utils::AddIncludePaths(PathsStr, HOpts, Delim);

    Preprocessor& PP = CI->getPreprocessor();
    SourceManager& SM = PP.getSourceManager();
    FileManager& FM = SM.getFileManager();
    HeaderSearch& HSearch = PP.getHeaderSearchInfo();
    const bool isFramework = false;

    // Add all the new entries into Preprocessor
    for (const size_t N = HOpts.UserEntries.size(); Idx < N; ++Idx) {
      const HeaderSearchOptions::Entry& E = HOpts.UserEntries[Idx];
      if (auto DE = FM.getOptionalDirectoryRef(E.Path))
        HSearch.AddSearchPath(DirectoryLookup(*DE, SrcMgr::C_User, isFramework),
                              E.Group == frontend::Angled);
    }

    //**//if (m_CUDACompiler)
    //**//  m_CUDACompiler->getPTXInterpreter()->AddIncludePaths(PathStr, Delm);
  }

  ///\brief Adds a single include path (-I).
  ///
  void AddIncludePath(llvm::StringRef PathsStr) {
    return AddIncludePaths(PathsStr, nullptr);
  }

  CompilationResult
  loadLibrary(const std::string& filename, bool lookup) {
    DynamicLibraryManager* DLM = getDynamicLibraryManager();
    std::string canonicalLib;
    if (lookup)
      canonicalLib = DLM->lookupLibrary(filename);

    const std::string &library = lookup ? canonicalLib : filename;
    if (!library.empty()) {
      switch (DLM->loadLibrary(library, /*permanent*/false, /*resolved*/true)) {
      case DynamicLibraryManager::kLoadLibSuccess: // Intentional fall through
      case DynamicLibraryManager::kLoadLibAlreadyLoaded:
        return kSuccess;
      case DynamicLibraryManager::kLoadLibNotFound:
        assert(0 && "Cannot find library with existing canonical name!");
        return kFailure;
      default:
        // Not a source file (canonical name is non-empty) but can't load.
        return kFailure;
      }
    }
    return kMoreInputExpected;
  }

  std::string toString(const char* type, void* obj) {
//**//
/*    LockCompilationDuringUserCodeExecutionRAII LCDUCER(*this);
    cling::valuePrinterInternal::declarePrintValue(*this);
    std::string buf, ret;
    llvm::raw_string_ostream ss(buf);
    ss << "*((std::string*)" << &ret << ") = cling::printValue((" << type << "*)"
       << obj << ");";
    CompilationResult result = process(ss.str().c_str());
    if (result != cling::Interpreter::kSuccess)
      llvm::errs() << "Error in Interpreter::toString: the input " << ss.str()
                   << " cannot be evaluated";

    return ret;
*/
    std::string ret;
    return ret;
  }


  }; //Interpreter
} //InterOp

#endif //USE_REPL


#endif //INTEROP_COMPATIBILITY
