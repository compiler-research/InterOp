
#include "Utils.h"

#include "cling/Interpreter/Interpreter.h"

#include "clang/AST/ASTContext.h"
#include "clang/Interpreter/InterOp.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Sema/Sema.h"

#include "gtest/gtest.h"

using namespace TestUtils;
using namespace llvm;
using namespace clang;
using namespace cling;

TEST(VariableReflectionTest, GetDatamembers) {
  std::vector<Decl*> Decls;
  std::string code = R"(
    class C {
    public:
      int a;
      static int b;
    private:
      int c;
      static int d;
    protected:
      int e;
      static int f;
    };
    )";

  GetAllTopLevelDecls(code, Decls);
  auto datamembers = InterOp::GetDatamembers(Decls[0]);

  EXPECT_EQ(InterOp::GetQualifiedName(datamembers[0]), "C::a");
  EXPECT_EQ(InterOp::GetQualifiedName(datamembers[1]), "C::c");
  EXPECT_EQ(InterOp::GetQualifiedName(datamembers[2]), "C::e");
  EXPECT_EQ(datamembers.size(), 3);
}

TEST(VariableReflectionTest, LookupDatamember) {
  std::vector<Decl*> Decls;
  std::string code = R"(
    class C {
    public:
      int a;
      static int b;
    private:
      int c;
      static int d;
    protected:
      int e;
      static int f;
    };
    )";

  GetAllTopLevelDecls(code, Decls);
  Sema *S = &Interp->getCI()->getSema();

  EXPECT_EQ(InterOp::GetQualifiedName(InterOp::LookupDatamember(S, "a", Decls[0])), "C::a");
  EXPECT_EQ(InterOp::GetQualifiedName(InterOp::LookupDatamember(S, "c", Decls[0])), "C::c");
  EXPECT_EQ(InterOp::GetQualifiedName(InterOp::LookupDatamember(S, "e", Decls[0])), "C::e");
}

TEST(VariableReflectionTest, GetVariableType) {
  std::vector<Decl*> Decls;
  std::string code = R"(
    class C {};

    template<typename T>
    class E {};

    int a;
    char b;
    C c;
    C *d;
    E<int> e;
    E<int> *f;
    int g[4];
    )";

  GetAllTopLevelDecls(code, Decls);

  EXPECT_EQ(InterOp::GetTypeAsString(InterOp::GetVariableType(Decls[2])), "int");
  EXPECT_EQ(InterOp::GetTypeAsString(InterOp::GetVariableType(Decls[3])), "char");
  EXPECT_EQ(InterOp::GetTypeAsString(InterOp::GetVariableType(Decls[4])), "class C");
  EXPECT_EQ(InterOp::GetTypeAsString(InterOp::GetVariableType(Decls[5])), "class C *");
  EXPECT_EQ(InterOp::GetTypeAsString(InterOp::GetVariableType(Decls[6])), "E<int>");
  EXPECT_EQ(InterOp::GetTypeAsString(InterOp::GetVariableType(Decls[7])), "E<int> *");
  EXPECT_EQ(InterOp::GetTypeAsString(InterOp::GetVariableType(Decls[8])), "int [4]");
}

TEST(VariableReflectionTest, GetVariableOffset) {
  std::vector<Decl *> Decls;
  std::string code = R"(
    int a;
    const int N = 5;
    class C {
    public:
      int a;
      double b;
      int *c;
      int d;

      static int s_a;
    };
    int C::s_a = 12;
    )";

  class C{
  public:
    int a;
    double b;
    int *c;
    int d;
  } c;


  GetAllTopLevelDecls(code, Decls);
  auto datamembers = InterOp::GetDatamembers(Decls[2]);

  EXPECT_TRUE((bool) InterOp::GetVariableOffset(Interp.get(), Decls[0])); // a
  EXPECT_TRUE((bool) InterOp::GetVariableOffset(Interp.get(), Decls[1])); // N

  EXPECT_EQ(InterOp::GetVariableOffset(Interp.get(), datamembers[0]),
          0);
  EXPECT_EQ(InterOp::GetVariableOffset(Interp.get(), datamembers[1]),
          ((unsigned long) &(c.b)) - ((unsigned long) &(c.a)));
  EXPECT_EQ(InterOp::GetVariableOffset(Interp.get(), datamembers[2]),
          ((unsigned long) &(c.c)) - ((unsigned long) &(c.a)));
  EXPECT_EQ(InterOp::GetVariableOffset(Interp.get(), datamembers[3]),
          ((unsigned long) &(c.d)) - ((unsigned long) &(c.a)));

  Sema *S = &Interp->getCI()->getSema();
  auto *VD_C_s_a = InterOp::GetNamed(S, "s_a", Decls[2]); // C::s_a
  EXPECT_TRUE((bool) InterOp::GetVariableOffset(Interp.get(), VD_C_s_a));
}

TEST(VariableReflectionTest, IsPublicVariable) {
  std::vector<Decl *> Decls, SubDecls;
  std::string code = R"(
    class C {
    public:
      int a;
    private:
      int b;
    protected:
      int c;
    };
    )";

  GetAllTopLevelDecls(code, Decls);
  GetAllSubDecls(Decls[0], SubDecls);

  EXPECT_TRUE(InterOp::IsPublicVariable(SubDecls[2]));
  EXPECT_FALSE(InterOp::IsPublicVariable(SubDecls[4]));
  EXPECT_FALSE(InterOp::IsPublicVariable(SubDecls[6]));
}

TEST(VariableReflectionTest, IsProtectedVariable) {
  std::vector<Decl *> Decls, SubDecls;
  std::string code = R"(
    class C {
    public:
      int a;
    private:
      int b;
    protected:
      int c;
    };
    )";

  GetAllTopLevelDecls(code, Decls);
  GetAllSubDecls(Decls[0], SubDecls);

  EXPECT_FALSE(InterOp::IsProtectedVariable(SubDecls[2]));
  EXPECT_FALSE(InterOp::IsProtectedVariable(SubDecls[4]));
  EXPECT_TRUE(InterOp::IsProtectedVariable(SubDecls[6]));
}

TEST(VariableReflectionTest, IsPrivateVariable) {
  std::vector<Decl *> Decls, SubDecls;
  std::string code = R"(
    class C {
    public:
      int a;
    private:
      int b;
    protected:
      int c;
    };
    )";

  GetAllTopLevelDecls(code, Decls);
  GetAllSubDecls(Decls[0], SubDecls);

  EXPECT_FALSE(InterOp::IsPrivateVariable(SubDecls[2]));
  EXPECT_TRUE(InterOp::IsPrivateVariable(SubDecls[4]));
  EXPECT_FALSE(InterOp::IsPrivateVariable(SubDecls[6]));
}

TEST(VariableReflectionTest, IsStaticVariable) {
  std::vector<Decl *> Decls, SubDecls;
  std::string code =  R"(
    class C {
      int a;
      static int b;
    };
    )";

  GetAllTopLevelDecls(code, Decls);
  GetAllSubDecls(Decls[0], SubDecls);

  EXPECT_FALSE(InterOp::IsStaticVariable(SubDecls[1]));
  EXPECT_TRUE(InterOp::IsStaticVariable(SubDecls[2]));
}

TEST(VariableReflectionTest, IsConstVariable) {
  std::vector<Decl *> Decls, SubDecls;
  std::string code =  R"(
    class C {
      int a;
      const int b = 2;
    };
    )";

  GetAllTopLevelDecls(code, Decls);
  GetAllSubDecls(Decls[0], SubDecls);

  EXPECT_FALSE(InterOp::IsConstVariable(SubDecls[1]));
  EXPECT_TRUE(InterOp::IsConstVariable(SubDecls[2]));
}

TEST(VariableReflectionTest, DISABLED_GetArrayDimensions) {
  std::vector<Decl *> Decls, SubDecls;
  std::string code =  R"(
    int a;
    int b[1];
    int c[1][2];
    )";

  GetAllTopLevelDecls(code, Decls);
  GetAllSubDecls(Decls[0], SubDecls);
  GetAllSubDecls(Decls[1], SubDecls);
  GetAllSubDecls(Decls[2], SubDecls);
  auto is_vec_eq = [](const std::vector<size_t> &arr_dims,
                      const std::vector<size_t> &truth_vals) {
    if (arr_dims.size() != truth_vals.size()) 
      return false;
    
    return std::equal(arr_dims.begin(), arr_dims.end(), truth_vals.begin());
  };

   EXPECT_TRUE(is_vec_eq(InterOp::GetArrayDimensions(Decls[0]), {}));
   EXPECT_TRUE(is_vec_eq(InterOp::GetArrayDimensions(Decls[1]), {1}));
   EXPECT_TRUE(is_vec_eq(InterOp::GetArrayDimensions(Decls[2]), {1,2}));
}
