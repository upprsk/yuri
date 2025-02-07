#include "ast.hpp"

#include "fmt/ranges.h"  // IWYU pragma: keep
#include "fmt/std.h"     // IWYU pragma: keep

auto fmt::formatter<yuri::AstNodeKind>::format(yuri::AstNodeKind c,
                                               format_context&   ctx) const
    -> format_context::iterator {
    using T = yuri::AstNodeKind;

    string_view name = "unknown";
    switch (c) {
        case T::Nil: name = "Nil"; break;
        case T::SourceFile: name = "SourceFile"; break;
        case T::Func: name = "Func"; break;
        case T::VarDecl: name = "VarDecl"; break;
        case T::Block: name = "Block"; break;
        case T::ExprStmt: name = "ExprStmt"; break;
        case T::ReturnStmt: name = "ReturnStmt"; break;
        case T::WhileStmt: name = "WhileStmt"; break;
        case T::Assign: name = "Assign"; break;
        case T::Add: name = "Add"; break;
        case T::Sub: name = "Sub"; break;
        case T::Mul: name = "Mul"; break;
        case T::Div: name = "Div"; break;
        case T::Id: name = "Id"; break;
        case T::Int: name = "Int"; break;
        case T::Err: name = "Err"; break;
    }

    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yuri::AstNode>::format(yuri::AstNode   t,
                                           format_context& ctx) const
    -> format_context::iterator {
    return fmt::format_to(ctx.out(), "{{{}, {}, {}, {}}}", t.kind, t.span,
                          t.value, t.children);
}
