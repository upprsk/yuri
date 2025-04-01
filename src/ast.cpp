#include "ast.hpp"

#include <variant>

#include "fmt/base.h"
#include "fmt/ranges.h"
#include "fmt/std.h"

auto fmt::formatter<yuri::AstNodeKind>::format(yuri::AstNodeKind n,
                                               format_context&   ctx) const
    -> format_context::iterator {
    string_view name = "unknown";
    switch (n) {
        case yuri::AstNodeKind::Err: name = "Err"; break;
        case yuri::AstNodeKind::VarDecl: name = "VarDecl"; break;
        case yuri::AstNodeKind::Block: name = "Block"; break;
        case yuri::AstNodeKind::ExprStmt: name = "ExprStmt"; break;
        case yuri::AstNodeKind::ReturnStmt: name = "ReturnStmt"; break;
        case yuri::AstNodeKind::Assign: name = "Assign"; break;
        case yuri::AstNodeKind::Add: name = "Add"; break;
        case yuri::AstNodeKind::Sub: name = "Sub"; break;
        case yuri::AstNodeKind::Mul: name = "Mul"; break;
        case yuri::AstNodeKind::Div: name = "Div"; break;
        case yuri::AstNodeKind::Eq: name = "Eq"; break;
        case yuri::AstNodeKind::Neq: name = "Neq"; break;
        case yuri::AstNodeKind::Lt: name = "Lt"; break;
        case yuri::AstNodeKind::Lte: name = "Lte"; break;
        case yuri::AstNodeKind::Gt: name = "Gt"; break;
        case yuri::AstNodeKind::Gte: name = "Gte"; break;
        case yuri::AstNodeKind::Neg: name = "Neg"; break;
        case yuri::AstNodeKind::Int: name = "Int"; break;
        case yuri::AstNodeKind::Id: name = "Id"; break;
    }
    return formatter<string_view>::format(name, ctx);
}

auto fmt::formatter<yuri::AstNode>::format(yuri::AstNode   n,
                                           format_context& ctx) const
    -> format_context::iterator {
    if (std::holds_alternative<std::monostate>(n.value)) {
        return fmt::format_to(ctx.out(), "{}([{}])", n.kind,
                              fmt::join(n.children, ", "));
    }

    if (n.children.size() == 0) {
        return fmt::format_to(ctx.out(), "{}({})", n.kind, n.value);
    }

    return fmt::format_to(ctx.out(), "{}({}, [{}])", n.kind, n.value,
                          fmt::join(n.children, ", "));
}
