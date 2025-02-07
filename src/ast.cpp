#include "ast.hpp"

#include <stdexcept>

#include "fmt/ranges.h"  // IWYU pragma: keep
#include "fmt/std.h"     // IWYU pragma: keep
#include "types.hpp"

namespace yuri {

auto AstNode::add_types() -> Type {
    switch (kind) {
        case AstNodeKind::Nil: return set_type(Type::Void());
        case AstNodeKind::SourceFile: {
            for (auto& node : children) node.add_types();

            return set_type(Type::Void());
        }
        case AstNodeKind::Func: {
            for (auto& node : children) node.add_types();

            return set_type(Type::Func());
        }
        case AstNodeKind::VarDecl:
        case AstNodeKind::Block:
        case AstNodeKind::ExprStmt:
        case AstNodeKind::ReturnStmt:
        case AstNodeKind::WhileStmt:
        case AstNodeKind::Assign:
        case AstNodeKind::Add:
        case AstNodeKind::Sub:
        case AstNodeKind::Mul:
        case AstNodeKind::Div:
        case AstNodeKind::Id:
        case AstNodeKind::Int:
        case AstNodeKind::Err: break;
    }

    throw std::runtime_error(fmt::format("not implemented for {}", *this));
}

}  // namespace yuri

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
    return fmt::format_to(ctx.out(), "{{{}, {}, {}, {}, {}}}", t.kind, t.type,
                          t.span, t.value, t.children);
}
