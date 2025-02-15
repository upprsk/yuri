#include <cstdio>
#include <memory>
#include <optional>
#include <string>

#include "codegen/mips2.hpp"
#include "codegen/ssir.hpp"
#include "cpptrace/from_current.hpp"
#include "error_reporter.hpp"
#include "fmt/base.h"
#include "fmt/core.h"
#include "fmt/ranges.h"  // IWYU pragma: keep
#include "parser.hpp"
#include "tokenizer.hpp"
#include "types.hpp"

auto read_entire_file(std::string const& path) -> std::optional<std::string> {
    std::unique_ptr<FILE, void (*)(FILE*)> f = {fopen(path.c_str(), "rb"),
                                                [](auto f) { fclose(f); }};
    if (!f) return std::nullopt;

    fseek(f.get(), 0, SEEK_END);
    auto len = ftell(f.get());

    fseek(f.get(), 0, SEEK_SET);

    std::string s;
    s.resize(len);
    if (static_cast<typeof(len)>(fread(
            s.data(), sizeof(std::string::value_type), len, f.get())) != len)
        return std::nullopt;

    return s;
}

auto main(int argc, char** argv) -> int {
    CPPTRACE_TRY {
        if (argc < 2) {
            fmt::println(stderr, "usage: {} <program>", argv[0]);
            return 1;
        }

        auto contents = read_entire_file(argv[1]);
        if (!contents) {
            fmt::println(stderr, "failed to read file: {}", argv[1]);
            return 1;
        }

        yuri::ErrorReporter er{*contents, argv[1]};

        auto tokens = yuri::tokenize(&er, *contents);
        auto ast = yuri::parse(&er, *contents, tokens);

        yuri::Env env;
        env.define("int", yuri::Type::make_type(), yuri::Type::Int());
        env.define("bool", yuri::Type::make_type(), yuri::Type::Bool());
        env.define("void", yuri::Type::make_type(), yuri::Type::Void());

        ast.add_types(env, er);
        // fmt::println("{}", ast);

        if (er.had_error()) return 1;

        auto m = yuri::ssir::codegen(ast, er);
        yuri::ssir::dump_module(m);

        if (er.had_error()) return 1;

        yuri::mips::v2::codegen_stdout(m, er);

        return 0;
    }
    CPPTRACE_CATCH(std::exception const& e) {
        fmt::println(stderr, "exception: {}", e.what());
        cpptrace::from_current_exception().print();
    }
}
