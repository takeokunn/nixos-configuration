---
name: cplusplus-ecosystem
description: Use for C++ projects - CMake/Ninja build, clang-tidy/clang-format, GoogleTest/Catch2, and Modern C++ (C++20/23/26) language patterns.
version: 3.0.0
---

Patterns for Modern C++ (C++20/23/26), CMake/Ninja builds, and the clang toolchain, focused on
behavior that surprises, build hazards, and concrete tool invocations rather than language basics.

## C++20 modules and coroutines

**Modules** replace textual `#include` with a compiled interface, cutting redundant reparsing
across translation units. Requires CMake 3.28+ with `CMAKE_CXX_SCAN_FOR_MODULES ON`; GCC 15+ and
Clang 18+ have production-ready support. `import std;` (whole standard library as one module) needs
CMake 3.30+ and `CMAKE_CXX_MODULE_STD ON`.

```cpp
// math.cppm
export module math;
export int add(int a, int b) { return a + b; }
```

**Coroutines** provide `co_await`/`co_yield`/`co_return` but ship no promise types — a raw
`co_yield` needs a hand-written `generator<T>` promise, or a library (cppcoro, libcoro), before it
compiles. C++23's `std::generator` (below) removes this requirement for the generator case.

**Defaulted `operator<=>`** generates `==`, `!=`, `<`, `>`, `<=`, `>=` from one declaration — the
surprise is forgetting that the synthesized `==` is member-wise, not your intended custom equality.

## C++23 — stable (GCC 14+, Clang 18+, MSVC 17.10+; use `-std=c++23` in production)

- **`std::expected<T, E>`** — monadic error handling (`.transform`, `.transform_error`) as an
  alternative to exceptions for expected-failure paths.
- **`std::print` / `std::println`** — type-safe formatted output; replaces iostream/printf
  formatting entirely.
- **`std::flat_map` / `std::flat_set`** — contiguous sorted storage, faster than `std::map`/`std::set`
  for small-to-medium collections from cache locality; iterator-invalidation rules differ from the
  node-based containers, so don't carry over `map`/`set` assumptions.
- **Deducing `this`** — an explicit object parameter eliminates CRTP boilerplate and enables
  recursive lambdas: `auto fib = [](this auto self, int n) { return n < 2 ? n : self(n-1) + self(n-2); };`
- **`std::generator<T>`** — standard coroutine generator; no custom promise type needed.
- **`import std;`** — needs CMake 3.30+, GCC 15+/Clang 18+ with libc++, `CMAKE_CXX_MODULE_STD ON`.
- **`std::stacktrace`** — portable capture via `std::stacktrace::current()`; on GCC, link
  `-lstdc++_libbacktrace`.
- **`if consteval`** — detects compile-time evaluation context, replacing
  `std::is_constant_evaluated()`, so one function body can take a fast non-constexpr runtime path
  and a separate compile-time path.
- **Multidimensional `operator[]`** — `operator[](size_t row, size_t col)` is now legal; before
  C++23 this required `operator()`.

## C++26 — progressively landing; check the compiler version before relying on any of these

- **Reflection** (`^^` operator, `-std=c++2c -freflection`) — compile-time introspection of types
  and members (`members_of(^^T)`). GCC 16.1 (April 2026) is the first release shipping it.
- **Contracts** — `pre(...)`, `post(r: ...)`, `contract_assert(...)`. GCC 16.1 ships an
  experimental implementation; behavior is not yet stable across compilers.
- **`std::execution`** (P2300) — sender/receiver model replacing ad-hoc async with composable
  pipelines (`schedule | then | then`, `sync_wait`). Reference implementation: stdexec. Shipping in
  GCC 16 and libc++.
- **Pattern matching** (P2688) — not approved for C++26, expected C++29. `std::visit` +
  `std::variant` remains the only portable option until then.

## Silent failure modes

- **Throwing from a destructor calls `std::terminate`.** Destructors are implicitly `noexcept`
  since C++11; an exception escaping one during stack unwinding terminates the process instead of
  propagating. Mark destructors `noexcept` explicitly and handle errors internally.
- **Using an object after `std::move`ing it compiles silently** and reads a moved-from
  (valid-but-unspecified) state — `clang-tidy`'s `bugprone-use-after-move` is the only thing that
  reliably catches this; the compiler does not.
- **`CMAKE_CXX_EXTENSIONS` defaults ON**, silently compiling with `-std=gnu++23` instead of
  `-std=c++23` — GNU extensions pass locally and fail on a strict-standard compiler elsewhere. Set
  `CMAKE_CXX_EXTENSIONS OFF` explicitly.
- **Locking multiple mutexes individually risks deadlock** from inconsistent acquisition order
  across threads. `std::scoped_lock` locks an arbitrary set atomically via a deadlock-avoidance
  algorithm — use it instead of nested `lock_guard`s.
- **A class relying on the Rule of Zero breaks the moment one member becomes a raw owning
  pointer.** Defining any one of destructor/copy-ctor/move-ctor/copy-assign/move-assign suppresses
  the compiler's synthesis of the other four — define all five (Rule of Five) once a class manages a
  resource directly, or keep it at zero by holding only smart pointers and standard containers.

## CMake / Ninja

```cmake
cmake_minimum_required(VERSION 3.30)
project(MyProject VERSION 1.0.0 LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)   # see Silent failure modes above
```

**CMakePresets.json** (3.21+) pins generator, build dir, and cache variables per configuration so
`cmake --preset X && cmake --build --preset X` reproduces across machines without CLI flags:

```json
{
  "version": 6,
  "configurePresets": [{
    "name": "default", "generator": "Ninja",
    "binaryDir": "${sourceDir}/build/${presetName}",
    "cacheVariables": { "CMAKE_CXX_STANDARD": "23", "CMAKE_EXPORT_COMPILE_COMMANDS": "ON" }
  }]
}
```

**Dependency management**: vcpkg
(`-DCMAKE_TOOLCHAIN_FILE=$VCPKG_ROOT/scripts/buildsystems/vcpkg.cmake`, dependencies declared in
`vcpkg.json`) integrates directly with presets. Conan 2.x (not 1.x — CLI and generator format
changed) uses `conanfile.py`/`conanfile.txt` with CMake generators:
`conan install . --output-folder=build --build=missing`.

**Warning flags** worth enabling as an `INTERFACE` target so every consumer inherits them:

```cmake
target_compile_options(project_warnings INTERFACE
  $<$<CXX_COMPILER_ID:GNU,Clang>:
    -Wall -Wextra -Wpedantic -Werror -Wshadow -Wnon-virtual-dtor -Wold-style-cast
    -Wcast-align -Wunused -Woverloaded-virtual -Wconversion -Wsign-conversion -Wnull-dereference
  >
)
```

Common invocations: `cmake -B build -G Ninja`, `cmake --build build`, `cmake --build build --target
test`, `cmake --install build --prefix /usr/local`.

## Toolchain

**GCC 16 changed its default standard from C++17 to C++20** — a project relying on the implicit
default silently picks up new language rules on a compiler upgrade; pin `-std=c++23` (or
`CMAKE_CXX_STANDARD`) explicitly rather than trusting the default. GCC 15+ also defaults C mode to
C23. Clang 19+ has comprehensive C++23 support; `-std=c++2c` enables the experimental C++26 mode on
both compilers.

Sanitizer flags:
- AddressSanitizer — `-fsanitize=address -fno-omit-frame-pointer`: buffer overflow, use-after-free.
- UndefinedBehaviorSanitizer — `-fsanitize=undefined`: signed overflow, null dereference, and other UB.
- ThreadSanitizer — `-fsanitize=thread`: data races and deadlocks; **cannot be combined with
  AddressSanitizer** in the same binary.
- MemorySanitizer — `-fsanitize=memory`: uninitialized reads, Clang-only; requires every linked
  library to be instrumented or it reports false positives.

**clang-tidy** (19+ for C++23): `clang-tidy src/*.cpp -- -std=c++23`. A `.clang-tidy` enabling
`bugprone-*, clang-analyzer-*, cppcoreguidelines-*, modernize-*, performance-*, readability-*` with
`WarningsAsErrors: '*'` turns findings into build failures rather than ignorable diagnostics.
High-value checks: `bugprone-use-after-move` (the only reliable catch for the moved-from trap
above), `cppcoreguidelines-owning-memory`, `performance-unnecessary-copy-initialization`.

**clang-format** (19+ for C++23 syntax): `clang-format -i src/*.cpp include/*.hpp`. Set `Standard:
c++23` explicitly in `.clang-format` — an older `Standard` value can reformat C++23-only syntax
(multidimensional subscript, deducing-this) incorrectly.

## Testing

**GoogleTest**:
```cmake
enable_testing()
find_package(GTest REQUIRED)
add_executable(tests tests/test_main.cpp)
target_link_libraries(tests PRIVATE GTest::gtest GTest::gtest_main)
include(GoogleTest)
gtest_discover_tests(tests)
```
`TEST_F` binds a test to a `::testing::Test` subclass; its `SetUp`/`TearDown` overrides run once per
test, not once per fixture instance.

**Catch2** (v3):
```cmake
find_package(Catch2 3 REQUIRED)
target_link_libraries(tests PRIVATE Catch2::Catch2WithMain)
include(CTest)
include(Catch)
catch_discover_tests(tests)
```
Each `SECTION` re-runs the entire enclosing `TEST_CASE` body from the top for that section's path
alone — local state set up before the `SECTION`s is fresh on every run, so mutating a local inside
one `SECTION` never leaks into a sibling `SECTION`.

## Context7 library IDs

| Library | ID |
|---|---|
| cppreference | `/websites/cppreference_com` |
| CMake | `/Kitware/CMake` |
| GoogleTest | `/google/googletest` |
| Catch2 | `/catchorg/Catch2` |

## Related

- [serena-usage](../serena-usage/SKILL.md) — symbol-level navigation and refactoring for C++ code.
- [context7-usage](../context7-usage/SKILL.md) — pulling current docs via the library IDs above
  instead of relying on possibly-stale training data.
- [investigation-patterns](../investigation-patterns/SKILL.md) — debugging with sanitizers,
  valgrind, and gdb once a sanitizer above has flagged a fault.
- [technical-documentation](../technical-documentation/SKILL.md) — writing Doxygen-based library
  documentation.
