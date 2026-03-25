---
name: cplusplus-ecosystem
description: "Use when working with C++ projects, 'CMakeLists.txt', 'Ninja', 'clang-tidy', 'clang-format', 'GoogleTest', 'Catch2', or Modern C++ (C++11-23) language patterns. Provides C++ ecosystem patterns for RAII, smart pointers, CMake builds, and testing frameworks."
---

Comprehensive patterns for Modern C++ (C++11-23), CMake build system, clang toolchain, and testing with GoogleTest/Catch2.

## Modern C++ Features

### Smart Pointers (RAII Memory Management)

```cpp
auto ptr = std::make_unique<MyClass>(args);     // Exclusive ownership, zero overhead
auto shared = std::make_shared<MyClass>(args);   // Shared ownership, reference counting
// weak_ptr: Non-owning observer of shared_ptr
```

### Move Semantics (C++11)

```cpp
std::vector<int> v1 = {1, 2, 3};
std::vector<int> v2 = std::move(v1); // v1 is now empty, no copy
```

### Key Features by Standard

- **C++11**: Move semantics, smart pointers, lambdas, `auto`, `constexpr` (simple)
- **C++14**: Generic lambdas, extended `constexpr` (loops/local vars)
- **C++17**: `if constexpr`, structured bindings, `std::optional`/`std::variant`/`std::any`
- **C++20**: Concepts, Ranges, Modules, Coroutines, spaceship operator (`<=>`)

### Concepts (C++20)

```cpp
template<typename T>
concept Addable = requires(T a, T b) { a + b; };

template<Addable T>
T add(T a, T b) { return a + b; }
```

### Ranges (C++20)

```cpp
auto result = numbers
    | std::views::filter([](int n) { return n % 2 == 0; })
    | std::views::transform([](int n) { return n * 2; });
```

### Vocabulary Types (C++17)

```cpp
std::optional<int> find_value(const std::vector<int>& v, int target) {
    auto it = std::find(v.begin(), v.end(), target);
    if (it != v.end()) return *it;
    return std::nullopt;
}

std::variant<int, std::string, double> data = 42;
std::visit([](auto&& val) { std::cout << val; }, data);
```

## Design Patterns

- **RAII**: Bind resource lifetime to object lifetime (files, sockets, locks)
- **Rule of Zero**: Prefer classes with no custom special member functions; use smart pointers and standard containers
- **Rule of Five**: If one special member function is defined, define all five (destructor, copy/move constructor, copy/move assignment)
- **PImpl**: Hide implementation details, reduce compile dependencies
- **CRTP**: Static polymorphism via Curiously Recurring Template Pattern
- **Type Erasure**: Hide concrete types behind uniform interface (`std::function`, `std::any`)

## Concurrency

```cpp
// Threads
auto future = std::async(std::launch::async, []{ return 42; });
int result = future.get();

// RAII locking
std::mutex mtx;
std::lock_guard<std::mutex> lock(mtx);

// Atomics
std::atomic<int> counter{0};
counter.fetch_add(1, std::memory_order_relaxed);
```

The agent should use `std::scoped_lock` for multiple mutexes to prevent deadlocks.

## CMake Build System

### Modern CMake (Target-Based)

```cmake
cmake_minimum_required(VERSION 3.20)
project(MyProject VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

add_library(mylib STATIC src/mylib.cpp)
target_include_directories(mylib PUBLIC include)
target_compile_features(mylib PUBLIC cxx_std_20)

add_executable(myapp src/main.cpp)
target_link_libraries(myapp PRIVATE mylib)
```

### Compiler Warnings

```cmake
add_library(project_warnings INTERFACE)
target_compile_options(project_warnings INTERFACE
  $<$<CXX_COMPILER_ID:GNU,Clang>:
    -Wall -Wextra -Wpedantic -Werror
    -Wshadow -Wnon-virtual-dtor -Wold-style-cast
    -Wcast-align -Wunused -Woverloaded-virtual
    -Wconversion -Wsign-conversion -Wnull-dereference
  >
)
```

### Build Commands

```bash
cmake -B build -G Ninja          # Configure with Ninja
cmake --build build              # Build
cmake --build build --target test # Run tests
cmake --install build --prefix /usr/local
```

## Toolchain

### clang-tidy

```bash
clang-tidy src/*.cpp -- -std=c++20
```

Key checks: `modernize-use-nullptr`, `modernize-use-override`, `bugprone-use-after-move`, `performance-unnecessary-copy-initialization`, `cppcoreguidelines-owning-memory`.

### clang-format

```bash
clang-format -i src/*.cpp include/*.hpp
```

### Sanitizers

```bash
# AddressSanitizer: buffer overflow, use-after-free
# UndefinedBehaviorSanitizer: signed overflow, null deref
# ThreadSanitizer: data races (cannot combine with ASan)
# MemorySanitizer: uninitialized reads (Clang only)
```

CMake integration:

```cmake
target_compile_options(myapp PRIVATE -fsanitize=address -fno-omit-frame-pointer)
target_link_options(myapp PRIVATE -fsanitize=address)
```

## Testing

### GoogleTest

```cpp
#include <gtest/gtest.h>

TEST(MyTest, BasicAssertion) {
    EXPECT_EQ(1 + 1, 2);
}

class MyFixture : public ::testing::Test {
protected:
    void SetUp() override { /* setup */ }
    void TearDown() override { /* cleanup */ }
};

TEST_F(MyFixture, FixtureTest) {
    EXPECT_TRUE(true);
}
```

CMake: `find_package(GTest REQUIRED)`, `gtest_discover_tests(tests)`.

### Catch2

```cpp
#include <catch2/catch_test_macros.hpp>

TEST_CASE("Basic arithmetic", "[math]") {
    REQUIRE(1 + 1 == 2);
    SECTION("multiplication") {
        REQUIRE(2 * 2 == 4);
    }
}
```

CMake: `find_package(Catch2 3 REQUIRED)`, `catch_discover_tests(tests)`.

## Anti-Patterns to Avoid

- **Raw pointer ownership** -- use `std::unique_ptr` or `std::shared_ptr`
- **Manual new/delete** -- use `std::make_unique`/`std::make_shared`
- **C-style casts** -- use `static_cast`, `dynamic_cast`, `const_cast`, `reinterpret_cast`
- **`using namespace` in headers** -- use fully qualified names
- **Throwing in destructors** -- mark destructors `noexcept`
- **`const_cast` abuse** -- fix the design instead

## Workflow

1. **Analyze**: Check CMakeLists.txt, review code patterns and standards, identify memory management needs
2. **Implement**: Use RAII and smart pointers, follow C++ Core Guidelines, prefer standard library
3. **Validate**: Build with warnings enabled, run static analysis, execute tests with sanitizers

## Best Practices

- Use smart pointers instead of raw pointers for ownership
- Enable `-Wall -Wextra -Werror` for all builds
- Run clang-tidy before committing, format with clang-format
- Prefer `const` correctness, `noexcept` for move constructors
- Use `constexpr` for compile-time computation
- Prefer `std::string_view` for non-owning string references
- Use range-based for loops and structured bindings
