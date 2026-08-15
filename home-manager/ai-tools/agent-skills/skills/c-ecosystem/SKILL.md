---
name: c-ecosystem
description: Use for C projects - C11/C17/C23 language patterns, Makefile, gcc/clang toolchain, valgrind, getopt, memory management, and CLI development.
version: 3.0.0
---

Patterns for C23 and modern C: language features, memory management, toolchain configuration, and CLI
development. Assumes familiarity with C syntax and pointers.

## Toolchain

**gcc**: GCC 15 defaults to `-std=gnu23`; C23 support is feature-complete there. GCC 8-14 default to
`gnu17`. Use `-std=c23` (or `gnu23`) explicitly rather than relying on the default across compiler versions.
Flags: `-Wall -Wextra -Wpedantic -Werror -Wshadow -Wconversion -Wstrict-prototypes -fanalyzer` (GCC 10+
static analysis).

**clang**: `-std=c23` from Clang 18+, `-std=c2y` (post-C23 draft) from Clang 19+. Same warning set;
`-Weverything` is useful for one-off audits but too noisy to run in CI unbounded.

### Sanitizers

- **AddressSanitizer** (`-fsanitize=address -fno-omit-frame-pointer -g`): catches buffer overflow,
  use-after-free. Cannot combine with ThreadSanitizer.
- **UndefinedBehaviorSanitizer** (`-fsanitize=undefined -g`): catches signed overflow, misaligned access,
  null-pointer UB that a normal build silently miscompiles around instead of crashing on.
- **ThreadSanitizer** (`-fsanitize=thread`): data-race detection. Mutually exclusive with ASan.
- **MemorySanitizer** (`-fsanitize=memory`, Clang only): flags reads of uninitialized memory, which ASan
  and UBSan do not catch because uninitialized-but-in-bounds reads are not memory-safety violations to them.

### Static and runtime analysis

- `clang-format -i src/*.c include/*.h` — formatter; project `.clang-format` sets style, indent, column
  limit, include ordering.
- `clang-tidy src/*.c -- -std=c23` — linter; project `.clang-tidy` scopes checks and can mark them
  `WarningsAsErrors`.
- `cppcheck --enable=all --error-exitcode=1 src/` — separate static-analysis engine, catches classes of bug
  clang-tidy's checks don't cover (e.g. some resource-leak patterns).
- `valgrind --leak-check=full --show-leak-kinds=all ./myapp` (memcheck, the default tool) finds leaks and
  invalid accesses **at runtime, on the exact code path exercised** — it proves nothing about paths not
  run, unlike a sanitizer instrumented into every path at compile time. `helgrind` for thread errors,
  `cachegrind`/`callgrind` for profiling.

### Testing

Check or Unity for minimal-dependency unit testing, Unity specifically for embedded/resource-constrained
targets, cmocka for BDD style.

```c
#include <check.h>

START_TEST(test_addition) {
    ck_assert_int_eq(1 + 1, 2);
}
END_TEST

Suite *math_suite(void) {
    Suite *s = suite_create("Math");
    TCase *tc = tcase_create("Core");
    tcase_add_test(tc, test_addition);
    suite_add_tcase(s, tc);
    return s;
}

int main(void) {
    Suite *s = math_suite();
    SRunner *sr = srunner_create(s);
    srunner_run_all(sr, CK_NORMAL);
    int failed = srunner_ntests_failed(sr);
    srunner_free(sr);
    return failed ? EXIT_FAILURE : EXIT_SUCCESS;
}
```

## Build systems

Make for simple Unix-only projects, CMake or Meson for cross-platform/dependency-heavy projects, Meson
specifically when build speed matters.

The flag set worth pinning in any of them is `-std=c23 -Wall -Wextra -Wpedantic -Werror`; the skeleton around
it is not worth restating.

```cmake
cmake_minimum_required(VERSION 3.30)
project(myapp VERSION 1.0.0 LANGUAGES C)
set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)
add_executable(myapp src/main.c src/utils.c)
target_compile_options(myapp PRIVATE
  $<$<C_COMPILER_ID:GNU,Clang>:-Wall -Wextra -Wpedantic -Werror>)
```

For detailed CMake patterns see the cplusplus-ecosystem skill.

```meson
project('myapp', 'c',
  version: '1.0.0',
  default_options: ['c_std=c23', 'warning_level=3', 'werror=true'])
executable('myapp', files('src/main.c', 'src/utils.c'), include_directories: include_directories('include'))
```

## Language: version-specific behavior

**C11** (ISO/IEC 9899:2011): `_Generic` type-generic selection, `_Atomic`, `_Thread_local`, `_Static_assert`,
`_Alignas`/`_Alignof`, `_Noreturn`, anonymous struct/union members.

**C17** (ISO/IEC 9899:2018): bugfix-only release, no new features. `__STDC_VERSION__` is `201710L`.
`ATOMIC_VAR_INIT` deprecated.

**C23** (ISO/IEC 9899:2024, published 2024-10-31, GCC 15 default): the current standard, with meaningful
C++-alignment changes that change what "correct" code looks like.

- `nullptr` / `nullptr_t` — **use this over `NULL`**: `NULL` is an integer-constant macro, so it participates
  in integer promotion and varargs ambiguity in ways `nullptr` cannot.
- `auto` now performs type inference for variable declarations, not just as a storage-class no-op.
- `constexpr` applies **only to object definitions**, not functions (unlike C++) — `constexpr int MAX = 1024;`
  is compile-time-constant in a way `#define` and even `const` are not (no memory, no runtime read).
- `typeof` / `typeof_unqual` standardized.
- `{}` performs zero-initialization for any type, including VLAs.
- `#embed` directive (plus `__has_embed`) embeds a binary resource as an initializer list at translate time,
  replacing the old objcopy/xxd-into-header workaround.
- `static_assert` takes a single argument (message no longer required).
- Digit separators: `1'000'000`, `0xFF'FF'FF'FF`.
- `#elifdef` / `#elifndef` preprocessor directives.
- Labels are now permitted at the end of a compound statement (before the closing brace) — previously a
  `label:` immediately before `}` was a constraint violation.
- Attributes: `[[nodiscard]]`, `[[maybe_unused]]`, `[[deprecated("...")]]`, `[[fallthrough]]`, `[[noreturn]]`,
  `[[reproducible]]`, `[[unsequenced]]`. `[[nodiscard]]` on a function whose return value encodes an error
  (e.g. an allocation result) turns a silently-ignored failure into a compiler warning.
- `bool`/`true`/`false` are keywords, not `stdbool.h` macros.
- **Implicit function declarations are a hard error in C23** (removed as a feature in C99, but many
  toolchains still only warned until C23 made it a constraint violation) — a call to an undeclared function
  that previously default-returned `int` and often crashed at link time or worse at runtime now fails to
  compile instead.

### Type system

Fixed-width types from `<stdint.h>` (`int8_t`…`uint64_t`, `intptr_t`/`uintptr_t`, `size_t`, `ptrdiff_t`) —
use these over `int`/`long` whenever a wire format, struct layout, or overflow boundary matters.

`_Generic` dispatches by argument type at compile time, giving a type-safe generic interface without `void*`:

```c
#define print_value(x) _Generic((x), \
    int: print_int, double: print_double, char *: print_string, default: print_unknown)(x)
```

Compound literals create anonymous struct/array values in place: `draw((struct point){.x = 10, .y = 20});`,
`int *arr = (int[]){1, 2, 3, 4, 5};`. Designated initializers name fields explicitly and leave the rest
zero-initialized, which is why they should be preferred over positional struct literals — a field added to
the struct later doesn't silently shift every positional initializer after it.

### Concurrency

`_Atomic` for lock-free counters, pthreads for anything needing real synchronization primitives (mutexes,
condition variables). `atomic_fetch_add`/`atomic_load` operate on `_Atomic int` without a lock. `_Thread_local`
gives per-thread storage duration, e.g. a thread-local `errno`-style value.

### Undefined behavior

The categories that matter because **the compiler is licensed to assume they never happen**, and optimizes
on that assumption rather than trapping it:

- **Memory**: null-pointer dereference, use-after-free, buffer overflow, double-free.
- **Arithmetic**: signed integer overflow (unlike unsigned, which wraps by defined semantics), division by
  zero, shift by an amount ≥ the type's width.
- **Aliasing**: strict-aliasing violations, type punning through anything other than a union.
- **Sequencing**: modifying the same object twice between sequence points (e.g. `i = i++ + 1;`).

ASan/UBSan turn these from silent miscompilation into a crash at the point of violation — run them during
development, not just before release, since the failure point they report is often far from where the bug
would otherwise first manifest.

### Traps that don't announce themselves

- `strncpy` does **not** null-terminate the destination when the source is `>=` the given size — it is not
  a safe drop-in for `strcpy`. Use `snprintf` for string construction instead.
- `sprintf` has no bounds checking; use `snprintf` with an explicit buffer size.
- `gets()` has no bounds checking and is removed from the standard; use `fgets()`.
- `scanf("%s", buf)` is as unbounded as `gets()`; use a width specifier (`%Ns`) or `fgets` + `sscanf`.
- Passing user input as a format string (`printf(user_input)`) is a format-string vulnerability; always
  `printf("%s", user_input)`.
- An unchecked `malloc` return doesn't crash immediately — the crash happens later, at the first dereference,
  far from the allocation site.

## Memory management

Choice of allocator follows lifetime and access pattern: stack/VLA for short-lived function-scoped data,
`malloc`/`free` with a single clear owner for long-lived single-owner data, an arena for many allocations
freed together (parsers, compilers, per-request state), a pool for fixed-size objects needing O(1)
alloc/free (game entities, connections).

```c
char *duplicate_string(const char *src) {
    if (!src) return NULL;
    size_t len = strlen(src) + 1;
    char *dst = malloc(len);
    if (!dst) return NULL;
    memcpy(dst, src, len);
    return dst;
}
```

Rules: always check `malloc`/`calloc`/`realloc` for `NULL`; use `calloc` when zero-initialization is needed;
after `realloc`, the original pointer may already be freed even on failure paths that return the old block
unchanged, so only ever dereference the returned pointer; set a pointer to `NULL` after `free` so a stray
use-after-free at least becomes a null-deref instead of silently reading freed memory.

### Arena allocator

Bulk allocation with a single deallocation point — nothing is freed individually, so there is no
per-object leak to track, only whether `arena_destroy` runs.

```c
typedef struct { char *base; size_t size; size_t offset; } Arena;

Arena arena_create(size_t size) {
    Arena a = {0};
    a.base = malloc(size);
    if (a.base) a.size = size;
    return a;
}

void *arena_alloc(Arena *a, size_t bytes) {
    size_t aligned = (bytes + 7) & ~7;  // 8-byte alignment
    if (a->offset + aligned > a->size) return NULL;
    void *ptr = a->base + a->offset;
    a->offset += aligned;
    return ptr;
}

void arena_reset(Arena *a) { a->offset = 0; }
void arena_destroy(Arena *a) { free(a->base); *a = (Arena){0}; }
```

### Pool allocator

Fixed-size objects via an intrusive free list threaded through the unused blocks themselves, giving O(1)
alloc/free with no search.

```c
typedef struct PoolBlock { struct PoolBlock *next; } PoolBlock;
typedef struct { PoolBlock *free_list; char *memory; size_t object_size; size_t capacity; } Pool;

Pool pool_create(size_t object_size, size_t count) {
    Pool p = {0};
    size_t size = object_size > sizeof(PoolBlock) ? object_size : sizeof(PoolBlock);
    p.memory = malloc(size * count);
    if (!p.memory) return p;
    p.object_size = size;
    p.capacity = count;
    for (size_t i = 0; i < count; i++) {
        PoolBlock *block = (PoolBlock *)(p.memory + i * size);
        block->next = p.free_list;
        p.free_list = block;
    }
    return p;
}

void *pool_alloc(Pool *p) {
    if (!p->free_list) return NULL;
    PoolBlock *block = p->free_list;
    p->free_list = block->next;
    return block;
}

void pool_free(Pool *p, void *ptr) {
    PoolBlock *block = ptr;
    block->next = p->free_list;
    p->free_list = block;
}

void pool_destroy(Pool *p) { free(p->memory); *p = (Pool){0}; }
```

### goto cleanup

A single exit path for error handling avoids the combinatorial leak risk of freeing at every early return:

```c
int process_file(const char *path) {
    int result = -1;
    FILE *fp = NULL;
    char *buffer = NULL;

    fp = fopen(path, "r");
    if (!fp) goto cleanup;
    buffer = malloc(BUFFER_SIZE);
    if (!buffer) goto cleanup;
    // ... process file ...
    result = 0;

cleanup:
    free(buffer);
    if (fp) fclose(fp);
    return result;
}
```

`free(NULL)` and passing `NULL` to `fclose` guard-checked above are both why the cleanup label works
unconditionally on partially-initialized state.

## CLI development

`getopt()` for simple short flags, `getopt_long()` once long options are needed, `argp` (GNU extension) when
you also want generated `--help` output.

```c
while ((opt = getopt(argc, argv, "vo:h")) != -1) {
    switch (opt) {
    case 'v': verbose = 1; break;
    case 'o': output = optarg; break;
    case 'h': printf("Usage: %s [-v] [-o output] [file...]\n", argv[0]); return 0;
    default:  fprintf(stderr, "Usage: %s [-v] [-o output] [file...]\n", argv[0]); return 1;
    }
}
// Remaining positional args: argv[optind] .. argv[argc-1]
```

```c
static struct option long_options[] = {
    {"verbose", no_argument, NULL, 'v'},
    {"output", required_argument, NULL, 'o'},
    {"help", no_argument, NULL, 'h'},
    {NULL, 0, NULL, 0},
};
while ((opt = getopt_long(argc, argv, "vo:h", long_options, NULL)) != -1) { /* same switch */ }
```

Exit codes: `0` (`EXIT_SUCCESS`), `1` (`EXIT_FAILURE`, general error), `2` (usage error), `126` (found but
not executable), `127` (not found), `128+N` (killed by signal N). `<sysexits.h>` gives finer-grained codes
(`EX_USAGE` 64, `EX_NOINPUT` 66, etc.) for scripts and other programs that branch on exit status.

Signal handling: use `sigaction()`, not `signal()` — `signal()`'s semantics (whether the handler resets,
whether it blocks the same signal) vary across platforms, `sigaction()`'s don't. Keep the handler itself to
setting a flag; anything more risks calling a non-async-signal-safe function from signal context. The flag
must be `_Atomic` (or `volatile sig_atomic_t`) since the handler can fire between any two instructions of the
main flow.

```c
static atomic_int running = 1;
static void handle_signal(int sig) { (void)sig; running = 0; }

int main(void) {
    struct sigaction sa = {0};
    sa.sa_handler = handle_signal;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGINT, &sa, NULL);
    sigaction(SIGTERM, &sa, NULL);
    while (running) { /* main loop */ }
    return 0;
}
```

Error reporting: prefix messages with the program name (derived from `argv[0]` via `strrchr(argv0, '/')`),
and separate the "no errno" case from the "has errno" case so the latter appends `strerror(errno)` — capture
`errno` into a local **before** calling any other libc function (including `fprintf`), since those can
clobber it.

```c
void error_errno(const char *fmt, ...) {
    int saved_errno = errno;
    fprintf(stderr, "%s: ", progname);
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fprintf(stderr, ": %s\n", strerror(saved_errno));
}
```

## Related

- [cplusplus-ecosystem](../cplusplus-ecosystem/SKILL.md) — detailed CMake patterns and clang-tidy configuration
  shared across C and C++ builds
- [context7-usage](../context7-usage/SKILL.md) — fetch current C documentation via the `cppreference_com`
  Context7 library id, for `<stdatomic.h>`, `malloc`, and other standard-library lookups
- [investigation-patterns](../investigation-patterns/SKILL.md) — debugging with Valgrind, GDB, and sanitizers
- [serena-usage](../serena-usage/SKILL.md) — navigating C codebases and header hierarchies by symbol
