---
name: c-ecosystem
description: "Use when working with C projects, 'C11', 'C17', 'C23', 'Makefile', 'gcc', 'clang', 'valgrind', 'getopt', or C language patterns. Provides Modern C (C11-C23) patterns, memory management strategies, CLI development with getopt, and toolchain configuration with sanitizers."
---

Modern C (C11/C17/C23) patterns for memory management, toolchain configuration, CLI tool development, and safe coding practices.

## C Language Standards

### C11 (ISO/IEC 9899:2011)

Major modernization: `_Generic` for type-generic selection, `_Atomic` for lock-free operations, `_Thread_local`, `_Static_assert`, `_Alignas`/`_Alignof`, `_Noreturn`, anonymous structs/unions.

### C17 (ISO/IEC 9899:2018)

Bug-fix release: `__STDC_VERSION__` updated to 201710L, defect reports resolved, `ATOMIC_VAR_INIT` deprecated.

### C23 (ISO/IEC 9899:2024)

Significant modernization: `nullptr` with `nullptr_t`, `typeof`, `constexpr`, `auto` type inference, binary literals (`0b`), digit separators, attributes (`[[nodiscard]]`, `[[maybe_unused]]`, `[[deprecated]]`), `bool`/`true`/`false` as keywords, `#embed` directive.

## Type System

The agent should use fixed-width integer types from `stdint.h`:

```c
#include <stdint.h>
#include <stddef.h>

// Fixed-width: int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t, uint32_t, uint64_t
// Pointer-sized: intptr_t, uintptr_t
// Size types: size_t, ptrdiff_t
uint32_t compute_hash(const uint8_t *data, size_t len);
```

Use `_Generic` for type-safe generic interfaces and designated initializers for struct clarity:

```c
struct config cfg = { .name = "myapp", .timeout = 30, .verbose = true };
```

## Memory Management

### Allocation Strategy Decision

- **Short-lived, function-scoped** -- stack allocation or VLA (with size limit)
- **Long-lived, single owner** -- `malloc`/`free` with clear ownership
- **Many small allocations freed together** -- arena allocator
- **Fixed-size objects** -- pool allocator

### Safe Allocation Pattern

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

**Critical rules:** Always check `malloc`/`calloc`/`realloc` return for NULL. Use `calloc` for zero-initialized memory. Set pointer to NULL after `free`.

### goto Cleanup Pattern

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

## CLI Development

### Argument Parsing

- **Simple flags/options** -- use `getopt()`
- **Long options needed** -- use `getopt_long()`
- **GNU-style with help generation** -- use `argp`

```c
#include <getopt.h>

static struct option long_options[] = {
    {"verbose", no_argument, NULL, 'v'},
    {"output",  required_argument, NULL, 'o'},
    {"help",    no_argument, NULL, 'h'},
    {NULL, 0, NULL, 0}
};

int main(int argc, char *argv[]) {
    int opt;
    while ((opt = getopt_long(argc, argv, "vo:h", long_options, NULL)) != -1) {
        switch (opt) {
        case 'v': verbose = 1; break;
        case 'o': output = optarg; break;
        case 'h': printf("Usage: %s [--verbose] [--output FILE]\n", argv[0]); return 0;
        default: return 1;
        }
    }
    return 0;
}
```

### Signal Handling

Use `sigaction()` over `signal()`. Keep handlers minimal (set flag only). Use `atomic_int` for signal flags.

### Exit Codes

Use `sysexits.h`: `EXIT_SUCCESS` (0), `EXIT_FAILURE` (1), `EX_USAGE` (64), `EX_NOINPUT` (66). Signals: 128+N.

## Concurrency (C11)

Use `_Atomic` for simple counters, pthreads for complex synchronization:

```c
#include <stdatomic.h>
_Atomic int counter = 0;
void increment(void) { atomic_fetch_add(&counter, 1); }
```

## Toolchain

### Compiler Flags

The agent should always compile with comprehensive warnings:

```bash
gcc -std=c11 -Wall -Wextra -Wpedantic -Werror -Wshadow -Wconversion -fanalyzer -o myapp myapp.c
```

### Sanitizers

```bash
# AddressSanitizer: buffer overflow, use-after-free
gcc -fsanitize=address -fno-omit-frame-pointer -g -o myapp myapp.c

# UndefinedBehaviorSanitizer
gcc -fsanitize=undefined -g -o myapp myapp.c

# Valgrind for release validation
valgrind --leak-check=full --show-leak-kinds=all ./myapp
```

### Static Analysis

- `clang-tidy src/*.c -- -std=c11` -- lint and static analysis
- `cppcheck --enable=all --error-exitcode=1 src/` -- additional static analysis

## Build Systems

- **Simple project, Unix-only** -- Make
- **Cross-platform, dependencies** -- CMake or Meson

```makefile
CC := gcc
CFLAGS := -std=c11 -Wall -Wextra -Wpedantic -g
SRCS := $(wildcard src/*.c)
OBJS := $(SRCS:.c=.o)
TARGET := myapp

all: $(TARGET)
$(TARGET): $(OBJS)
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS)
%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<
clean:
	rm -f $(OBJS) $(TARGET)
```

## Anti-Patterns to Avoid

- **void pointer abuse** -- use `_Generic` macros or code generation instead
- **Unbounded string functions** -- use `snprintf`, never `sprintf`, `strcpy`, or `gets()`
- **Unchecked malloc** -- always check for NULL
- **Format string vulnerabilities** -- always use `printf("%s", user_input)`
- **Magic numbers** -- use named constants with `#define` or `enum`

## Workflow

1. **Analyze**: Check existing code patterns, identify memory ownership, review headers
2. **Implement**: Use C11 minimum, follow memory management patterns, handle all errors
3. **Validate**: Compile with warnings, run sanitizers, test with Valgrind

## Best Practices

- Always check return values of `malloc`/`calloc`/`realloc`
- Enable `-Wall -Wextra -Werror` for all builds
- Run AddressSanitizer during development, Valgrind before release
- Use fixed-width integer types from `stdint.h`
- Prefer `snprintf` over `sprintf`, use designated initializers
- Document ownership semantics in function comments
- Use `static` for file-local functions, prefer `const` for read-only parameters

## Undefined Behavior

Common UB categories: memory (null deref, use-after-free, buffer overflow, double-free), arithmetic (signed overflow, division by zero), aliasing (strict aliasing violations), sequence points. Mitigate with ASan and UBSan during development.
