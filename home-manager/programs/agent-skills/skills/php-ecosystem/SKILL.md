---
name: php-ecosystem
description: "Use when working with 'PHP 8', 'composer', 'phpunit', 'pest', 'phpstan', 'psalm', 'PSR standards', 'php-cs-fixer', or modern PHP patterns. Provides comprehensive modern PHP (8.3+) ecosystem patterns including type system, testing, static analysis, and package development."
---

Comprehensive patterns for modern PHP (8.3+) language features, PSR standards, testing with PHPUnit/Pest, static analysis with PHPStan/Psalm, and framework-agnostic package development.

## Core Concepts

- **strict_types**: Enable `declare(strict_types=1)` in all PHP files for type safety
- **PSR standards**: Follow PSR-1, PSR-4, PER-CS for interoperability
- **Dependency injection**: Inject dependencies through constructor; avoid service locator pattern
- **Value objects**: Use `readonly` classes for immutable domain values (PHP 8.2+)
- **Property hooks**: Use property hooks for validation and computed properties (PHP 8.4+)

## PHP Version Features

### PHP 8.4 (2024-11)

Property hooks, asymmetric visibility (`public private(set)`), `#[Deprecated]` attribute, lazy objects, `array_find()`, `array_any()`, `array_all()`.

```php
class User
{
    public string $email {
        set {
            if (!filter_var($value, FILTER_VALIDATE_EMAIL)) {
                throw new InvalidArgumentException('Invalid email');
            }
            $this->email = strtolower($value);
        }
    }

    public string $fullName {
        get => $this->firstName . ' ' . $this->lastName;
    }

    public private(set) string $status = 'active';

    public function __construct(
        public string $firstName,
        public string $lastName,
    ) {}
}
```

### PHP 8.3 (2023-11)

Typed class constants, `json_validate()`, `#[Override]` attribute, granular DateTime exceptions.

### PHP 8.2 (2022-12)

Readonly classes, DNF types, `null`/`false`/`true` as standalone types.

### PHP 8.1 (2021-11)

Enums, readonly properties, fibers, intersection types, `never` return type, first-class callable syntax.

### PHP 8.0 (2020-11)

Named arguments, attributes, constructor property promotion, union types, match expression, nullsafe operator.

## Type System Patterns

### Enums (PHP 8.1+)

```php
enum Status: string
{
    case Draft = 'draft';
    case Published = 'published';
    case Archived = 'archived';

    public function label(): string
    {
        return match($this) {
            self::Draft => 'Draft',
            self::Published => 'Published',
            self::Archived => 'Archived',
        };
    }
}

$status = Status::from('published');
```

### Readonly Classes (PHP 8.2+)

```php
readonly class Money
{
    public function __construct(
        public int $amount,
        public string $currency,
    ) {
        if ($amount < 0) {
            throw new InvalidArgumentException('Amount cannot be negative');
        }
    }

    public function add(Money $other): self
    {
        if ($this->currency !== $other->currency) {
            throw new InvalidArgumentException('Currency mismatch');
        }
        return new self($this->amount + $other->amount, $this->currency);
    }
}
```

### Custom Attributes (PHP 8.0+)

```php
#[Attribute(Attribute::TARGET_METHOD)]
class Route
{
    public function __construct(
        public string $path,
        public string $method = 'GET',
    ) {}
}

class UserController
{
    #[Route('/users', 'GET')]
    public function index(): array { return []; }
}
```

## PSR Standards

| PSR | Title | Key Points |
|-----|-------|------------|
| PSR-1 | Basic Coding Standard | UTF-8, StudlyCaps classes, camelCase methods |
| PSR-4 | Autoloading | Map namespaces to directories in `composer.json` |
| PSR-12 | Extended Coding Style | 4 spaces, braces on next line, declare visibility |
| PSR-3 | Logger Interface | Use `LoggerInterface` for logging |
| PSR-7 | HTTP Messages | Immutable request/response objects |
| PSR-15 | HTTP Handlers | Middleware with `MiddlewareInterface` |

### PSR-4 Autoloading

```json
{
    "autoload": {
        "psr-4": {
            "App\\": "src/",
            "App\\Tests\\": "tests/"
        }
    }
}
```

## Design Patterns

### Repository Pattern

```php
interface UserRepositoryInterface
{
    public function find(UserId $id): ?User;
    public function findByEmail(Email $email): ?User;
    public function save(User $user): void;
}

class PdoUserRepository implements UserRepositoryInterface
{
    public function __construct(private PDO $pdo) {}

    public function find(UserId $id): ?User
    {
        $stmt = $this->pdo->prepare('SELECT * FROM users WHERE id = :id');
        $stmt->execute(['id' => $id->toString()]);
        $row = $stmt->fetch(PDO::FETCH_ASSOC);
        return $row ? $this->hydrate($row) : null;
    }
}
```

### Dependency Injection

```php
interface CacheInterface
{
    public function get(string $key): mixed;
    public function set(string $key, mixed $value, int $ttl = 3600): void;
}

class ProductService
{
    public function __construct(
        private ProductRepositoryInterface $repository,
        private CacheInterface $cache,
    ) {}
}
```

## Composer

### Package Structure

```
my-package/
├── src/
├── tests/
├── composer.json
├── phpunit.xml.dist
├── phpstan.neon
├── .php-cs-fixer.dist.php
└── README.md
```

### Scripts

```json
{
    "scripts": {
        "test": "phpunit --colors=always",
        "analyse": "phpstan analyse --memory-limit=512M",
        "cs-fix": "php-cs-fixer fix",
        "ci": ["@cs-check", "@analyse", "@test"]
    }
}
```

## Testing

### PHPUnit

```php
use PHPUnit\Framework\TestCase;
use PHPUnit\Framework\Attributes\Test;
use PHPUnit\Framework\Attributes\DataProvider;

class CalculatorTest extends TestCase
{
    #[Test]
    #[DataProvider('additionProvider')]
    public function itAddsNumbers(int $a, int $b, int $expected): void
    {
        $calculator = new Calculator();
        $this->assertSame($expected, $calculator->add($a, $b));
    }

    public static function additionProvider(): array
    {
        return [
            'positive' => [1, 2, 3],
            'negative' => [-1, -2, -3],
        ];
    }
}
```

### Pest

```php
test('it adds numbers', function () {
    expect((new Calculator())->add(2, 3))->toBe(5);
});

it('throws on division by zero', function () {
    (new Calculator())->divide(10, 0);
})->throws(DivisionByZeroError::class);
```

## Static Analysis

### PHPStan Configuration

```yaml
# phpstan.neon
parameters:
    level: 9
    paths: [src, tests]
    checkMissingIterableValueType: true
    checkGenericClassInNonGenericObjectType: true
```

Levels: 0 (basic) to 9 (strict no-mixed). Level 10 available in PHPStan 2.0+. Start at level 5-6 for existing projects, level 9+ for new projects.

### PHP CS Fixer

```php
return (new PhpCsFixer\Config())
    ->setRules([
        '@PER-CS' => true,
        '@PHP84Migration' => true,
        'strict_types' => true,
        'no_unused_imports' => true,
        'ordered_imports' => ['sort_algorithm' => 'alpha'],
    ])
    ->setFinder($finder)
    ->setRiskyAllowed(true);
```

## Database (PDO)

```php
$pdo = new PDO($dsn, $user, $pass, [
    PDO::ATTR_ERRMODE => PDO::ERRMODE_EXCEPTION,
    PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC,
    PDO::ATTR_EMULATE_PREPARES => false,
]);

// Always use prepared statements
$stmt = $pdo->prepare('SELECT * FROM users WHERE email = :email');
$stmt->execute(['email' => $email]);
```

## Workflow

1. **Analyze**: Check PHP version in `composer.json`, review existing type patterns, identify PSR standards
2. **Implement**: Add `declare(strict_types=1)`, define interfaces first, use constructor promotion, add return types
3. **Validate**: Run `phpstan analyse`, then `php-cs-fixer fix`, then `phpunit`

## Critical Rules

- The agent should enable `declare(strict_types=1)` in all PHP files
- The agent should use prepared statements for all database queries
- The agent should run PHPStan level 9+ before committing
- The agent should never suppress errors with the `@` operator
- The agent should follow PER-CS coding style

## Anti-Patterns

- **God class**: Split into focused single-responsibility classes
- **Service locator**: Use constructor injection for explicit dependencies
- **Array shapes everywhere**: Create value objects or DTOs with typed properties
- **SQL concatenation**: Always use prepared statements with parameters
- **Null returns for errors**: Throw exceptions or use Result type
