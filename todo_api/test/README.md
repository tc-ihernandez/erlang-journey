# Tests

This directory is reserved for future unit and integration tests.

## Planned Test Structure

```
test/
├── todo_db_tests.erl       # Unit tests for database module
├── todo_handler_tests.erl  # Unit tests for HTTP handler
├── todo_api_tests.erl      # Integration tests
└── README.md               # This file
```

## Testing Framework

Erlang provides several testing frameworks:
- **EUnit** - Built-in unit testing framework
- **Common Test** - Comprehensive testing framework
- **PropEr** - Property-based testing

## Current Testing

For now, automated API testing is done via the shell script:
```bash
./scripts/test_api.sh
```

See [TESTING_GUIDE.md](../docs/TESTING_GUIDE.md) for details.

## Future Improvements

- [ ] Add EUnit tests for each module
- [ ] Add Common Test suites for integration testing
- [ ] Add property-based tests with PropEr
- [ ] Set up CI/CD pipeline
- [ ] Add coverage reporting

## Running Future Tests

When tests are added, run them with:
```bash
rebar3 eunit
rebar3 ct
```

## Contributing Tests

When adding tests:
1. Follow Erlang testing conventions
2. Name test files with `_tests.erl` suffix
3. Include both positive and negative test cases
4. Document test purposes with comments
5. Ensure tests are isolated and repeatable

---

For manual testing, see the [Testing Guide](../docs/TESTING_GUIDE.md).

