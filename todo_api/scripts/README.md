# Scripts

Utility scripts for the TODO API project.

## Available Scripts

### 🚀 [start.sh](start.sh)
Convenience script to start the TODO API server.

**Usage:**
```bash
./scripts/start.sh
```

**What it does:**
- Checks if Erlang and rebar3 are installed
- Provides installation instructions if missing
- Starts the application with `rebar3 shell`

---

### 🧪 [test_api.sh](test_api.sh)
Automated test suite that runs comprehensive API tests.

**Usage:**
```bash
./scripts/test_api.sh
```

**What it tests:**
- Health check endpoint
- Creating TODOs (multiple tests)
- Listing all TODOs
- Getting specific TODOs
- Updating TODOs
- Deleting TODOs
- Error handling (404, 400)
- Validation logic

**Requirements:**
- API must be running on http://localhost:8000
- `curl` must be installed

**Output:**
- ✓ Green checkmarks for passing tests
- ✗ Red X marks for failing tests
- Detailed HTTP responses for debugging

---

### 📝 [start_server.erl](start_server.erl)
Escript for starting the server without rebar3 (advanced use).

**Usage:**
```bash
./scripts/start_server.erl
```

**Note:** Requires Erlang to be in PATH and dependencies to be compiled.

---

## Running Scripts

Make sure scripts are executable:
```bash
chmod +x scripts/*.sh
chmod +x scripts/*.erl
```

## Troubleshooting

**Permission denied:**
```bash
chmod +x scripts/test_api.sh
```

**Script not found:**
Make sure you're running from the project root:
```bash
cd todo_api
./scripts/test_api.sh
```

**API not running:**
Start the API first in another terminal:
```bash
rebar3 shell
```

Then run the test script in a new terminal.

---

For more information, see the [Testing Guide](../docs/TESTING_GUIDE.md).

