# Quick Start Guide

## Prerequisites

1. **Install Erlang/OTP** (version 24+)
   - macOS: `brew install erlang`
   - Ubuntu/Debian: `sudo apt-get install erlang`
   - Windows: Download from [erlang.org](https://www.erlang.org/downloads)

2. **Install rebar3** (Erlang build tool)
   - macOS: `brew install rebar3`
   - Linux: Follow instructions at [rebar3.org](https://rebar3.org/docs/getting-started/)
   - Or download binary: `wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3`

## Running the Application

### Step 1: Compile and Start

```bash
cd todo_api
rebar3 shell
```

You should see:
```
=================================================
Starting TODO API...
=================================================

✓ TODO API started successfully
✓ HTTP Server listening on http://localhost:8000
✓ Mnesia database initialized

Available endpoints:
  GET    /health          - Health check
  GET    /todos           - List all todos
  GET    /todos/:id       - Get a specific todo
  POST   /todos           - Create a new todo
  PUT    /todos/:id       - Update a todo
  DELETE /todos/:id       - Delete a todo

=================================================
```

### Step 2: Test the API

Open a new terminal and try these commands:

#### 1. Health Check
```bash
curl http://localhost:8000/health
```

#### 2. Create TODOs
```bash
# First TODO
curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{"title":"Learn Erlang basics","description":"Study syntax and OTP"}'

# Second TODO
curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{"title":"Build a project","description":"Create a CRUD API"}'

# Third TODO
curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{"title":"Get feedback","description":"Share with community"}'
```

#### 3. List All TODOs
```bash
curl http://localhost:8000/todos | jq
```

#### 4. Get Specific TODO
```bash
curl http://localhost:8000/todos/1 | jq
```

#### 5. Update TODO (mark as completed)
```bash
curl -X PUT http://localhost:8000/todos/1 \
  -H "Content-Type: application/json" \
  -d '{"completed":true}' | jq
```

#### 6. Update TODO (change title)
```bash
curl -X PUT http://localhost:8000/todos/2 \
  -H "Content-Type: application/json" \
  -d '{"title":"Build an awesome project","completed":true}' | jq
```

#### 7. Delete TODO
```bash
curl -X DELETE http://localhost:8000/todos/3
```

#### 8. Verify deletion
```bash
curl http://localhost:8000/todos | jq
```

## Testing Script

Save this as `test_api.sh` and run it:

```bash
#!/bin/bash

BASE_URL="http://localhost:8000"

echo "=== Testing TODO API ==="
echo ""

echo "1. Health Check"
curl -s $BASE_URL/health | jq
echo ""

echo "2. Create TODO #1"
curl -s -X POST $BASE_URL/todos \
  -H "Content-Type: application/json" \
  -d '{"title":"Task 1","description":"First task"}' | jq
echo ""

echo "3. Create TODO #2"
curl -s -X POST $BASE_URL/todos \
  -H "Content-Type: application/json" \
  -d '{"title":"Task 2","description":"Second task"}' | jq
echo ""

echo "4. List all TODOs"
curl -s $BASE_URL/todos | jq
echo ""

echo "5. Get TODO #1"
curl -s $BASE_URL/todos/1 | jq
echo ""

echo "6. Update TODO #1"
curl -s -X PUT $BASE_URL/todos/1 \
  -H "Content-Type: application/json" \
  -d '{"completed":true}' | jq
echo ""

echo "7. Delete TODO #2"
curl -s -X DELETE $BASE_URL/todos/2
echo ""

echo "8. Final list"
curl -s $BASE_URL/todos | jq
echo ""

echo "=== Tests Complete ==="
```

Make it executable and run:
```bash
chmod +x test_api.sh
./test_api.sh
```

## Troubleshooting

### Port Already in Use
If port 8080 is busy, edit `src/todo_api_sup.erl` and change the port number:
```erlang
{port, 8080}  % Change to another port like 3000
```

### Mnesia Errors
If you see Mnesia errors, stop the application and clean the database:
```bash
rm -rf Mnesia.*
rebar3 shell
```

### Dependencies Not Found
If Elli or jsone are not found:
```bash
rebar3 get-deps
rebar3 compile
```

## Stopping the Application

In the Erlang shell:
- Press `Ctrl+C` twice, or
- Type `q().` and press Enter

## Next Steps

1. Read the [full README](README.md) for detailed documentation
2. Explore the source code in `src/`
3. Try modifying the API to add new features
4. Learn about Erlang's concurrency model

## Common Issues

**Q: "command not found: rebar3"**  
A: Install rebar3 using your package manager or download from rebar3.org

**Q: "command not found: erl"**  
A: Install Erlang/OTP from erlang.org

**Q: Can't connect to API**  
A: Make sure the application is running (`rebar3 shell`) and check the startup messages

**Q: JSON parsing errors**  
A: Ensure you're using proper JSON syntax and Content-Type header

## Pro Tips

- Use `jq` to format JSON responses: `curl ... | jq`
- Use `-v` flag with curl to see full HTTP details: `curl -v ...`
- Check Mnesia data: In Erlang shell, run `mnesia:info().`
- Hot reload code: In Erlang shell, run `c(module_name).`

Happy coding! 🚀

