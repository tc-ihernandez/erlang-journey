# Testing Guide - TODO API

## 🚀 Installation and Testing

### Step 1: Install Erlang and rebar3

```bash
# Install Erlang/OTP and rebar3
brew install erlang rebar3

# Verify installation
erl -version
rebar3 --version
```

### Step 2: Start the application

```bash
# Navigate to project directory
cd /Users/ivettehernandez/Documents/GitHub/erlang-journey/todo_api

# Compile and run
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
```

### Step 3: Test the API (in another terminal)

#### Option A: Automated Test 🎯 (Recommended)

```bash
# Open a new terminal and run:
cd /Users/ivettehernandez/Documents/GitHub/erlang-journey/todo_api
./test_api.sh
```

This script runs 12 automated tests and shows results with ✓ or ✗.

#### Option B: Manual Tests with curl 📝

Open a **new terminal** and run these commands:

**1. Health Check (verify it works)**
```bash
curl http://localhost:8000/health
```
Should respond: `{"status":"ok"}`

**2. Create your first TODO**
```bash
curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{"title":"Learn Erlang","description":"Complete TODO API project"}'
```

Expected response:
```json
{
  "id": 1,
  "title": "Learn Erlang",
  "description": "Complete TODO API project",
  "completed": false,
  "created_at": 1702234567
}
```

**3. Create more TODOs**
```bash
# TODO 2
curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{"title":"Get feedback","description":"Share project"}'

# TODO 3
curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{"title":"Celebrate","description":"Project working!"}'
```

**4. View all TODOs**
```bash
curl http://localhost:8000/todos
```

If you have `jq` installed (for pretty JSON):
```bash
curl http://localhost:8000/todos | jq
```

**5. View a specific TODO**
```bash
curl http://localhost:8000/todos/1
```

**6. Update a TODO (mark as completed)**
```bash
curl -X PUT http://localhost:8000/todos/1 \
  -H "Content-Type: application/json" \
  -d '{"completed":true}'
```

**7. Update title and description**
```bash
curl -X PUT http://localhost:8000/todos/2 \
  -H "Content-Type: application/json" \
  -d '{"title":"New title","description":"New description"}'
```

**8. Delete a TODO**
```bash
curl -X DELETE http://localhost:8000/todos/3
```

**9. Verify deletion**
```bash
curl http://localhost:8000/todos/3
```
Should respond: `{"error":"Todo not found"}` with status code 404

**10. View final list**
```bash
curl http://localhost:8000/todos | jq
```

### Step 4: Explore with Postman or Insomnia (Optional)

If you prefer a graphical interface:

1. Open Postman or Insomnia
2. Import this collection:

**Base URL:** `http://localhost:8000`

**Endpoints:**
- GET `/health`
- GET `/todos`
- GET `/todos/1`
- POST `/todos` (Body: `{"title":"Test","description":"Description"}`)
- PUT `/todos/1` (Body: `{"completed":true}`)
- DELETE `/todos/1`

### 🎬 Command Sequence Demo

```bash
# Run this to see the complete flow:

echo "=== Starting tests ==="

echo -e "\n1. Health Check:"
curl -s http://localhost:8000/health && echo ""

echo -e "\n2. Create TODO:"
curl -s -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{"title":"My first task","description":"Learn Erlang"}' && echo ""

echo -e "\n3. List TODOs:"
curl -s http://localhost:8000/todos && echo ""

echo -e "\n4. Mark as completed:"
curl -s -X PUT http://localhost:8000/todos/1 \
  -H "Content-Type: application/json" \
  -d '{"completed":true}' && echo ""

echo -e "\n=== Tests completed ==="
```

### 🐛 Troubleshooting

**Issue: Port 8000 already in use**
```bash
# See what's using the port
lsof -i :8000

# Change port in src/todo_api_sup.erl (line with {port, 8000})
# Or kill the process:
kill -9 <PID>
```

**Issue: "Connection refused"**
- Make sure the app is running with `rebar3 shell`
- Verify you see the message "HTTP Server listening on http://localhost:8000"

**Issue: Compilation errors**
```bash
cd /Users/ivettehernandez/Documents/GitHub/erlang-journey/todo_api
rm -rf _build
rebar3 clean
rebar3 compile
```

**Issue: Mnesia errors**
```bash
# Stop the app (Ctrl+C twice in the Erlang shell)
# Clean database
rm -rf Mnesia.*
# Restart
rebar3 shell
```

### 📊 Verify in Erlang Shell

While the app is running, in the Erlang shell you can:

```erlang
% View Mnesia info
mnesia:info().

% View running applications
application:which_applications().

% View supervised processes
supervisor:which_children(todo_api_sup).

% Read all TODOs directly from Mnesia
mnesia:transaction(fun() -> 
    mnesia:match_object(todo, {todo, '_', '_', '_', '_', '_'}, read) 
end).
```

### ✅ Testing Checklist

- [ ] Erlang and rebar3 installed
- [ ] Application compiled without errors
- [ ] Server started on port 8000
- [ ] Health check responds OK
- [ ] Can create TODOs
- [ ] Can list TODOs
- [ ] Can get a specific TODO
- [ ] Can update TODOs
- [ ] Can delete TODOs
- [ ] Errors return appropriate status codes (404, 400)

### 🎉 Success!

If all tests pass, your API is working perfectly. 

**Next steps:**
1. Experiment creating your own TODOs
2. Try to break the API (validation, errors)
3. Read the code in `src/` to understand how it works
4. Modify something and reload with `c(module_name).` in the shell

Enjoy your Erlang API! 🚀

