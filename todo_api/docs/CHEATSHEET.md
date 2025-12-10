# TODO API - Cheat Sheet

Quick reference for common operations and commands.

## Starting the Application

```bash
# Navigate to project
cd todo_api

# Start with rebar3 (recommended)
rebar3 shell

# Or use the start script
./start.sh
```

## Quick Test Commands

```bash
# Health check
curl http://localhost:8000/health

# Create TODO
curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{"title":"My task","description":"Task details"}'

# List all
curl http://localhost:8000/todos

# Get one (replace 1 with actual ID)
curl http://localhost:8000/todos/1

# Update (replace 1 with actual ID)
curl -X PUT http://localhost:8000/todos/1 \
  -H "Content-Type: application/json" \
  -d '{"completed":true}'

# Delete (replace 1 with actual ID)
curl -X DELETE http://localhost:8000/todos/1
```

## Erlang Shell Commands

```erlang
% Compile and reload a module
c(todo_handler).

% Recompile all modules
make:all([load]).

% Check Mnesia info
mnesia:info().

% List all tables
mnesia:system_info(tables).

% Read all todos directly from Mnesia
mnesia:transaction(fun() -> mnesia:match_object(todo, {todo, '_', '_', '_', '_', '_'}, read) end).

% Stop the application
application:stop(todo_api).

% Start the application
application:start(todo_api).

% Quit Erlang shell
q().
```

## API Endpoints Summary

| Method | Endpoint       | Body Example                                      | Response Code |
|--------|----------------|---------------------------------------------------|---------------|
| GET    | /health        | -                                                 | 200           |
| GET    | /todos         | -                                                 | 200           |
| GET    | /todos/:id     | -                                                 | 200, 404      |
| POST   | /todos         | `{"title":"Task","description":"Details"}`        | 201, 400      |
| PUT    | /todos/:id     | `{"title":"New","completed":true}`                | 200, 404, 400 |
| DELETE | /todos/:id     | -                                                 | 204, 404      |

## JSON Request Format

### Create TODO
```json
{
  "title": "Required field",
  "description": "Optional field"
}
```

### Update TODO
```json
{
  "title": "Optional - new title",
  "description": "Optional - new description",
  "completed": true
}
```

## Response Format

### Success Response
```json
{
  "id": 1,
  "title": "Task title",
  "description": "Task description",
  "completed": false,
  "created_at": 1702234567
}
```

### Error Response
```json
{
  "error": "Error message"
}
```

### List Response
```json
{
  "todos": [
    {
      "id": 1,
      "title": "Task 1",
      "description": "Description",
      "completed": false,
      "created_at": 1702234567
    }
  ]
}
```

## Common Issues & Solutions

### Port 8080 in use
Edit `src/todo_api_sup.erl`, line with `{port, 8080}`, change to another port.

### Mnesia errors
```bash
# Stop app and clean database
rm -rf Mnesia.*
rebar3 shell
```

### Dependencies not found
```bash
rebar3 get-deps
rebar3 compile
```

### Module not found after changes
```erlang
% In Erlang shell
c(module_name).
```

## Development Workflow

1. **Make changes** to `.erl` files
2. **Reload module** in shell: `c(module_name).`
3. **Test** with curl commands
4. **Repeat**

Or restart the whole app:
```erlang
application:stop(todo_api).
application:start(todo_api).
```

## Testing

```bash
# Run full test suite
./test_api.sh

# Test with jq for pretty output
curl http://localhost:8000/todos | jq

# Verbose curl (see headers)
curl -v http://localhost:8000/todos

# Save response to file
curl http://localhost:8000/todos > response.json
```

## File Structure Quick Reference

```
src/
├── todo_api.app.src      # App config (dependencies, metadata)
├── todo_api_app.erl      # Application behavior (start/stop)
├── todo_api_sup.erl      # Supervisor (process management)
├── todo_db.erl           # Database layer (Mnesia CRUD)
└── todo_handler.erl      # HTTP handler (routing, JSON)
```

## Useful curl Options

```bash
-X METHOD              # HTTP method (GET, POST, PUT, DELETE)
-H "Header: Value"     # Add header
-d 'data'              # Request body
-v                     # Verbose output
-s                     # Silent mode
-w "\n%{http_code}"    # Show HTTP status code
| jq                   # Pretty print JSON
```

## Module Functions Quick Reference

### todo_db
```erlang
todo_db:create(#{<<"title">> => <<"Task">>}).
todo_db:read(1).
todo_db:read_all().
todo_db:update(1, #{<<"completed">> => true}).
todo_db:delete(1).
```

### Application Control
```erlang
application:start(todo_api).
application:stop(todo_api).
application:which_applications().
```

## Debugging Tips

```erlang
% Enable more verbose output
application:set_env(todo_api, log_level, debug).

% Check if process is running
whereis(todo_db).

% Check supervisor children
supervisor:which_children(todo_api_sup).

% Process info
process_info(whereis(todo_db)).
```

## Performance Testing

```bash
# Apache Bench (if installed)
ab -n 1000 -c 10 http://localhost:8000/todos

# Simple loop test
for i in {1..100}; do
  curl -s http://localhost:8000/health > /dev/null
  echo "Request $i completed"
done
```

## Next Steps

- Add authentication
- Implement pagination
- Add filtering/sorting
- Create unit tests
- Add more fields (due_date, priority)
- Implement search
- Add tags/categories

## Resources

- [Elli Documentation](https://github.com/elli-lib/elli)
- [Mnesia Documentation](https://www.erlang.org/doc/man/mnesia.html)
- [jsone Documentation](https://github.com/sile/jsone)
- [Learn You Some Erlang](https://learnyousomeerlang.com/)

---

**Pro Tip:** Keep this cheat sheet open while developing! 📝

