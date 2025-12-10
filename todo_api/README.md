# TODO API - Erlang CRUD Application

A simple REST API for managing TODO items, built with Erlang using the Elli HTTP framework and Mnesia database.

## Features

- **RESTful API** with full CRUD operations
- **Mnesia database** for persistent storage
- **OTP principles** (Application, Supervisor, gen_server)
- **JSON** request/response handling
- **Error handling** with appropriate HTTP status codes

## Prerequisites

- Erlang/OTP 24 or higher
- rebar3

## Installation

1. Navigate to the project directory:
```bash
cd todo_api
```

2. Get dependencies:
```bash
rebar3 get-deps
```

3. Compile the project:
```bash
rebar3 compile
```

## Running the Application

Start the application in development mode:

```bash
rebar3 shell
```

The API will start on `http://localhost:8000`

To stop the application, press `Ctrl+C` twice or type `q().` in the Erlang shell.

## API Endpoints

### Health Check

Check if the API is running:

```bash
curl http://localhost:8000/health
```

**Response:**
```json
{
  "status": "ok"
}
```

### Create a TODO

Create a new TODO item:

```bash
curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Learn Erlang",
    "description": "Complete the Erlang journey"
  }'
```

**Response (201 Created):**
```json
{
  "id": 1,
  "title": "Learn Erlang",
  "description": "Complete the Erlang journey",
  "completed": false,
  "created_at": 1702234567
}
```

### Get All TODOs

List all TODO items:

```bash
curl http://localhost:8000/todos
```

**Response (200 OK):**
```json
{
  "todos": [
    {
      "id": 1,
      "title": "Learn Erlang",
      "description": "Complete the Erlang journey",
      "completed": false,
      "created_at": 1702234567
    }
  ]
}
```

### Get a Specific TODO

Get a single TODO by ID:

```bash
curl http://localhost:8000/todos/1
```

**Response (200 OK):**
```json
{
  "id": 1,
  "title": "Learn Erlang",
  "description": "Complete the Erlang journey",
  "completed": false,
  "created_at": 1702234567
}
```

**Error (404 Not Found):**
```json
{
  "error": "Todo not found"
}
```

### Update a TODO

Update an existing TODO:

```bash
curl -X PUT http://localhost:8000/todos/1 \
  -H "Content-Type: application/json" \
  -d '{
    "completed": true
  }'
```

You can update any combination of fields:
- `title` (string)
- `description` (string)
- `completed` (boolean)

**Response (200 OK):**
```json
{
  "id": 1,
  "title": "Learn Erlang",
  "description": "Complete the Erlang journey",
  "completed": true,
  "created_at": 1702234567
}
```

### Delete a TODO

Delete a TODO by ID:

```bash
curl -X DELETE http://localhost:8000/todos/1
```

**Response:** 204 No Content (empty body)

**Error (404 Not Found):**
```json
{
  "error": "Todo not found"
}
```

## Project Structure

```
todo_api/
├── rebar.config          # Dependency management and configuration
├── src/
│   ├── todo_api.app.src  # Application metadata
│   ├── todo_api_app.erl  # Application behavior (startup/shutdown)
│   ├── todo_api_sup.erl  # Supervisor (manages child processes)
│   ├── todo_db.erl       # Database layer (Mnesia CRUD operations)
│   └── todo_handler.erl  # HTTP handler (routing and request handling)
└── README.md
```

## Architecture

The application follows OTP design principles:

1. **Application (`todo_api_app`)**: Entry point, starts the supervisor
2. **Supervisor (`todo_api_sup`)**: Manages child processes with `one_for_one` strategy
3. **Database Server (`todo_db`)**: gen_server that handles Mnesia operations
4. **HTTP Handler (`todo_handler`)**: Elli callback module for request routing

### Data Flow

```
HTTP Client → Elli Server → todo_handler → todo_db → Mnesia
                                ↓
                            JSON Response
```

## Database

The application uses **Mnesia**, Erlang's distributed database system. Data is stored on disk and persists between application restarts.

### Schema

Each TODO has the following fields:

- `id` - Unique integer identifier (auto-generated)
- `title` - String (required)
- `description` - String (optional)
- `completed` - Boolean (default: false)
- `created_at` - Unix timestamp (auto-generated)

## Error Handling

The API returns appropriate HTTP status codes:

- `200 OK` - Successful GET/PUT request
- `201 Created` - Successful POST request
- `204 No Content` - Successful DELETE request
- `400 Bad Request` - Invalid input (missing title, invalid JSON, etc.)
- `404 Not Found` - Resource not found
- `500 Internal Server Error` - Server error

## Testing Examples

Here's a complete workflow to test all endpoints:

```bash
# 1. Check health
curl http://localhost:8000/health

# 2. Create first TODO
curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{"title":"Buy groceries","description":"Milk, eggs, bread"}'

# 3. Create second TODO
curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{"title":"Exercise","description":"30 minutes cardio"}'

# 4. List all TODOs
curl http://localhost:8000/todos

# 5. Get specific TODO (use ID from create response)
curl http://localhost:8000/todos/1

# 6. Update TODO to mark as completed
curl -X PUT http://localhost:8000/todos/1 \
  -H "Content-Type: application/json" \
  -d '{"completed":true}'

# 7. Delete TODO
curl -X DELETE http://localhost:8000/todos/2

# 8. Verify deletion
curl http://localhost:8000/todos
```

## Development

### Compile and Run

```bash
# Compile only
rebar3 compile

# Run with interactive shell
rebar3 shell

# Clean build artifacts
rebar3 clean
```

### Hot Code Reloading

While the application is running in `rebar3 shell`, you can reload modules:

```erlang
% Recompile all modules
make:all([load]).

% Reload specific module
c(todo_handler).
```

## Dependencies

- **Elli** (3.3.0) - Fast and lightweight HTTP server
- **jsone** (1.8.1) - JSON encoding/decoding library

## Technical Highlights

This project demonstrates:

- ✓ OTP Application structure
- ✓ Supervisor design pattern
- ✓ gen_server behavior
- ✓ Mnesia database operations
- ✓ HTTP request handling with Elli
- ✓ JSON serialization/deserialization
- ✓ Pattern matching and guards
- ✓ Error handling
- ✓ RESTful API design

## License

Apache 2.0

## Author

Built as part of the Erlang learning journey.

