# API Reference

Complete reference for all TODO API endpoints with request and response examples.

## Base URL

```
http://localhost:8000
```

## Response Format

All responses are in JSON format with appropriate HTTP status codes.

## Endpoints

- [Health Check](#health-check)
- [Create TODO](#create-todo)
- [Get All TODOs](#get-all-todos)
- [Get TODO by ID](#get-todo-by-id)
- [Update TODO](#update-todo)
- [Delete TODO](#delete-todo)

---

## Health Check

Check if the API is running.

### Request

```http
GET /health
```

### Response

**Status:** `200 OK`

**Body:**
```json
{
  "status": "ok"
}
```

**Example:**
```bash
curl http://localhost:8000/health
```

---

## Create TODO

Create a new TODO item.

### Request

```http
POST /todos
Content-Type: application/json
```

**Body:**
```json
{
  "title": "string (required)",
  "description": "string (optional)"
}
```

### Response

**Status:** `201 Created`

**Body:**
```json
{
  "id": 1,
  "title": "Learn Erlang",
  "description": "Master functional programming",
  "completed": false,
  "created_at": 1702234567,
  "updated_at": 1702234567
}
```

### Examples

**Minimal request:**
```bash
curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Learn Erlang"
  }'
```

**Full request:**
```bash
curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Learn Erlang",
    "description": "Master functional programming"
  }'
```

### Error Responses

**Missing title (400 Bad Request):**
```json
{
  "error": "Title is required"
}
```

**Empty title (400 Bad Request):**
```json
{
  "error": "Title cannot be empty"
}
```

**Invalid JSON (400 Bad Request):**
```json
{
  "error": "Invalid JSON"
}
```

---

## Get All TODOs

Retrieve all TODOs, optionally filtered by completion status.

### Request

```http
GET /todos
GET /todos?completed=true
GET /todos?completed=false
```

**Query Parameters:**
- `completed` (optional): Filter by completion status
  - `true` - Get only completed TODOs
  - `false` - Get only pending TODOs
  - Omit parameter - Get all TODOs

### Response

**Status:** `200 OK`

**Body:**
```json
{
  "todos": [
    {
      "id": 1,
      "title": "Learn Erlang",
      "description": "Master functional programming",
      "completed": false,
      "created_at": 1702234567,
      "updated_at": 1702234567
    },
    {
      "id": 2,
      "title": "Build API",
      "description": "Create REST API with Elli",
      "completed": true,
      "created_at": 1702234600,
      "updated_at": 1702234700
    }
  ]
}
```

### Examples

**Get all TODOs:**
```bash
curl http://localhost:8000/todos
```

**Get only completed TODOs:**
```bash
curl "http://localhost:8000/todos?completed=true"
```

**Response (completed only):**
```json
{
  "todos": [
    {
      "id": 2,
      "title": "Build API",
      "description": "Create REST API with Elli",
      "completed": true,
      "created_at": 1702234600,
      "updated_at": 1702234700
    }
  ]
}
```

**Get only pending TODOs:**
```bash
curl "http://localhost:8000/todos?completed=false"
```

**Response (pending only):**
```json
{
  "todos": [
    {
      "id": 1,
      "title": "Learn Erlang",
      "description": "Master functional programming",
      "completed": false,
      "created_at": 1702234567,
      "updated_at": 1702234567
    }
  ]
}
```

### Error Responses

**Invalid completed parameter (400 Bad Request):**
```json
{
  "error": "Invalid 'completed' parameter. Use 'true' or 'false'"
}
```

---

## Get TODO by ID

Retrieve a specific TODO by its ID.

### Request

```http
GET /todos/:id
```

**Path Parameters:**
- `id` (required): TODO identifier (integer)

### Response

**Status:** `200 OK`

**Body:**
```json
{
  "id": 1,
  "title": "Learn Erlang",
  "description": "Master functional programming",
  "completed": false,
  "created_at": 1702234567,
  "updated_at": 1702234567
}
```

### Example

```bash
curl http://localhost:8000/todos/1
```

### Error Responses

**TODO not found (404 Not Found):**
```json
{
  "error": "Todo not found"
}
```

**Invalid ID format (400 Bad Request):**
```json
{
  "error": "Invalid todo ID"
}
```

Example of invalid ID:
```bash
curl http://localhost:8000/todos/abc
# Returns 400 Bad Request
```

---

## Update TODO

Update an existing TODO. All fields are optional.

### Request

```http
PUT /todos/:id
Content-Type: application/json
```

**Path Parameters:**
- `id` (required): TODO identifier (integer)

**Body (all fields optional):**
```json
{
  "title": "string (optional)",
  "description": "string (optional)",
  "completed": boolean (optional)
}
```

### Response

**Status:** `200 OK`

**Body:**
```json
{
  "id": 1,
  "title": "Learn Erlang",
  "description": "Master functional programming and OTP",
  "completed": true,
  "created_at": 1702234567,
  "updated_at": 1702234900
}
```

Note: `updated_at` is automatically set to the current timestamp.

### Examples

**Mark as completed:**
```bash
curl -X PUT http://localhost:8000/todos/1 \
  -H "Content-Type: application/json" \
  -d '{
    "completed": true
  }'
```

**Update title:**
```bash
curl -X PUT http://localhost:8000/todos/1 \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Learn Erlang and OTP"
  }'
```

**Update multiple fields:**
```bash
curl -X PUT http://localhost:8000/todos/1 \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Learn Erlang and OTP",
    "description": "Master functional programming and OTP",
    "completed": true
  }'
```

**Response:**
```json
{
  "id": 1,
  "title": "Learn Erlang and OTP",
  "description": "Master functional programming and OTP",
  "completed": true,
  "created_at": 1702234567,
  "updated_at": 1702235000
}
```

### Error Responses

**TODO not found (404 Not Found):**
```json
{
  "error": "Todo not found"
}
```

**Invalid ID format (400 Bad Request):**
```json
{
  "error": "Invalid todo ID"
}
```

**Invalid JSON (400 Bad Request):**
```json
{
  "error": "Invalid JSON"
}
```

---

## Delete TODO

Delete a TODO by its ID.

### Request

```http
DELETE /todos/:id
```

**Path Parameters:**
- `id` (required): TODO identifier (integer)

### Response

**Status:** `204 No Content`

**Body:** Empty

### Example

```bash
curl -X DELETE http://localhost:8000/todos/1
```

**Success (no output):**
```bash
# HTTP 204 No Content
# Empty response body
```

### Error Responses

**TODO not found (404 Not Found):**
```json
{
  "error": "Todo not found"
}
```

**Invalid ID format (400 Bad Request):**
```json
{
  "error": "Invalid todo ID"
}
```

---

## CORS Support

All endpoints support CORS with the following headers:

```http
Access-Control-Allow-Origin: *
Access-Control-Allow-Methods: GET, POST, PUT, DELETE, OPTIONS
Access-Control-Allow-Headers: Content-Type
```

### OPTIONS Preflight

```http
OPTIONS /todos
OPTIONS /todos/:id
```

**Status:** `200 OK`

**Body:**
```json
{
  "message": "CORS preflight"
}
```

---

## Error Handling

### HTTP Status Codes

| Code | Description |
|------|-------------|
| 200 | OK - Request successful |
| 201 | Created - Resource created successfully |
| 204 | No Content - Request successful, no content to return |
| 400 | Bad Request - Invalid input or malformed request |
| 404 | Not Found - Resource not found |
| 500 | Internal Server Error - Server error occurred |

### Error Response Format

All errors return JSON with an `error` field:

```json
{
  "error": "Error message description"
}
```

### Common Errors

**Bad Request (400):**
- Missing required fields
- Invalid data format
- Invalid query parameters
- Malformed JSON

**Not Found (404):**
- TODO with specified ID doesn't exist
- Invalid route

**Internal Server Error (500):**
- Database errors
- Unexpected server errors

---

## Data Types

### TODO Object

```typescript
{
  id: number,              // Unique identifier (auto-generated)
  title: string,           // TODO title (required, non-empty)
  description: string,     // TODO description (optional)
  completed: boolean,      // Completion status (default: false)
  created_at: number,      // Unix timestamp (auto-generated)
  updated_at: number       // Unix timestamp (auto-updated)
}
```

### Timestamps

All timestamps are Unix timestamps (seconds since epoch).

**Example:**
```json
{
  "created_at": 1702234567,  // December 10, 2023
  "updated_at": 1702234900   // December 10, 2023 (5 minutes later)
}
```

---

## Testing Examples

### Complete Workflow

```bash
# 1. Health check
curl http://localhost:8000/health

# 2. Create first TODO
curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{"title":"Learn Erlang","description":"Study OTP"}'

# 3. Create second TODO
curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{"title":"Build API","description":"Create REST API"}'

# 4. Get all TODOs
curl http://localhost:8000/todos

# 5. Get specific TODO
curl http://localhost:8000/todos/1

# 6. Mark as completed
curl -X PUT http://localhost:8000/todos/1 \
  -H "Content-Type: application/json" \
  -d '{"completed":true}'

# 7. Get only completed
curl "http://localhost:8000/todos?completed=true"

# 8. Get only pending
curl "http://localhost:8000/todos?completed=false"

# 9. Delete TODO
curl -X DELETE http://localhost:8000/todos/2

# 10. Verify deletion
curl http://localhost:8000/todos
```

---

## Rate Limiting

Currently, there is no rate limiting implemented. This may be added in future versions.

## Authentication

Currently, the API is open and does not require authentication. This may be added in future versions.

---

## Version

API Version: 0.1.0

Last Updated: December 2024

