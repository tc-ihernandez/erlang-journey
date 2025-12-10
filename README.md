# Erlang Journey

Just a JS dev trying to survive Erlang

## Projects

### 1. TODO API - CRUD Application

A fully functional REST API for managing TODO items, demonstrating core Erlang/OTP concepts.

**Location:** [`todo_api/`](todo_api/)

**Features:**
- RESTful CRUD operations (Create, Read, Update, Delete)
- Elli HTTP framework
- Mnesia database for persistence
- OTP Application and Supervisor design
- JSON request/response handling
- Comprehensive error handling

**Tech Stack:**
- Erlang/OTP
- Elli (HTTP server)
- Mnesia (database)
- jsone (JSON library)

**Quick Start:**
```bash
cd todo_api
rebar3 shell
```

See the [TODO API README](todo_api/README.md) for detailed documentation.

## Learning Goals

- ✅ Build a practical CRUD API
- ✅ Understand OTP principles (Application, Supervisor, gen_server)
- ✅ Work with Mnesia database
- ✅ Implement HTTP request handling
