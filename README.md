# 🚀 Erlang Journey

Just a JS dev trying to survive Erlang 😅

## 📋 TODO API - Full-Stack CRUD Application

A comprehensive REST API built with Erlang/OTP, featuring a modern Material Design frontend and complete API documentation.

**📂 Project Location:** [`todo_api/`](todo_api/)

---

## ✨ Key Features

### Backend (Erlang/OTP)
- ✅ **RESTful CRUD Operations** - Complete Create, Read, Update, Delete
- ✅ **Advanced Filtering** - Search, pagination, sorting, tags
- ✅ **Rate Limiting** - Protect API from abuse
- ✅ **Statistics Dashboard** - Real-time analytics
- ✅ **Audit Log** - Track all changes and history
- ✅ **Priority & Due Dates** - Task management features
- ✅ **Soft Delete** - Safe deletion with restore capability
- ✅ **Bulk Operations** - Complete or delete multiple tasks at once

### Tech Stack
- **Backend:** Erlang/OTP 26, Elli HTTP framework, Mnesia database
- **Frontend:** Vanilla JavaScript, Material Design, Poppins font
- **Tools:** Postman Collection, EUnit tests, Shell scripts

---

## 🎥 Demo Videos

### Postman API Testing
> **Coming soon:** Video demonstration of all API endpoints using Postman

<!-- 
Add your Postman demo video here:
[![Postman Demo](thumbnail.png)](your-video-link)
-->

### Frontend Demo
> **Coming soon:** Video walkthrough of the Material Design frontend

<!-- 
Add your frontend demo video here:
[![Frontend Demo](thumbnail.png)](your-video-link)
-->

---

## 📚 Documentation

For complete API documentation, endpoints reference, and setup instructions:

👉 **[View Full Documentation](todo_api/README.md)**

**Quick Links:**
- [API Reference](todo_api/docs/API_REFERENCE.md) - All endpoints with examples
- [Quick Start Guide](todo_api/docs/QUICKSTART.md) - Get running in 5 minutes
- [Testing Guide](todo_api/docs/TESTING_GUIDE.md) - How to test the API
- [Postman Collection](todo_api/TODO_API.postman_collection.json) - Ready to import

---

## 🤔 JavaScript vs Erlang: My Experience
Honestly? Picking up Erlang after years of JavaScript has been one of those unexpected but very welcome perspective shifts in my career.

The first big change was pattern matching. Instead of navigating endless if/else branches, you just describe the shape of the data and let the language route the logic. It keeps your code focused, declarative, and way easier to reason about — something I didn’t realize I’d been missing until I used it.

Immutability across the board also surprised me. At first it felt limiting, but it quickly became a feature I appreciated. It forces cleaner thinking, eliminates entire categories of side-effect bugs, and makes debugging feel like less of a detective novel and more of a checklist.

And then there’s the classic “let it crash” mindset. Coming from JS, where you tend to bubble-wrap everything in try/catch, Erlang’s supervision model felt almost rebellious. But once you see the resilience you get out of it, it clicks — the system becomes self-healing instead of you micromanaging every failure.

Recursion as the primary iteration tool definitely pushed me out of my comfort zone. Tail-call optimization almost felt like a cheat code once I wrapped my head around it.

But concurrency is where Erlang really shines. After working with it, you understand why Erlang systems run telecom infrastructure and still feel timeless.

Overall, learning Erlang expanded the way I think about design, reliability, and distributed systems. It’s one of those languages that doesn’t just add a tool to your stack — it shifts your mental model. And honestly, that’s been the best part.

---

## 🚀 Quick Start

```bash
# Clone the repository
git clone https://github.com/yourusername/erlang-journey.git
cd erlang-journey/todo_api

# Compile and run
rebar3 compile
erl -sname todo_api -pa _build/default/lib/*/ebin \
    -eval "application:ensure_all_started(todo_api)" -noshell

# In another terminal, seed demo data
./scripts/seed_demo_data.sh

# Open the frontend
open public/index.html
```

**Server runs on:** `http://localhost:8000`

---

## 📊 Project Stats

- **Lines of Erlang Code:** ~2,000+
- **API Endpoints:** 21
- **Tests:** EUnit suite included
- **Demo Tasks:** 20 sample records
- **Documentation Pages:** 6

---

## 🎯 Learning Goals

- ✅ Build a practical CRUD API with Erlang
- ✅ Understand OTP principles (Application, Supervisor, gen_server)
- ✅ Work with Mnesia database
- ✅ Implement HTTP request handling with Elli
- ✅ Master pattern matching and recursion
- ✅ Build concurrent, fault-tolerant systems
- ✅ Create a modern frontend to consume the API
- ✅ Apply Material Design principles
- ✅ Write comprehensive documentation

---

## 📝 License

MIT License - Feel free to use this for learning!

---

## 🙏 Acknowledgments

Built while learning Erlang, inspired by the need for a simple yet powerful TODO API that showcases the best of both Erlang backend and modern frontend design.

**Special thanks to:**
- The Erlang/OTP team for an amazing language
- Elli framework for simplicity
- Material Design for UI inspiration
- TigerConnect for color palette inspiration

---

**Happy Coding! 🎉**

*If you're also learning Erlang, feel free to use this as a reference. We're all in this together!* 💪
