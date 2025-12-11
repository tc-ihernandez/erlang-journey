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

Honestly? Learning Erlang after years of JavaScript has been weird but cool. The biggest mindshift was pattern matching - instead of writing a million `if/else` statements, you just describe what your data looks like and Erlang figures it out. Also, everything is immutable by default, which felt limiting at first but actually prevents so many stupid bugs.

The "let it crash" philosophy is wild. In JavaScript I'm used to wrapping everything in try/catch, but in Erlang you literally let things fail and the supervisor just restarts them. It felt wrong but it works. And recursion instead of loops? My brain hurt for a while, not gonna lie.

The coolest part is concurrency - it's just built in. No promises, no async/await drama, just spawn a process and send messages. JavaScript feels like it's trying to do concurrency, but Erlang was literally made for it.

Overall, it's been a really good learning experience. Erlang is genuinely solid for what it does, and I can see why it's been around for so long. Definitely glad I took the time to learn it!

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
