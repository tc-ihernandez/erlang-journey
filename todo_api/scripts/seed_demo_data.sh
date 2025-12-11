#!/bin/bash

# Demo Data Seeding Script for TODO API
# This script creates sample tasks to demonstrate all features

API_URL="http://localhost:8000"

echo "🌱 Seeding demo data for TODO API..."
echo "======================================"
echo ""

# Function to create a TODO
create_todo() {
    curl -s -X POST "$API_URL/todos" \
        -H "Content-Type: application/json" \
        -d "$1" > /dev/null
    echo "✓ Created: $(echo $1 | grep -o '"title":"[^"]*"' | cut -d'"' -f4)"
}

# 1. Urgent task with due date
create_todo '{
    "title": "Deploy Production Release v2.0",
    "description": "Deploy the new version to production servers before deadline",
    "priority": "urgent",
    "tags": ["devops", "production", "critical"],
    "due_date": "25/12/2025"
}'

# 2. High priority with tags
create_todo '{
    "title": "Fix Critical Security Vulnerability",
    "description": "Patch CVE-2024-1234 in authentication module",
    "priority": "high",
    "tags": ["security", "backend", "urgent"]
}'

# 3. Learning task
create_todo '{
    "title": "Complete Erlang OTP Course",
    "description": "Finish chapters 8-12 on supervision trees and gen_server patterns",
    "priority": "high",
    "tags": ["learning", "erlang", "otp"],
    "due_date": "31/12/2025"
}'

# 4. Meeting
create_todo '{
    "title": "Team Standup Meeting",
    "description": "Daily sync with the development team",
    "priority": "medium",
    "tags": ["meeting", "team"],
    "due_date": "11/12/2025 09:00:00"
}'

# 5. Documentation
create_todo '{
    "title": "Write API Documentation",
    "description": "Document all new endpoints and features for the REST API",
    "priority": "medium",
    "tags": ["documentation", "api", "technical-writing"]
}'

# 6. Code review
create_todo '{
    "title": "Review Pull Request #247",
    "description": "Code review for new authentication middleware implementation",
    "priority": "high",
    "tags": ["code-review", "backend", "security"],
    "due_date": "12/12/2025"
}'

# 7. Testing
create_todo '{
    "title": "Write Unit Tests for Payment Module",
    "description": "Achieve 90% code coverage for the payment processing system",
    "priority": "medium",
    "tags": ["testing", "quality-assurance", "backend"]
}'

# 8. Low priority task
create_todo '{
    "title": "Update Dependencies",
    "description": "Check and update all npm packages to latest stable versions",
    "priority": "low",
    "tags": ["maintenance", "dependencies"]
}'

# 9. Design task
create_todo '{
    "title": "Design New Landing Page",
    "description": "Create mockups for the new marketing landing page",
    "priority": "medium",
    "tags": ["design", "frontend", "ui-ux"],
    "due_date": "20/12/2025"
}'

# 10. Database optimization
create_todo '{
    "title": "Optimize Database Queries",
    "description": "Improve performance of slow queries in user dashboard",
    "priority": "high",
    "tags": ["database", "performance", "backend"]
}'

# 11. Bug fix
create_todo '{
    "title": "Fix Mobile Responsive Issues",
    "description": "Address layout problems on iOS Safari and Android Chrome",
    "priority": "high",
    "tags": ["bug", "frontend", "mobile"],
    "due_date": "15/12/2025"
}'

# 12. Content task
create_todo '{
    "title": "Prepare Conference Presentation",
    "description": "Create slides for ErlangFactory 2025 talk on distributed systems",
    "priority": "medium",
    "tags": ["conference", "presentation", "erlang"]
}'

# Additional tasks for more comprehensive demo
curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Refactor Authentication Module",
    "description": "Improve security and add JWT token refresh logic",
    "priority": "high",
    "due_date": "18/12/2025",
    "tags": ["refactoring", "security", "backend", "jwt", "authentication"]
  }'

curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Client Demo Presentation",
    "description": "Prepare slides and demo environment for client meeting",
    "priority": "urgent",
    "due_date": "14/12/2025 14:00:00",
    "tags": ["presentation", "client", "demo", "sales"]
  }'

curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Learn Pattern Matching in Erlang",
    "description": "Deep dive into Erlang pattern matching and guards",
    "priority": "medium",
    "due_date": "30/12/2025",
    "tags": ["learning", "erlang", "patterns", "functional-programming"]
  }'

curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Setup CI/CD Pipeline",
    "description": "Configure GitHub Actions for automated testing and deployment",
    "priority": "high",
    "due_date": "22/12/2025",
    "tags": ["devops", "ci-cd", "automation", "github-actions"]
  }'

curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Weekly Newsletter",
    "description": "Write and send team newsletter with updates",
    "priority": "low",
    "due_date": "13/12/2025",
    "tags": ["communication", "team", "newsletter"]
  }'

curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Implement WebSocket Real-time Updates",
    "description": "Add real-time notifications using WebSockets",
    "priority": "medium",
    "due_date": "28/12/2025",
    "tags": ["websockets", "real-time", "backend", "feature"]
  }'

curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Performance Monitoring Setup",
    "description": "Integrate monitoring tools and set up dashboards",
    "priority": "high",
    "due_date": "19/12/2025",
    "tags": ["monitoring", "performance", "observability", "devops"]
  }'

curl -X POST http://localhost:8000/todos \
  -H "Content-Type: application/json" \
  -d '{
    "title": "Lunch with Design Team",
    "description": "Discuss Q1 2025 design system updates",
    "priority": "low",
    "due_date": "16/12/2025 12:30:00",
    "tags": ["meeting", "design", "team", "social"]
  }'

echo ""
echo "======================================"
echo "✅ Demo data seeded successfully!"
echo ""
echo "📊 Created 20 sample tasks with:"
echo "   • All 4 priority levels"
echo "   • Various due dates"
echo "   • Multiple tags per task"
echo "   • Different categories (devops, learning, meetings, etc.)"
echo ""
echo "🌐 Open the frontend: public/index.html"
echo "📡 API running at: $API_URL"
echo ""
echo "🎬 Ready for your demo!"
echo "======================================"

